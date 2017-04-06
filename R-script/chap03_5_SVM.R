# chap03_5_SVM





##################################################
#Support Vector Machine 
##################################################
# SVM 알고리즘 - 두 범주를 직선으로 분류(이진분류) 
# 선형분리 - 2개의 집합을 직선으로 분리(초평면 분리) 
# 초평면(Separating Hyperplane) : 2차원 이상 공간에서 평면 
# 가상의 직선을 중심으로 거리를 계산하여 직사각형 형태로 영역 확장
# -> 가장 가까운 점이 만날때 까지 영역  확장 

# 바이오인포매틱스의 유전자 데이터 분류 
# 용도 : 인간의 얼굴, 문자, 숫자 인식(이미지 데이터 패턴 인식) 
# 예) 스캐너로 스캔된 문서 이미지를 문자로 인식 


###############################################
####### e1071 패키지 
###############################################
# 관련 패키지(e1071, kernlab, klaR 등) 4개 중 e1071 가장 많이 사용함 

    library(e1071)  
    
    # 1. SVM 기본 개념 익히기 - Support Vector, Margin
        df = data.frame(
            x1 = c(1,2,1,2,4,5,6),
            x2 = c(8,7,5,6,1,3,2),
            y=factor(c(1,1,1,1,0,0,0))
        )
        
        
        
    # 2. svm 모델 생성 
        
        # 형식) svm(y ~ x, data, type(method), kernel) : method, kernel 속성 생략 가능
        model_svm = svm(y ~ ., data = df)
        # default 속성 : , type = "C-classification", kernel="radial"
        # type : 분리 방식 
        # kernel : 비선형(non linear) 관계를 선형적(linear)으로 변환하는 역할 
        # kernel 종류 : linear, polynomial, radial, sigmoid
        
        model_svm
        summary(model_svm)
        names(model_svm)
        model_svm$fitted
        
        # svm 모델 시각화 
        par(mfrow=c(1,1))
        plot(df$x1, df$x2, col=df$y)  
        X11()
        plot(model_svm, df) # 분류 Factor levels에 의해서 2개 분류 
        
    
    # 3. kernel="linear" 변경 
        model_svm2 = svm(y~., data=df, method = "C-classification", kernel="linear")
        model_svm2

        plot(model_svm2, df)
        
        
        
        
############################
# iris 데이터 실습 
############################

# 1. 데이터셋 생성 
    data(iris)
    set.seed(415) # random 결과를 동일하게 지
    idx = sample(1:nrow(iris), 0.7*nrow(iris))
    training = iris[idx, ]
    testing = iris[-idx, ]
    training
    testing
    dim(training) # 105
    dim(testing) # 45


# 2. 분류모델 생성 
    model_svm = svm(Species ~ ., data = training, na.action =na.omit)
    summary(model_svm)


# 3. 분류모델 성능 평가(testing set 적용 예측값 생성)  
    pred <- predict(model_svm, testing)
    
    # 혼돈 matrix 작성 
    table(pred, testing$Species)

    # 분류정확도
    # pred         setosa versicolor virginica
    #   setosa         14          0         0
    #   versicolor      0         14         1
    #   virginica       0          3        13
    
    
    confusionMatrix(pred, testing$Species)
    
    
    
    
    
    
    
    
##################################################
#Support Vector Machine 문제 : spamfiltering
##################################################
    
    # 단계1. 실습 데이터 가져오기
        load(file.choose()) # sms_data_total.RData
        ls() 
    
    # 단계2. 데이터 탐색 
        dim(train_sms) # train 데이터 
        dim(test_sms) # test 데이터 
        names(train_sms)
        table(train_sms$type) # sms 메시지 유형 
        table(test_sms$type)
    
        
    # 단계3. 분류모델 생성 : 기본 파라미터 사용 
    
        model_sms <- svm(train_sms$type~., data=train_sms)
    
    # 단계4. 분류모델 평가
    
        pred <- predict(model_sms, test_sms)    
    
    # 단계5. 분류정확도  
    
        table(pred, test_sms$type)
    
        # pred    ham spam
        #   ham  1187  178
        #   spam    1   28
        
        library(caret)
    
        confusionMatrix(pred, test_sms$type)
            
    # 단계6. 분류모델 수정 : 3개 kernel 방식 적용(linear, polynomial, sigmoid)
    # 각 kernel 방식에 따른 차이점 비교  
    
    
        model_l <- svm(train_sms$type~., data=train_sms, kernel='linear')
        model_p <- svm(train_sms$type~., data=train_sms, kernel='polynomial')
        model_s <- svm(train_sms$type~., data=train_sms, kernel='sigmoid')
        
        
        pred_l <- predict(model_l, test_sms)  
        pred_p <- predict(model_p, test_sms)  
        pred_s <- predict(model_s, test_sms)  
    
        table(pred_l, test_sms$type)
        # pred_l  ham spam
        #   ham  1170  112
        #   spam   18   94
       
        table(pred_p, test_sms$type)
        # pred_p  ham spam
        #   ham  1188  206
        #   spam    0    0
       
        table(pred_s, test_sms$type)
        # pred_s  ham spam
        #   ham  1188  193
        #   spam    0   13
    
    
    
        
        
        
        
        
        
        
        
#######################################
### 스캔된 이미지 문자 인식 
#######################################
    # 1. 파일 가져오기 
        letterdata = read.csv(file.choose())	#letterdata.csv
        str(letterdata) # 'data.frame':	20000 obs. of  17 variables:
        # y : letter, x : 나머지 16
        
    # 2. 데이터 셋 생성 
        set.seed(415)
        idx = sample(1:nrow(letterdata), 0.7*nrow(letterdata))
        training_letter = letterdata[idx, ]
        testing_letter  = letterdata[-idx, ]
        
    # 3. NA 제거 
        training_letter2 = na.omit(training_letter)
        testing_letter2 = na.omit(testing_letter)
        
    # 4. 분류모델 생성 
        model_letter <- svm(letter~., data = training_letter2)
    
    # 5. 분류모델 평가 
        pred_letter <- predict(model_letter, testing_letter2)
        
        # 혼돈 matrix 
        t <- table(pred_letter, testing_letter2$letter)
        
        sum(diag(t))/nrow(testing_letter2)
        # 0.9376667
        
        
        # 대각선이 정분류
        result <- pred_letter == testing_letter2$letter # TRUE/FALSE
        prop.table(table(result))
        #      FALSE       TRUE 
        # 0.06233333 0.93766667 
                
        
        
        
        
        