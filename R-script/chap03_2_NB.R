# chap03_2_NB







##################################################
# Naive Bayes 알고리즘 
##################################################

    # 조건부 확률 적용 예측 
    # 비교적 성능 우수
    # 베이즈 이론 적용
    #  -> 조건부 확률 이용 
    #  -> 스펨 메시지 분류에 우수함
    
    # 조건부 확률 : 사건 A가 발생한 상태에서 사건 B가 발생할 확률 
    # P(B|A) = P(A|B) * P(B) / P(A)
    # ----------------------------------------------------------
    # ex) 비아그라,정력 단어가 포함된 메시지가 스팸일 확률
    # P(스팸|비아그라,정력)
    # 사건 A : 비아그라, 정력 -> P(A) : 5/100(5%)
    # 사건 B : 스팸 -> P(B) : 20/100(20%)
    # P(A|B) : 스팸일때 비아그라, 정력일 경우 -> 4/20(20%) 

    p <- (4/20)*(20/100)/(5/100)
    p   # 0.8
    s

    # ex2) 비아그라, 정력 단어(사건 A)가 포함된 메시지가 햄(사건 B)일 확률
    # 사건 A : 비아그라, 정력 -> P(A) : 5/100(5%)
    # 사건 B : 햄 -> P(B) : 80/100(80%)
    # P(A|B) : 햄일때 비아그라, 정력일 경우 -> 1/80(1.25%) 

    p2 <- (1/80)*(80/100)/(5/100)
    p2    # 0.2
    

##################################################
# Naive Bayes 기본실습 : iris
##################################################

# 패키지 설치 
install.packages('e1071')
library(e1071) # naiveBayes()함수 제공   

    # 1. train과 test 데이터 셋 생성  
        data(iris)
        set.seed(415) # random 결과를 동일하게 지정
        idx <- sample(1:nrow(iris), 0.7*nrow(iris)) # 7:3 비율 
        
        train <- iris[idx, ]
        test <- iris[-idx, ]
        train; test
        nrow(train) # 105

        
    # 2. 분류모델 생성 : train data 이용    
        
        # 형식1) naiveBayes(train$x변수, train$y변수)
        # 형식2) y~x  ==> naiveBayes(Species ~ ., data=train)   (이렇게 맞나?) 
        model <- naiveBayes(train[-5], train$Species) 
        model # 105개 학습 데이터를 이용하여 x변수(4개)를 y변수로 학습시킴  


    # 3. 분류모델 평가 : test data 이용 
        # 형식) predict(model, test, type='class')  
        p <- predict(model, test, type='class') 
        # type='class' : y변수의 범주의 값으로 분류를 해준다.
        # type='class'를 지정안하면 비율로 값을 반환한다. 
        p   

        
    # 4. 분류모델 평가(예측결과 평가) 
        table(p, test$Species) # 예측결과, 원형 test의 y변수   


    # 분류 정확도
        # p            setosa versicolor virginica
        # setosa         14          0         0
        # versicolor      0         15         2
        # virginica       0          2        12
    
        (14+15+12)/nrow(test) # 0.9111111
        
        
        
        
        
        
        
##################################################
# Naive Bayes 응용실습 : 기상데이터 분석
##################################################
        
    # 1. 데이터 가져오기 
        weatherAUS <- read.csv('C:/NCS/Rwork_II/data/weatherAUS.csv')
        weatherAUS <- weatherAUS[ ,c(-1,-2, -22, -23)] # 칼럼 제외 
        # weatherAUS <- na.omit(weatherAUS)
        head(weatherAUS)    
        summary(weatherAUS)
        
    # 2. 데이터 생성/전처리  
        set.seed(415)
        idx = sample(1:nrow(weatherAUS), 0.7*nrow(weatherAUS))
        train_w = weatherAUS[idx, ]
        test_w  = weatherAUS[-idx, ]
        
        head(train_w)
        head(test_w)
        dim(train_w) # [1] 25816    20
        dim(test_w) # [1] 11065    20
        
        
    # 3. 분류모델(분류기) 생성 : train data 이용    
        # 형식2) niveBayes(y변수 ~ x변수, data)  
        model = naiveBayes(RainTomorrow ~ ., data = train_w)
        model
        
    # 4. 분류모델 평가(예측기) : test data 이용 
        # 형식) predict(model, test, type='class')
        p <- predict(model, test_w, type='class')
        table(p, test_w$RainTomorrow)
        
        (7244+1508)/nrow(test_w)
        # 0.7909625
        
        
    # 5. 분류정확도 
    # ----------------------------------------------------------
    # 가상데이터에 대한 정분류율, 오분류율, 정확률, 재현율 
    # ----------------------------------------------------------
            
        # p       No  Yes
        #   No  7244 1002
        #   Yes 1139 1508
       
         # 1. 정분류율(Accuracy) 
            (7244+1508)/nrow(test_w)
            # 0.7909625
        
        # 2. 오분류율(Inaccuracy)  
            (1002+1139)/nrow(test_w)
            # 0.193493
        
        # 3. 정확률(Precision) : 참이라고 검출한 것 중, 참의 수
            1508/(1139+1508)
            # 0.5697015
            
        # 4. 재현율(Recall) : 실제 참인 것들 중, 참의 수
            1508/(1002+1508)
            # 0.1723035
            
        # 5. 거짓 긍정률(False Positive) : 거짓인데 참이라고 분류
            1139/(7244+1139)
            # 0.1358702
            
        # 6. 거짓 부정률(False Positive) : 참인데 거짓이라고 분류
            1002/(1002+1508)
            # 0.3992032
            
            
            
## 문) weatherAUS 데이터 셋을 대상으로 random하게 30,000개 표본을 샘플링하여
##     다음과 같이 k겹 교차검정을 수행하시오
            
            
            
            weatherAUS <- read.csv('C:/NCS/Rwork_II/data/weatherAUS.csv')
            weatherAUS <- weatherAUS[ ,c(-1,-2, -22, -23)] # 칼럼 제외 
            idx = sample(1:nrow(weatherAUS), 30000)
            
            weatherAUS <- weatherAUS[idx,]
            
        #단계1: K겹 교차검정을 위한 샘플링 
            library(cvTools)
            cross <- cvFolds(nrow(weatherAUS), K=3, R=1) 
            cross    
        
        #단계2: K겹 교차검정 데이터 보기
            str(cross) # 구조 보기 
        
            cross # 5겹 교차검정 데이터 보기
            length(cross$which) # 30000
            dim(cross$subsets) # 30000   1
            
        
        #단계3: K겹 교차검정 수행  
            
            k=1:3 # 3겹  
            cnt=1 # 카운터 변수 -> 1차 테스트 
            acc <- numeric() # 분류정확도 저장 -> 2차 모델 생성  
            
            
            for(i in k){
                
                cat(i,'번째 검정\n')
                
                idx <- cross$subsets[cross$which==i,1]
                train_AUS <- weatherAUS[idx,]
                test_AUS <- weatherAUS[-idx,]
                
                
                model = naiveBayes(RainTomorrow ~ ., data = train_AUS)
                pred_AUS <- predict(model, test_AUS, type='class')
                
                t <- table(pred_AUS, test_AUS$RainTomorrow)
                
                acc[cnt] <- (t[1,1]+t[2,2])/nrow(test_AUS)
                
                cat('결과값 : ', acc[cnt],'\n\n')
                
                cnt <- cnt +1
            }
            
            
            acc
            mean(acc)
            
            
            
            
            