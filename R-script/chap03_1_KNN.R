# chap03_1_KNN




##################################################
#  k-Nearest Neighbors(kNN) 알고리즘 분류모형    #
##################################################

# 최근접 이웃(kNN) : k개의 인접한 값을 찾아서 분류 
# 비모수적 방법의 분류모형 
# 거리를 기반으로 계산하여 분류모형 생성 
# 유클리드안 거리 계산법 적용 


# kNN 개념 예제 : 과일(2개), 단백질(2개), 채소(2개) 
# 품목   : (단맛, 아삭거림)  분류 
# grape  : (8, 5)            과일(1)
# fish   : (2, 3)          단백질(2) 
# carrot : (7, 10)           채소(3)
# orange : (7, 3)            과일(1) 
# celery : (3, 8)            채소(3)
# cheese : (1, 1)          단백질(2) 


    # 1. train data 생성
        # y변수 factor vector : 분류그룹(1-과일, 2-단백질, 3-채소)
        train_df = data.frame(x1 = c(8, 2, 7, 7, 3, 1), 
                              x2 = c(5, 3, 10, 3, 8, 1), 
                              y=factor(c(1, 2, 3, 1, 3, 2)))
        train_df
        plot(train_df$x1, train_df$x2, col=train_df$y, xlab='단맛', ylab='아삭거림')
    
        # 토마토(6,4) vs 오렌지(7,3)
        sqrt((7-6)^2 +(3-4)^2)   # 1.414214
        
        # 토마토(6,4) vs 샐러리(3,8)
        sqrt((3-6)^2 +(8-4)^2)   # 5
        
        
    # 2. test data 생성 : 새로운 항목 - 토마토(6,4), 땅콩(3,8), 사과(10,9)
        test_df = data.frame(x1=c(6,3,10), x2=c(4,8,9))
        test_df
    
    # 3. kNN 분류모델 생성 
        install.packages('class')
        library(class) # knn()함수 제공 
    
        k <- sqrt(nrow(train_df))
        k # 2.44949 -> 3
        
        # 형식) knn(훈련데이터(y제외), 검정데이터, 훈련데이터 y변수, k)
        knn(train_df[, -3], test_df, train_df$y, k=3) # prob=TRUE : 예측 의미
        # [1] 1 3 1
        # 토마토 : 과일, 땅콩 : 채소, 사과 : 과일

        
        # KNN 알고리즘의 고려사항
        # 1. x변수 숫자(계산)
        # 2. y변수 분류 레이블
        # 3. 최적의 k값 구하기
        
###############################
# kNN 분류모형 : 간단 실습    # 
###############################
        
# 1. data 생성 
    data(iris)
    set.seed(415) # random 결과를 동일하게 지정
    idx = sample(1:nrow(iris), 0.7*nrow(iris) )
    training = iris[idx, ]
    testing = iris[-idx, ]
    training
    testing
    names(training)

# 2. 분류모델 생성 
    
    model.knn = knn(training[, -5], testing[, -5], training$Species, k = 3, prob=TRUE)
    model.knn
    summary(model.knn)


# 3. 분류모델 평가(검정데이터 적용) 
    t <- table(model.knn, testing$Species)


# 4. 분류모델 성능 평가 - 분류 정확도 
    # 분류정확도 = 정분류 / test set 길이 
    (t[1,1]+t[2,2]+t[3,3])/sum(t)
    # 0.9111111


    # k값 변경
    sqrt(nrow(training)) # 10.24695 -> 11
    model.knn = knn(training[, -5], testing[, -5], training$Species, k = 11, prob=TRUE)
    t <- table(model.knn, testing$Species)
    (t[1,1]+t[2,2]+t[3,3])/sum(t)
    # 0.9333333
    
    
    
    
    
    
    
    
    
###############################
# kNN 분류모델 : 응용 실습    # 
###############################
    
    # 1. 데이터셋 가져오기 
        #"wdbc_data.csv" : 유방암 진단결과 데이터 셋
        # Wisconsin Diagnostic Breast Cancer (WDBC) : 위스콘신 유방암 센터 
        wdbc <- read.csv('C:/NCS/Rwork_II/data/wdbc_data.csv', stringsAsFactors = FALSE)
        str(wdbc)
    
    
    # 2. 데이터 탐색 및 전처리 
        wdbc <- wdbc[-1] # id 칼럼 제외 
        head(wdbc)
        head(wdbc[, c('diagnosis')], 10)
    
        # 1) 목표변수(y변수)를 factor형으로 변환 
        
            wdbc$diagnosis <- factor(wdbc$diagnosis, levels = c("B", "M"))
            str(wdbc$diagnosis)
            # 백분율 적용 빈도수 보기  
            prop.table(table(wdbc$diagnosis)) * 100
            
            
        # 2) x변수 보기 : 30개 실험 측정치 칼럼 
            summary(wdbc[,c(2:31)])
        
        # 3) 수치 데이터 정규화
            normalize <- function(x){ # 정규화를 위한 함수 정의 
                return ((x - min(x)) / (max(x) - min(x)))
            }
        
            # wdbc[2:31] : x변수에 해당한 칼럼 대상 정규화 수행 
            wdbc_x <- as.data.frame(lapply(wdbc[2:31], normalize))
            wdbc_x
            class(wdbc_x) # [1] "data.frame"
            nrow(wdbc_x) # [1] 569
    
    
        # 4) 훈련데이터와 검정데이터 생성 : 7 : 3 비율 
            set.seed(415) # 시드값 적용 - 동일한 랜덤값 제공 
            idx = sample(1:nrow(wdbc_x), 0.7*nrow(wdbc_x))
            wdbc_train = wdbc_x[idx, ] # 훈련 데이터 
            wdbc_test = wdbc_x[-idx, ] # 검정 데이터 
            dim(wdbc_train) # [1] 398  30
            dim(wdbc_test) # [1] 171  30
        
            # 원본 데이터에서 y변수가 포함된 칼럼값 저장 - 분류모델 결과 확인용  
            wdbc_train_y <- wdbc[idx, 1] # 훈련 데이터의 diagnosis 칼럼 
            wdbc_test_y <- wdbc[-idx, 1] # 검정 데이터의 diagnosis 칼럼 
    
    
    # 3. kNN 분류모델 생성 
    
        # k값 구하기 - 훈련데이터의 제곱근
        dim(wdbc_train) # [1] 398  30
        k = sqrt(398)
        k # 19.94994 -> k = 19(홀수 지정 )
        wdbc_pred <- knn(wdbc_train, wdbc_test, wdbc_train_y, k=19)
        wdbc_pred # 분류예측모델 
    
    
    # 4. kNN 분류모델 성능 평가 
    
        # 검정데이터의 y변수와 분류모델 예측치와 비교 평가  
        table(wdbc_pred, wdbc_test_y) # 행 : 분류예측치, 열 : 원본 data 
        
        # 분류정확도 
        (110+54)/ nrow(wdbc_test)              
    
    
    # 5. 최적의 K값 구하기
        # k = 5 ~ 25[19]
        
        result <- numeric()
        k = 5:25
        for(i in k ){
            cat('k=',i,'\n')
            wdbc_pred <- knn(wdbc_train, wdbc_test, wdbc_train_y, k=i)
            t <- table(wdbc_pred, wdbc_test_y)
            print(t)
            cat('분류정확도 = ',(t[1,1]+t[2,2])/sum(t),'\n\n')
            result[i-4] <- (t[1,1]+t[2,2])/sum(t)
        }
        
        which(result==max(result))  # 2 12 -> 6, 16
        sort(result, decreasing = T)
        
        
        
        
        
        
        
        
        
    
    