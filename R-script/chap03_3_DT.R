# chap03_3_DT






library(rpart) # rpart() : 분류모델 생성 
install.packages("rpart.plot")
library(rpart.plot) # prp(), rpart.plot() : rpart 시각화
install.packages('rattle')
library('rattle') # fancyRpartPlot() : node 번호 시각화 


     # 단계1. 실습데이터 생성 

        data(iris)
        set.seed(415)
        idx = sample(1:nrow(iris), 0.7*nrow(iris))
        train = iris[idx, ]
        test = iris[-idx, ]
        dim(train) # 105 5
        dim(test) # 45  5
        
        table(train$Species)
    
        
        
    # 단계2. 분류모델 생성 
        
        # rpart(y변수 ~ x변수, data)
        model = rpart(Species~., data=train) # iris의 꽃의 종류(Species) 분류 
        model
    
        # 분류모델 시각화 - rpart.plot 패키지 제공 
        prp(model) # 간단한 시각화   
        rpart.plot(model) # rpart 모델 tree 출력
        fancyRpartPlot(model) # node 번호 출력(rattle 패키지 제공)
        
    
        
    # 단계3. 분류모델 평가  
        
        pred1 <- predict(model, test) # 비율 예측 --> rpart는 기본적으로 비율을 반환한다, ctree가 분류!
        pred2 <- predict(model, test, type="class") # 분류 예측 

        pred1[1:5]
        pred2[1:5]
        
        
        # 1) 분류모델로 분류된 y변수 보기 
            table(pred)
        
        # 2) 분류모델 성능 평가 
            table(pred, test$Species)
            
            
            


            
    
            
##################################################
# Decision Tree 응용실습 : 암 진단 분류 분석
##################################################
   
     # "wdbc_data.csv" : 유방암 진단결과 데이터 셋 분류

    # 1. 데이터셋 가져오기 
        wdbc <- read.csv('C:/NCS/Rwork_II/data/wdbc_data.csv', stringsAsFactors = FALSE)
        str(wdbc)
        
        
    # 2. 데이터 탐색 및 전처리 
        wdbc <- wdbc[-1] # id 칼럼 제외(이상치) 
        head(wdbc)
        head(wdbc[, c('diagnosis')], 10) # 진단결과 : B -> '양성', M -> '악성'
    
        # 목표변수(y변수)를 factor형으로 변환 
        wdbc$diagnosis <- factor(wdbc$diagnosis, levels = c("B", "M"))
        wdbc$diagnosis[1:10]
    
        
    # 3. 정규화 - scale() : 서로 다른 특징을 갖는 칼럼값을 균등하게 적용 
        normalize <- function(x){ # 정규화를 위한 함수 정의 
            return ((x - min(x)) / (max(x) - min(x)))
        }
    
        
        # wdbc[2:31] : x변수에 해당한 칼럼 대상 정규화 수행 
        wdbc_x <- as.data.frame(lapply(wdbc[2:31], normalize))
        wdbc_x
        summary(wdbc_x) # 0 ~ 1 사이 정규화 
        class(wdbc_x) # [1] "data.frame"
        nrow(wdbc_x) # [1] 569
        
        wdbc_df <- data.frame(wdbc$diagnosis, wdbc_x)
        dim(wdbc_df) # 569  31
        head(wdbc_df)
    
        
    # 4. 훈련데이터와 검정데이터 생성 : 7 : 3 비율 
        idx = sample(nrow(wdbc_df), 0.7*nrow(wdbc_df))
        wdbc_train = wdbc_df[idx, ] # 훈련 데이터 
        wdbc_test = wdbc_df[-idx, ] # 검정 데이터 
        dim(wdbc_train) # [1] 398  30
        dim(wdbc_test) # [1] 171  30
        
        
    # 5. rpart 분류모델 생성 
        model_wdbc <- rpart(wdbc.diagnosis ~ ., data = wdbc_train)
        model_wdbc
        
        rpart.plot(model_wdbc)
        fancyRpartPlot(model_wdbc)
        
        
    # 6. 분류모델 평가  
            
        pred_wdbc <- predict(model_wdbc, wdbc_test, type='class')
        pred_wdbc        
        
        table(pred_wdbc, wdbc_test$wdbc.diagnosis)
        # pred_wdbc  B  M
        #         B 89  3
        #         M  8 71
        
        (89+71)/nrow(wdbc_test)
        
        
        
        
            