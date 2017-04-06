# chap03_4_RM





##################################################
#randomForest
##################################################
# 결정트리(Decision tree)에서 파생된 모델 
# 랜덤포레스트는 앙상블 학습기법을 사용한 모델
# 앙상블 학습 : 새로운 데이터에 대해서 여러 개의 Tree로 학습한 다음, 
# 학습 결과들을 종합해서 예측하는 모델(PPT 참고)
# DT보다 성능 향상, 과적합 문제를 해결


# 랜덤포레스트 구성방법(2가지)
# 1. 결정 트리를 만들 때 데이터의 일부만을 복원 추출하여 트리 생성 
#  -> 데이터 일부만을 사용해 포레스트 구성 
# 2. 트리의 자식 노드를 나눌때 일부 변수만 적용하여 노드 분류
#  -> 변수 일부만을 사용해 포레스트 구성 
# [해설] 위 2가지 방법을 혼용하여 랜덤하게 Tree(학습데이터)를 구성한다.

# 새로운 데이터 예측 방법
# - 여러 개의 결정트리가 내놓은 예측 결과를 투표방식(voting) 방식으로 선택 


install.packages('randomForest')
library(randomForest) # randomForest()함수 제공 


data(iris)

    # 1. 랜덤 포레스트 모델 생성 
        # 형식) randomForest(y ~ x, data, ntree, mtry)
        model = randomForest(Species~., data=iris)  # ntree : 500, mtry : 2 --> 기본값
        model

        (50+47+47)/nrow(iris) # 0.96
        
        
    # 2. 파라미터 조정 300개의 Tree와 4개의 변수 적용 모델 생성 
        model = randomForest(Species~., data=iris, 
                             ntree=300, mtry=4, na.action=na.omit )
        model # 4.67%의 오차율
        
        t <- table(model$predicted,iris$Species)
        t <- table(iris$Species, model$predicted)
        (t[1,1]+t[2,2]+t[3,3])/nrow(iris)
        
        
    # 3. 최적의 파리미터(ntree, mtry) 찾기
        # - 최적의 분류모델 생성을 위한 파라미터 찾기
        
        ntree <- c(400, 500, 600)
        mtry <- c(2:4)

        # 2개 vector이용 data frame 생성 
        param <- data.frame(n=ntree, m=mtry)
        param

        result <- numeric()
        cnt = 1
        for(i in param$n){     # 400, 500, 600
            cat('ntree = ',i,'\n')
            for(j in param$m){   # 2, 3, 4
                cat('\tmtry = ',j,'\n')
                model <- randomForest(Species~., data=iris, ntree=i, mtry=j, na.action = na.omit)
                print(model)
                t <- table(model$predicted, iris$Species)
                result[cnt] <- (t[1,1]+t[2,2]+t[3,3])/nrow(iris)
                cnt = cnt+1
            }
        }
        result
        
        
        
    # 4. 중요 변수 생성  :: 옵션 - importance = T, 함수 - importance(모델)
        model3 = randomForest(Species ~ ., data=iris, 
                              ntree=500, mtry=2, 
                              importance = T,
                              na.action=na.omit )
        model3 

        
        importance(model3)
        #                 setosa versicolor virginica MeanDecreaseAccuracy MeanDecreaseGini
        # Sepal.Length  6.182264   7.583242  8.999583            11.381820         10.53708
        # Sepal.Width   4.334040   1.235607  5.699199             5.513061          2.29815
        # Petal.Length 22.564090  34.392203 26.107546            33.548675         42.16998
        # Petal.Width  21.609265  34.184265 30.487711            33.603547         44.21454
        
        # MeanDecreaseAccuracy : 분류 정확도의 개선에 기여하는 변수. 높을수록 중요한 것! : Petal.Width
        # MeanDecreaseGini : 노드 불순도(불확실성) 개선에 기여하는 변수 : Petal.Width

        # 시각화        
        varImpPlot(model3)
        
        
        # entropy : 불확실성 척도
        # 동전 앞/뒷면 = 0.5
        x1 <- 0.5; x2 <- 0.5    # x1: 앞면, x2: 뒷면
        e1 <- -x1*log2(x1) + -x2*log2(x2)
        e1 # 1
        
        # 동전 앞/뒷면 = 0.5
        x1 <- 0.8; x2 <- 0.2    # x1: 앞면, x2: 뒷면
        e2 <- -x1*log2(x1) + -x2*log2(x2)
        e2 # 0.7219281
        
        # 불확실성이 1 -> 0.72로 떨어졌다. 즉, 확률이 0.5일때보다 0.8이되면 앞면이 나올 확률이 높아지기 때문에
        # 불확실성이 낮아졌다고 볼 수 있다. 
        
        

        
        
        
        
        
#####################################################
#randomForest: 데이터 분할(메모리 효율성)  
#####################################################
    
    install.packages('foreach')
    library(foreach)
        
        
    m <- matrix(1:9, 3,3)
    m
    # 형식) foreach(반복수) %do% 반복작업 
    foreach(i=1:ncol(m)) %do% mean(m[,i])  # list 반환
    # .combine 옵션에 따라 반환형태를 정의할 수 있다.
    foreach(i=1:ncol(m), .combine = c) %do% mean(m[,i]) # vector 반환
    foreach(i=1:ncol(m), .combine = rbind) %do% mean(m[,i]) # matrix 반환   
    
    # 형식) foreach(반복수) %do% 반복작업 
    # 1000 tree -> 250씩 4개 분할 처리 
    # foreach(반복수) %do% randomForest()
    model_iris <- foreach(i= rep(250, 4), .combine = combine) %do% 
        randomForest(Species~., data=iris, ntree=i, mtry=2, na.action=na.omit )

    model_iris
        
    
    
#####################################################
#randomForest: 병렬 처리(CPU 효율성) 
#####################################################
    library(randomForest) 
    library(foreach)
    install.packages('doParallel')
    library(doParallel) 
    
    # 멀티코어(4개 cpu 사용) 방식의 병렬 처리
    registerDoParallel(cores=4) # multicore수 4개 지정  
    getDoParWorkers() # 현재 사용 가능한 core 수 보기 
    
    
    # 형식) foreach(반복수) %dopar% 반복할 작업
    system.time(
        rf_iris <- foreach(ntree=rep(250, 8), .combine=combine, .packages = 'randomForest', multicombine=TRUE) %dopar%
            randomForest(Species~., data=iris, ntree=ntree, na.action=na.omit )
    )
    
    
    rf_iris
        
        

        


        