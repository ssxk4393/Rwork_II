##########################
## 제4장 ANN 연습문제 
##########################

# concrete.csv 데이터 셋 설명 
# 시멘트, 물, 골재 등의 콘크리트 관련 자재 8가지를 
# 투입하여 만들어진 콘크리트를 대상으로 내구력을    
# 예측하는 모델 생성 - 건축물의 내구성 추정에 활용

# 1. 데이터 가져오기 - concrete.csv
    concrete <- read.csv(file.choose(), header = T)
    str(concrete) # 'data.frame':	1030 obs. of  9 variables:
    # x 변수 : 8개 속성, y변수 : strength
    # x변수 : 시멘트,슬래그,화분,물,고성능 감수제,굵은골재,잔골재,숙성시간
    # y변수 : strength(콘크리트 강도) 

# 2. 데이터 정규화 : 9개 칼럼 정규화 
    
    # 정규화 함수
    normal <- function(x){
      return (( x - min(x)) / (max(x) - min(x)))
    }  

    concrete_df <- as.data.frame(lapply(concrete, normal))
    
# 3. train/test 생성(7:3 비율) 

    idx <- sample(1:nrow(concrete_df), nrow(concrete_df)*0.7)

    train_con <- concrete_df[idx,]
    test_con <- concrete_df[-idx,]
    
    dim(train_con)
    dim(test_con)
    
    str(train_con)
            
    
## formula 만드는 식
    n <- names(concrete)
    f <- as.formula(paste("strength ~", paste(n[!n %in% 'strength'], collapse = '+')))
    f
    
# 4-1. 분류모델 생성(은닉 노드 1개) & Network 시각화 

    node1 <- neuralnet(strength ~ cement+slag+ash+water+superplastic+coarseagg+fineagg+
                           age, data=train_con, hidden=1, linear.output=FALSE)
    
    plot(node1)

        
# 4-2. 분류모델 생성(은닉 노드 2개) & Network 시각화 

    node2 <- neuralnet(strength ~ cement+slag+ash+water+superplastic+coarseagg+fineagg+
                           age, data=train_con, hidden=2, linear.output=FALSE)
    
    plot(node2)
    
# 5. 분류모델 성능 평가  : 은닉 노드 1개 vs 은닉 노드 2개 성능 평가 

    pred1 <- compute(node1, test_con[,c(1:8)])    
    pred2 <- compute(node2, test_con[,c(1:8)])   
    
    cor(pred1$net.result, test_con$strength)  # 0.8021946564
    cor(pred2$net.result, test_con$strength)  # 0.9072225532
     
    
    
    
    
    
    
    
    

##################################################
# Deep learning Neural Network
##################################################
    
    install.packages("deepnet") # deep Learning 지원 
    library(deepnet) # nn.train()함수 제공 
    ## Warning: package 'deepnet' was built under R version 3.1.2
    
    # deep learning 알고리즘을 패키지화
    # deepnet()함수 제공 - 수치 data만 가능 
    # deep learning : 다중 은닉층(Multi Hidden Layer)을 갖는 ANN
    
    
    
    # 1. dbn.dnn.train : deep learning model 생성 함수 
    
        # * DNN : Deep Neural Networks(입력층 -> 은닉층 -> 출력층으로 이루어진 인공신경망)
        # * DBN : Deep Belief Networks(심층 신뢰 신경망 : 계속 층을 쌓아서 전체 신경망 구성) 
        
        # 형식) dbn.dnn.train(x, y, hidden)
        # x : 학습 데이터(matrix 형태만 가능)
        # y : 결과 데이터(vector나 matrix 형태 가능)
        # hidden : 은닉층 노드수(100,100,100) : 노드 100개인 은닉층 3개
        
        d1 <- c(rnorm(1000, 1, 0.5), rnorm(1000, -0.6, 0.2))
        d2 <- c(rnorm(1000, -0.8, 0.2), rnorm(1000, 2, 1))
        x <- matrix(c(d1, d2), nrow = 2000, ncol = 2) # 2000행2열 
        str(x) # [1:2000, 1:2] 
        
        y <-c(rep(1,1000),rep(0,1000)) 
        str(y) # [1:2000]
        
        
        
        ?dbn.dnn.train
        # x 학습데이터, y 결과데이터 -> 노드 1000개인 3층 은닉층 학습 
        dnn_model <- dbn.dnn.train(x, y, hidden = c(1000,1000,1000))
        dnn_model
    
        
        
    # 2. nn.predict() : deep learning 예측치 생성 
        
        # 형식) nn.predict(nn, x)
        # nn : train 기능으로 학습된 변수(exDnn) 
        # x : 학습데이터(matrix 형태만 가능)
        
        # (1) 검정데이터 생성
            t1 <- c(rnorm(1000, 1, 0.5), rnorm(1000, -0.6, 0.2))
            t2 <- c(rnorm(1000, -0.8, 0.2), rnorm(1000, 2, 1))
            test <- matrix(c(t1, t2), nrow = 2000, ncol = 2)# 학습데이터와 구조 동일   
        
        # (2) 예측치 생성(검정데이터 이용) 
            pred <-nn.predict(dnn_model, test)
            pred # 예측결과 확률값 

            cpred <- ifelse(pred[,1]>=0.5, 1, 0)
            cpred
                            
            table(cpred, y)
            (990 + 1000)/length(y)
            # 0.995
            
    # 3. nn.test() : 전체 에러율를 구하는 함수
    
        # 형식) nn.test(nn, x, y)
        # nn : deep learning model 
        # x : 검정 데이터
        # y : 학습 데이터
    
        nn.test(dnn_model, test, y) 
        # 0.0025
        
        
        
    
########################################
### OR 연산 
########################################
        
    ?nn.train() # Training Neural Network
    # - Training single or mutiple hidden layers neural network by BP
    # - Backpropagation 적용으로 error(cost)를 최소화 
    
    # 1. train/test set 생성 
        x = matrix(c(0,0,1,1,0,1,0,1), ncol=2) 
        y = c(0,1,1,1) # 이진 분류 
        x; y
    
        
    # 2. deep learning model 생성 : x -> y 형태로 학습 
        nn <- nn.train(x, y, hidden=c(2)) 
        # 기본 히든 노드, 학습률, 학습횟수 
        # hidden = c(10), learningrate = 0.8, numepochs = 3 
    
        
    # 3. deep learning 예측치 생성
        # 1) 학습이 안된 경우 
        pred <- nn.predict(nn, x) # x : 입력데이터로 평가   
        pred
        
        # 2) 튜닝 : 학습횟수(numepoch)와 학습률(learningrate) 높이기    
        nn <- nn.train(x, y, hidden=c(2), learningrate=10, numepoch=100)
        
        pred <- nn.predict(nn, x)
        pred
    
        # 분류결과 binary 적용 
        cpred <- ifelse(pred >= 0.5, 1, 0)
        table(cpred, y) # 분류정확도 : 100%
        
        nn.test(nn, x, y) 
    
        
        
        
########################################
### AND 연산 
########################################
    
        
    # 1. train/test set 생성 
        x = matrix(c(0,0,1,1,0,1,0,1), ncol=2) 
        y = c(0,0,0,1)
        x; y
    
    # 2. deep learning model 생성 : x -> y 형태로 학습 
        nn <- nn.train(x, y, hidden=c(2)) 
    
    
    # 3. deep learning 예측치 생성
    
        # 1) 학습이 안된 경우 
            pred <- nn.predict(nn, x)
            pred
        
        # 2) 학습횟수는 낮추고, 학습률을 높이는 방법  
            nn <- nn.train(x, y, hidden=c(2), learningrate=10, numepoch=100)
            
            pred <- nn.predict(nn, x)
            pred
    
        # 분류결과 binary 적용 
        cpred <- ifelse(pred >= 0.5, 1, 0)
        table(cpred, y) # 분류정확도 : 100%
    
    
        
        
#########################################
## XOR 문제해결  
#########################################
    # AND/OR 잘 분류됨 ANN 활성화 
    # XOR 문제로 인하여 ANN 침체
    # XOR 문제 해결로 인한 딥 신경망으로 복잡한 문제해결  
    
    # 1. dataset 생성 
        x = matrix(c(0,0,1,1,0,1,0,1), ncol=2) 
        y = c(0,1,1,0)
        x; y
    
        ?nn.train
        
    # 2. deepnet 학습시키기 
        library(deepnet)
        nn <- nn.train(x, y, hidden=c(2)) 
        
        # 1) 학습이 안된 경우 : 분류 안되는 경우 
            nn.predict(nn, x) # model를 이용하여 x(input) 예측 
        
        
        # 2-1) 학습 횟수 늘리기 : numepoch=100000(십만번)
            nn <- nn.train(x, y, hidden=c(2), numepoch=100000)
            
            pred <- nn.predict(nn, x)
            pred
    
            # 분류결과 binary 적용 
            cpred <- ifelse(pred >= 0.5, 1, 0)
            table(cpred, y) # 분류정확도 : 100%
        
        # 2-2) 학습 횟수는 낮추고, 학습률을 높이는 방법  
            nn <- nn.train(x, y, hidden=2, learningrate=10, numepoch=10000)
            
            pred <- nn.predict(nn, x)
            pred
    
            # 분류결과 binary 적용 
            cpred <- ifelse(pred >= 0.5, 1, 0)
            table(cpred, y) # 분류정확도 : 100%
