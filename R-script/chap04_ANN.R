# chap04_ANN





##################################################
#Neural Network 
##################################################
# 신경망(neural network) 기법은 반복적인 학습 과정을 거쳐 데이터에 내재되어 있는 
# 패턴을 찾아내고 이를 일반화함으로써 대용량 데이터로부터 의사결정에 필요한 
# 유용한 정보를 찾아내는 블랙박스 기법이다. 
# 여러 분야의 다양한 문제에 적용될 수 있고, 독립변수와 종속변수의 관계 확인이 어려운  
# 데이터에 대해서도 좋은 결과를 주는 것으로 알려져 있다. 
# 하지만, 분류나 예측 결과만을 제공할 뿐 결과가 어떻게 나왔는가에 대한 
# 이유를 설명하지 못한다.

# 주요 패키지 : nnet(초기), neuralnet(최신) 

##################################################
# Neural Network : nnet
##################################################

install.packages("nnet") # 뉴럴네트워크 
library(nnet)
?nnet

#################################
# [ANN 기초 실습] 
#################################

# 1) data frame 생성 
    df = data.frame(
        x2 = c(1:6),
        x1 = c(6:1),
        y = factor(c('n','n','n','y','y','y'))
    )
    str(df)
    df

# 2) 분류모형 생성 - nnet(formula, size = hidden-node의 수) 
    
    model_net1 = nnet(y ~ ., df, size = 1) # size는 분석자가 지정
    
    # network, input, output 보기 
    model_net1
    
    # 가중치(weights) 내용 보기 
    summary(model_net1) 



# 3) 분류모형 예측
    model_net1$fitted.values # 변수 이용 
    
    predict(model_net1, df) # 함수 이용
    
    p <- predict(model_net1, df, type="class") # 분류 결과(y변수) 


# 4) 분류정확도 
    table(p, df$y) 



#################################
# [iris 이용 ANN 실습] 
#################################

    data(iris)
    set.seed(123) # random 결과를 동일하게 지
    idx = sample(1:nrow(iris), 0.7*nrow(iris))
    training = iris[idx, ]
    testing = iris[-idx, ]
    training
    testing
    names(training)
    nrow(training) # 105(70%)


    # 분류모델 생성 
    model_net_iris1 = nnet(Species ~ ., training, size = 1, maxit=5000) 
    model_net_iris1 # 11 weights
    
    model_net_iris3 = nnet(Species ~ ., training, size = 3) 
    model_net_iris3 # 27 weights
    
    #size는 은닉층 노드 수, 정규화 없이 신경망 만들기
    summary(model_net_iris1) # 11개 가중치 확인
    summary(model_net_iris3) # 27개 가중치 확인 
    
    
    # 분류모델 평가 
    predict(model_net_iris1, testing, type = "class") # 분류 결과 보기 
    predict(model_net_iris3, testing, type = "class") # 분류 결과 보기 
    
    
    # 혼돈 매트릭스 적용 
    table(predict(model_net_iris1, testing, type = "class"), testing$Species)
    
    table(predict(model_net_iris3, testing, type = "class"), testing$Species)

    
    
#------------------------------------------------------------
# [ANN 고려사항] 
# 과적합(overfitting)을 피하기 위해서 정규화 과정이 필요
# 최적의 Hidden node 찾기  
#------------------------------------------------------------
    
    
    
    
    
    
    
##################################################
# Neural Network : neuralnet 
##################################################
# - 가장 최근에 나온 패키지 
# - y변수에 수치 데이터만 가능(범주형 변수 안됨)
# - 예) yes, no -> 1, 0 변환 
# - 은닉노드를 갖는 선형회귀 분석 방법과 유사 
# - Back Propagation 알고리즘 적용 
    
    install.packages('neuralnet')
    library(neuralnet)
    
    
    data("iris")
    table(iris$Species) 
    set.seed(123) # random 결과를 동일하게 지
    idx = sample(1:nrow(iris), 0.7*nrow(iris))
    training_iris = iris[idx, ]
    testing_iris = iris[-idx, ]
    dim(training_iris) # 105   6
    dim(testing_iris) # 45  6
    
    # 1) 숫자형으로 변환 Species2 칼럼 생성 
        training_iris$Species2[training_iris$Species == 'setosa'] <- 1 
        training_iris$Species2[training_iris$Species == 'versicolor'] <- 2 
        training_iris$Species2[training_iris$Species == 'virginica'] <- 3
        training_iris$Species <- NULL # 기존 문자열 칼럼 제거 
        head(training_iris); tail(training_iris)
        
        testing_iris$Species2[testing_iris$Species == 'setosa'] <- 1 
        testing_iris$Species2[testing_iris$Species == 'versicolor'] <- 2 
        testing_iris$Species2[testing_iris$Species == 'virginica'] <- 3
        testing_iris$Species <- NULL # 기존 문자열 칼럼 제거 
        head(testing_iris); tail(testing_iris)
    
    # 2) 정규화 함수 정의 : 0 ~ 1 범위로 정규화 
        normal <- function(x){
            return (( x - min(x)) / (max(x) - min(x)))
        }
        
        # 칼럼 값 정규화 - 전체 칼럼값의 규모를 축소(0~1)
        training_nor <- as.data.frame(lapply(training_iris,normal))
        summary(training_nor) # 0 ~ 1 확인
        
        testing_nor <- as.data.frame(lapply(testing_iris,normal))
        summary(testing_nor) # 0 ~ 1 확인
    
    
        
    # 3) 분류모델 생성 - 은닉 노드 1개 
        # 형식) neuralnet(formula, data, hidden) 
        model_net = neuralnet(Species2 ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, 
                              data=training_nor, hidden = 1, linear.output=FALSE)
        model_net
        model_net$data
        x11()
        plot(model_net) # Neural Network 모형으로 시각화 
    
        
    # 4) 모델 성능 평가 - compute() 함수 이용 검정 데이터 예측 생성
        # 모델의 정확도를 평가하기 위해서 predict() 대신 compute()함수 이용
        # 예측된 꽃의 분류와 실제 값(test set) 사이의 상관관계 측정 
        model_result <- compute(model_net, testing_nor[c(1:4)])
        model_result$net.result # 분류 예측값 보기  
        
        # 상관분석 : 두 수치 벡터 값의 상관계수로 두 변수 간 선형관계의 강도 측정 
        cor(model_result$net.result, testing_nor$Species2)
        # 0.9605610556
    
    # 5) 분류모델 성능 향상 : 은닉노드 2개 지정(backprop 적용)  
        model_net2 = neuralnet(Species2 ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, 
                              data=training_nor, hidden = 2, linear.output=FALSE,
                              algorithm="backprop", learningrate=0.01) 
        # algorithm="backprop" : 역전파 알고리즘 적용 
        # learningrate : backprop 사용시 설정
        model_net2
        
        model_result2 <- compute(model_net2, testing_nor[c(1:4)])
        cor(model_result2$net.result, testing_nor$Species2)  
        # 0.9642696519
    
    
    
    
    
