##########################
## 제2장 연습문제 
##########################

# 문1) iris 데이터 셋을 대상으로 다음과 같이 모델트리 모델을 생성하시오.
# <조건1> y변수 : 5번칼럼, x변수 : 1 ~ 4 칼럼
# <조건2> y변수 대상 더미변수 생성 
# (setosa -> 1, versicolor -> 2, virginica -> 3)
# <조건3> 7:3 비율로 데이터 셋 구성(train_iris, test_iris)
# <조건4> 모델 평가는 상관계수 이용 

library(RWeka)

# 1. 데이터 셋 생성 

# 2. 모델 생성 

# 3. 예측치 생성 

# 4. 모델 평가 : 상관계수 평가 


# 문2) iris 데이터 셋을 대상으로 다음과 같이 다중선형회귀 모델을 생성하시오.
# <조건1> 변수 선택법(both)을 적용하여 변수 모델링 
# <조건2> y변수 : 5번칼럼, x변수 : 1 ~ 4 칼럼
# <조건3> y변수 대상 더미변수 생성 
# (setosa -> 1, versicolor -> 2, virginica -> 3)
# <조건4> 7:3 비율로 데이터 셋 구성(train_iris, test_iris)
# <조건5> 모델 평가는 상관계수 이용 

model2_iris <- lm(species1 ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data = training_iris) 
model2_iris 
summary(model2_iris) # 0.932 

# 1. 변수 선택법 

# 2. 회귀모델

# 3. 모델 평가 