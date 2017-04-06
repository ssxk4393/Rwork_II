##########################
## 제3장 kNN 연습문제 
##########################

# 문) 기상데이터를 이용하여 내일 비(RainTomorrow) 유무를 kNN 알고리즘을 적용하여 분류하시오.
# <조건1> y변수 : RainTomorrow, x 변수 : 1,2,22,23 칼럼을 제외한 나머지
# <조건2> 데이터 전처리 : NA를 포함하는 모든 관측치 제외(na.omit()함수 이용) 
# <조건3> 7:3 비율로 train, test 데이터 분리
# <조건4> 모델 생성 시 x변수에서 Factor형 변수 제외 
# <조건5> 모델 성능 향상을 위한 최적의 K값 찾기(110 ~ 120 범위에서) 

#Weatehr data set
weatherAUS <- read.csv('C:/NCS/Rwork_II/data/weatherAUS.csv')
str(weatherAUS)
weatherAUS <- weatherAUS[,c(-1,-2,-22,-23)]
summary(weatherAUS)
weatherAUS <- na.omit(weatherAUS)

idx <- sample(1:nrow(weatherAUS), nrow(weatherAUS)*0.7)
train <- weatherAUS[idx,]
test <- weatherAUS[-idx,]
dim(train)
dim(test)

str(train)

sqrt(12164)
model_knn <- knn(train[,c(-6,-8,-9,-20)], test[,c(-6,-8,-9,-20)], train[,20], k=111)
model_knn
table(model_knn, test[,20])
# model_knn   No  Yes
#       No  3897  648
#       Yes  140  529

r <- numeric()
k <- 100:120
for(i in k){
    model_knn <- knn(train[,c(-6,-8,-9,-20)], test[,c(-6,-8,-9,-20)], train[,20], k=i)
    t <- table(model_knn, test[,20])
    a <- (t[1,1]+t[2,2])/sum(t)
    cat('k= ',i,'\n')
    cat('분류정확도 = ',a,'\n')
    r[i-99] <- a
}

which(r==max(r)) # 1  --> 101 일때 최대 정확도 0.8498274
sort(r, decreasing = T)
r[18]


