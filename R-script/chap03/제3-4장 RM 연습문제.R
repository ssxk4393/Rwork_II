##########################
## 제3-4장 RM 연습문제 
##########################

# 문1) 100개의 Tree와 2개의 분류변수를 파라미터로 지정하여 모델을 생성하고, 
#       분류정확도를 구하시오.
#  조건> 1,2,22,23 칼럼을 제외하여 데이터 셋 구성 
#  기상데이터(weatherAUS)

weatherAUS = read.csv(file.choose())
weatherAUS = weatherAUS[ ,c(-1,-2, -22, -23)]
str(weatherAUS)
weatherAUS <- na.omit(weatherAUS)


# RainTomorrow : y, 나머지 : x

?randomForest
model <- randomForest(RainTomorrow~., data=weatherAUS, mtree=100, mtry=2, na.action=na.omit, importance=T)
model

normalize <- function(x){ # 정규화를 위한 함수 정의 
    return ((x - min(x)) / (max(x) - min(x)))
}



# 문2) 변수의 중요도 평가를 통해서 가장 중요한 변수를 확인하고, 시각화 하시오. 

importance(model)
#                     No         Yes MeanDecreaseAccuracy MeanDecreaseGini
# MinTemp       35.67001   7.0020276             38.59612         251.6302
# MaxTemp       33.35604  -2.5936808             35.42218         231.7290
# Rainfall      25.38671  35.5953673             42.17228         302.5455
# Evaporation   33.33463  -4.8192887             32.29814         204.1507
# Sunshine      38.27569  48.8847634             56.58737         580.4798
# WindGustDir   52.56492 -18.7834662             40.95765         342.5475
# WindGustSpeed 42.69657  26.6864324             49.50508         294.1907
# WindDir9am    47.40134  -6.0901662             38.99617         353.2572
# WindDir3pm    48.32279 -12.7124765             40.91871         344.8973
# WindSpeed9am  27.41968  -0.2228503             25.82358         168.8743
# WindSpeed3pm  28.11506   2.9499717             28.73431         176.9704
# Humidity9am   32.41401  17.9546864             39.47363         301.2209
# Humidity3pm   49.01147  63.6943977             67.67066         802.2932
# Pressure9am   37.68864  13.7396429             42.00757         328.4166
# Pressure3pm   41.46532  19.8455702             46.65420         348.3195
# Cloud9am      20.17700  20.5051741             28.95212         231.1878
# Cloud3pm      20.71992  37.8794786             40.58515         350.3645
# Temp9am       40.39401  -4.0856003             42.67272         230.3072
# Temp3pm       27.87637   5.7018769             29.86953         255.7311

which(i[,3]==max(i[,3]))   # Humidity3pm 
which(i[,4]==max(i[,4]))   # Humidity3pm
varImpPlot(model)

confusionMatrix(model$predicted, weatherAUS$RainTomorrow)




