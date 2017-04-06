# chap02_Regression





##################################

    # 선형회귀 분석

##################################

    # 의료비 예측
    # - 의료보험 가입자 1,338명을 대상으로 한 데이터 셋으로 의료비 인상 예측 

    # 1. 데이터 셋 가져오기 - insurance.csv

        insurance <- read.csv('C:/NCS/Rwork_II/data/insurance.csv', header = T)
        str(insurance)
        #'data.frame':	1338 obs. of  7 variables:
        #$ age     : 나이 : int  19 18 28 33 32 31 46 37 37 60 ...
        #$ sex     : 성별 :(x1) Factor w/ 2 levels "female","male": 1 2 2 2 2 1 1 1 2 1 ...
        #$ bmi     : 비도만 지수 : num  27.9 33.8 33 22.7 28.9 ...
        #$ children: 자녀수 : int  0 1 3 0 0 0 1 3 2 0 ...
        #$ smoker  : 흡연 여부 :(x2) Factor w/ 2 levels "no","yes": 2 1 1 1 1 1 1 1 1 1 ...
        #$ region  : 지역 Factor w/ 4 levels "northeast","northwest",..: 4 3 3 2 2 3 3 2 1 2 ...
        #$ charges : 의료비(y) : num  16885

        
    # 2. 데이터 탐색
    
        # 1) 의료비 분포 보기 
            summary(insurance$charges)
            # 대표값으로 대칭성 확인  
            # 평균(13270) > 중위수(9382) > 최빈수(?) : 오른쪽 비대칭(왼쪽 기울어짐)
            # 평균 < 중위수 < 최빈수 : 왼쪽 비대칭(오른쪽 기울어짐)
            
        # 2) 수치형 칼럼 간의 상관관계보기 
            cor(insurance[c('age', 'bmi', 'children', 'charges')])
        
        # 3) 상관관계 시각화 
            library(psych)
            pairs.panels(insurance[c('age', 'bmi', 'children', 'charges')])

            
    # 3. 회귀모델 생성
            
        # 1) 데이터 셋 생성(7:3 비율) 
            set.seed(123) # random 결과를 동일하게
            idx = sample(1:nrow(insurance), 0.7*nrow(insurance))
            training_ins = insurance[idx, ]
            testing_ins = insurance[-idx, ]
            dim(training_ins) # 936   7
            dim(testing_ins) # 402   7
            
        # 2) 회귀모델 생성
            model_ins <- lm(charges ~ age + children + bmi + sex + smoker, data=training_ins)
            model_ins # 절편과 기울기 보기

    # 4. 회귀모델 평가
        summary(model_ins) # Adjusted R-squared:  0.7466 

    # 5. 회귀모델 성능 평가 - 검정 데이터 이용
        pred <- predict(model_ins, testing_ins)
        pred # 검정데이터의 의료비(charges) 예측 

        # 상관계수로 예측치 성능 평가  
        cor(pred, testing_ins$charges) # 0.8678934


        
##################################################
#  모델 설명력 향상 : 변수 선택법, 상호작용 적용  
##################################################

# 1. 변수 선택법 적용 : 영향력이 있는 설명변수를 선택하는데 효과적이다.  
    step(model_ins, direction="both")
    
    model_ins <- lm(charges ~ age + children + bmi + smoker, data=training_ins)
    model_ins # 절편과 기울기 보기

    # 회귀모델 : 설명력 
    summary(model_ins) # Adjusted R-squared:  0.7468 (0.0002) 


# 2. 상호작용 변수 적용 : 상호영향을 주는 변수 반영  

    # bmi -> 더미변수 : 30이상 -> 고도비만 : 1, 아니면 : 0
    training_ins$bmi2 <- ifelse(training_ins$bmi >= 30, 1, 0)
    head(training_ins$bmi2) # 0 0 1 1 0 0
    
    # 상호작용 변수 적용 모델 생성 
    model_ins2 <- lm(charges ~ age + bmi + children + bmi2 * smoker, data=training_ins)
    summary(model_ins2) # Adjusted R-squared:  0.8561
    
    
    
    
    
    
    
    
    
    
    
    

    
    
###################################
#### 수치예측 관련 모델
###################################
    # 모델트리 : 선형회귀모델 방식으로 모델 생성 - RWeka
    # - y변수를 수치로 예측
    # - 선형회귀모델 적용  
    
    install.packages('RWeka')
    library(RWeka) # M5P()함수 제공 
    
    # 1) 데이터 셋 가져오기
        # 포루트갈산의 화이트 와인 실습파일 
        wine <- read.csv('C:/NCS/Rwork_II/data/whitewines.csv')
        str(wine) # 'data.frame':	4898 obs. of  12 variables:
        # y변수 : quality(블라인드 테스트를 통해서 0~10 등급으로 와인의 질 적용)
        # x변수 : 나머지 11개 변수(화학적 특성)
        # -> 산도(acidity), 당도(sugar), 염화물(chlorides),황(sulfur),알코올(alcohol) 등 특성) 
    
    # 2) 데이터 분석/데이터 셋 생성 
        hist(wine$quality) # 정규분포 
        shapiro.test(wine$quality)  # 2.2e-16 < 0.05 정규분포를 따르지 않는데 따른다고 가정하고 시작함
        
        
        idx = 1:3700 # 4898 중에서 3700개를 training으로 지정 
        training_wine = wine[idx, ]
        testing_wine = wine[-idx, ]
        dim(training_wine) # 3700   12
        dim(testing_wine) # 1198   12
    
    
    # 3) 모델트리 모델 생성  
        model_wine <- M5P(quality ~ ., data = training_wine) 
        model_wine
    
    # 4) 모델 성능 평가 - 검정 데이터 이용 
        pred2 <- predict(model_wine, testing_wine)
    
        # (1) 요약통계량으로 평가 
            summary(pred2)
            summary(testing_wine$quality)
        
        # (2) 상관계수로 평가 
            cor(pred2, testing_wine$quality) # 0.6293855
    
    
