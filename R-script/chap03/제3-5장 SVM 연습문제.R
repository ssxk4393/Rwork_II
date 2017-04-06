##########################
## 제3-5장 SVM 연습문제 
##########################

# 문1) 기상데이터를 다음과 같이 SVM에 적용하여 분류하시오. 
# 조건1> RainTomorrow~MinTemp+MaxTemp+Rainfall+Sunshine+Evaporation 포물라 적용 
# 조건2> kernel = radial, linear 2개 적용, 각 predict결과 비교 

# 1. 파일 가져오기 
    weatherAUS = read.csv(file.choose())	#weatherAUS.csv
    weatherAUS = weatherAUS[ ,c(-1,-2, -22, -23)] # 칼럼 제외 
    str(weatherAUS)

# 2. 데이터 셋 생성 
    set.seed(415)
    idx = sample(1:nrow(weatherAUS), 0.7*nrow(weatherAUS))
    training_w = weatherAUS[idx, ]
    testing_w  = weatherAUS[-idx, ]


# 3. NA 제거 
    training_w = na.omit(training_w)
    testing_w = na.omit(testing_w)

# 4. 분류모델 생성 

    model_svm1 <- svm(RainTomorrow~MinTemp+MaxTemp+Rainfall+Sunshine+Evaporation, data=training_w, kernel='radial')
    model_svm2 <- svm(RainTomorrow~MinTemp+MaxTemp+Rainfall+Sunshine+Evaporation, data=training_w, kernel='linear')
    
    model_svm1 # 4814
    model_svm2 # 5254


# 5. 분류모델 평가 
    pred1 <- predict(model_svm1, testing_w)
    pred2 <- predict(model_svm2, testing_w)
    
    
    # kernel=radial 방식
        table(pred1, testing_w$RainTomorrow)
        # pred1   No  Yes
        #   No  3925  801
        #   Yes  177  388
        (3925+388)/nrow(testing_w)  # 0.8151578
    
    # kernel=linear 방식
        table(pred2, testing_w$RainTomorrow)
        # pred2   No  Yes
        #   No  4034  997
        #   Yes   68  192
        (4034+192)/nrow(testing_w)  # 0.7987148
    

        
        
        
## 문2) 위 weatherAUS 데이터 셋을 이용하여 NB, DT, RM, SVM 알고리즘을 적용하여 분류정확도를 비교하시오.
        
    weatherAUS = read.csv(file.choose())	#weatherAUS.csv
    weatherAUS = weatherAUS[ ,c(-1,-2, -22, -23)] # 칼럼 제외 
    str(weatherAUS)
    
    set.seed(415)
    idx = sample(1:nrow(weatherAUS), 0.7*nrow(weatherAUS))
    training_w = weatherAUS[idx, ]
    testing_w  = weatherAUS[-idx, ]
    
    
    training_w = na.omit(training_w)
    testing_w = na.omit(testing_w)
    
    str(training_w)
    testing_w
    
    
    ### 모형 별 시도.
    
    ## NB
        model_nb <- naiveBayes(RainTomorrow~., data=training_w)
        pred_nb <- predict(model_nb, testing_w)
        t <- table(pred_nb, testing_w$RainTomorrow)
        sum(diag(t))/nrow(testing_w)
        # 0.8111888
        
    ## DT
        library(rpart)
        model_dt <- rpart(RainTomorrow~., data=training_w)
        pred_dt <- predict(model_dt, testing_w, type="class")    
        t <- table(pred_dt, testing_w$RainTomorrow)
        sum(diag(t))/nrow(testing_w)
        # 0.8365148
    
        library(rpart.plot)
        library('rattle')
        prp(model_dt)
        fancyRpartPlot(model_dt)
        
        
    ## RM
        library(randomForest)
        r <- numeric()
        for(i in 2:5){
            cat('mtry = ', i)
            model_rm <- randomForest(RainTomorrow~., data=training_w, mtree=500, mtry=i, na.action=na.omit)
            pred_rm <- predict(model_rm, testing_w)
            t <-table(pred_rm, testing_w$RainTomorrow)
            r[i-1] <- sum(diag(t))
        }
        
        which(r==max(r))  # 3 --> mtry : 4일때 최대!
        max(r)/nrow(testing_w)
        # 0.8561709
    
        
    
    ## SVM
        
    model_r <- svm(RainTomorrow~MinTemp+MaxTemp+Rainfall+Sunshine+Evaporation, data=training_w, kernel='radial')
    model_s <- svm(RainTomorrow~MinTemp+MaxTemp+Rainfall+Sunshine+Evaporation, data=training_w, kernel='sigmoid')    
    model_l <- svm(RainTomorrow~MinTemp+MaxTemp+Rainfall+Sunshine+Evaporation, data=training_w, kernel='linear')
    model_p <- svm(RainTomorrow~MinTemp+MaxTemp+Rainfall+Sunshine+Evaporation, data=training_w, kernel='polynomial')
    
    pred_r <- predict(model_r, testing_w)
    pred_s <- predict(model_s, testing_w)
    pred_l <- predict(model_l, testing_w)
    pred_p <-predict(model_p, testing_w)
    
    
    t_r <- table(pred_r, testing_w$RainTomorrow)
    t_s <- table(pred_s, testing_w$RainTomorrow)
    t_l <- table(pred_l, testing_w$RainTomorrow)
    t_p <- table(pred_p, testing_w$RainTomorrow)
    
    
    sum(diag(t_r))/nrow(testing_w)  # 0.8151578
    sum(diag(t_s))/nrow(testing_w)  # 0.7338877
    sum(diag(t_l))/nrow(testing_w)  # 0.7987148
    sum(diag(t_p))/nrow(testing_w)  # 0.8081648
    
    
    
    
    ## 결과(정분류율)
    
        # NB : 0.8111888
        # DT : 0.8365148
        # RM : 0.8561709(mtree:500, mtry:4)
        # SVM : 0.8151578(radial)
    
        ## mtree:500, mtry:4인 RM이 가장 높은 정분류율을 나타낸다.
    
    
    