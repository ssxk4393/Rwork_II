##########################
## 제3-3장 DT 연습문제 
##########################

# 문) Spam 메시지 데이터 셋을 이용하여 DT 분류모델을 생성하고.

    # 정분류율, 오분류율, 정확률을 구하시오. 
    
    # 실습 데이터 가져오기
    sms_data = read.csv(file.choose()) # sms_spam_tm.csv
    dim(sms_data) # [1] 5558(row) 6824(word)
    sms_data$sms_type
    str(sms_data)
    
    sms_data <- sms_data[,-1]
    
    # 1. train과 test 데이터 셋 생성 (7:3)
    
        idx <- sample(1:nrow(sms_data), nrow(sms_data)*0.7)
        train_sms <- sms_data[idx,]
        test_sms <- sms_data[-idx,]    
        
    # 2. model 생성 - train_sms
    
        model_sms <- rpart(sms_type~., data=train_sms)
        
        
    # 3. 예측치 생성 - test_sms
    
        pred_sms <- predict(model_sms, test_sms, type="class")
        
    # 4. 분류정확도 : 정분류율, 오분류율, 정확률

        table(pred_sms, test_sms$sms_type)
        # pred_sms  ham spam
        #     ham  1428   69
        #     spam   28  143

        library(caret)
        confusionMatrix(pred_sms, test_sms$sms_type, positive = 'spam')                
