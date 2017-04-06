##########################
## 제3-2장 NB 연습문제 
##########################

# 문) Spam 메시지 데이터 셋을 이용하여 NB 분류모델을 생성하고,
# 정분류율, 정확률, 재현율를 구하시오. 

# 실습 데이터 가져오기(TM에서 전처리한 데이터) 
sms_data <- read.csv('C:/NCS/Rwork_II/data/sms_spam_tm.csv')
head(sms_data[,c(1:10)],1) # 1행 10칼럼(단어) 보기 
dim(sms_data) # [1] 5558(row) 6824(word)
sms_data$sms_type

# X 칼럼 제외 
sms_data.df <- sms_data[-1]
head(sms_data.df)
str(sms_data.df) # 5558 obs. of  6823 variables:

# 1. train과 test 데이터 셋 생성 (7:3)

    idx <- sample(1:nrow(sms_data.df), nrow(sms_data.df)*0.7)
    train_sms <- sms_data.df[idx,]
    test_sms <- sms_data.df[-idx,]
    dim(train_sms)
    dim(test_sms)

# 2. model 생성 - train_sms 

    model_sms <- naiveBayes(sms_type~., data=train_sms)
    model_sms
    
# 3. 예측치 생성 - test_sms

    pred <- predict(model_sms, test_sms, type="class")
    
# 4. 분류정확도 : 정분류율, 정밀도, 재현율 

    table(pred, test_sms[,1])
    
    # pred    ham spam
    #   ham  1426   30
    #   spam   10  202
    
    # 정분류율
    (1426+202)/nrow(test_sms)  # 0.9760192
    
    # 정밀도 : 참이라고 한 것 중 참의 수
    1426/(1426+30)  # 0.9793956
    
    # 재현율 : 참인 것 중에 참이라고 한 수
    1426/(1426+10) # 0.9930362
    
    
    
    install.packages('caret')
    library(caret)    
    
    cm <- confusionMatrix(pred, test_sms[,1], positive = 'ham')
    cm$byClass
        
    