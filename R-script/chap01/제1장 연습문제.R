#################################
## <제1장 연습문제>
#################################

# 문1) acq 데이터 셋을 대상으로 다음과 같이 TDM 객체를 생성하시오.
    # <조건1> 전체 단어의 갯수는 몇개인가 ? 1107
    # <조건2> 최대 단어 길이는 몇개인가 ? 16
    
    data(acq) # corpus 객체 
    str(head(acq))
    
    # 작업절차 : acq -> DATA전처리(2단계 ~ 8단계) -> DTM -> TDM -> ?

    # 1. DATA 전처리(2단계 ~ 8단계)
    # 2. DTM 생성 
    # 3. TDM 생성
    
        acq = tm_map(acq, content_transformer(tolower))
        acq = tm_map(acq, removeNumbers)
        acq = tm_map(acq, removePunctuation)
        acq = tm_map(acq, removeWords, stopwords("SMART"))
        acq = tm_map(acq, stripWhitespace)
        acq = tm_map(acq, stemDocument)
        acq = tm_map(acq, stripWhitespace)
        acq_dtm = DocumentTermMatrix(acq)                

        
        acq_tdm <- t(acq_dtm)
        
        # 최대 단어길이
        max(str_length((acq_dtm$dimnames$Terms)))
        # 16
        

# 문2) crude 데이터 셋을 대상으로 다음과 같이 TDM 객체를 생성하시오.
    # <조건1> 단어 길이 : 1 ~ 8
    # <조건2> 가중치 적용 : 출현빈도수의 비율 
    # <조건3> 위 조건의 결과를 대상으로 단어수는 몇개인가 ?  651
    
    data(crude)
    class(crude)
    
    # 1. DATA전처리(2단계 ~ 8단계)
        crude = tm_map(crude, content_transformer(tolower))
        crude = tm_map(crude, removeNumbers)
        crude = tm_map(crude, removePunctuation)
        crude = tm_map(crude, removeWords, stopwords("SMART"))
        crude = tm_map(crude, stripWhitespace)
        crude = tm_map(crude, stemDocument)
        crude = tm_map(crude, stripWhitespace)
    
    # 2. DTM 생성 
        crude_dtm = DocumentTermMatrix(crude, control = list(wordLengths= c(1,8), weighting = weightTfIdf))
        # (documents: 20, terms: 651)
        
        
    # 3. TDM 생성 
        crude_tdm <- t(crude_dtm)
        crude_tdm <- as.matrix(crude_tdm)
    # 단어 갯수 : 651개 
        str(crude_tdm)
    
    