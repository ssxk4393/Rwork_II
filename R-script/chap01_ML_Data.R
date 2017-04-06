# chap01_ML_Data




# 텍스트 마이닝(Text Mining)
# - 정형화 되지 않은 텍스트를 대상으로 유용한 정보를 찾는 기술
# - 토픽분석, 연관어 분석, 감성분석 등
# - 텍스트 마이닝에서 가장 중요한 것은 형태소 분석
# - 형태소 분석 : 문장을 분해 가능한 최소한의 단위로 분리하는 작업 

# 1. sms_spam.csv 가져오기 - stringsAsFactors = FALSE : factor형으로 읽지 않음 
    sms_data <- read.csv('C:/NCS/Rwork_II/data/sms_spam.csv', stringsAsFactors = FALSE)
    str(sms_data)
    #'data.frame':	5558 obs. of  2 variables:
    # $ type: chr  "ham" "ham" "ham" "spam" ...
    # $ text: chr
    
    names(sms_data) <- c('type', 'text')
    
    

# 2. 분석을 위한 데이터 처리 : sms 문장을 단어 단위로 생성 
    
    install.packages('tm')
    library(tm)
    install.packages('SnowballC')
    library(SnowballC) # stemDocument()함수 제공
    
    sms_corpus = Corpus(VectorSource(sms_data$text)) # 1) 말뭉치 생성(vector -> corpus 변환) 
    inspect(sms_corpus[1])      # Hope you are having a good week. Just checking in
    inspect(sms_corpus[4])      # complimentary 4 STAR Ibiza Holiday or 짙10,000 
                                # cash needs your URGENT collection. 09066364349 
                                # NOW from Landline not to lose out! Box434SK38WP150PPM18+
    
    # 문장 하나하나 전처리!!
    sms_corpus = tm_map(sms_corpus, content_transformer(tolower))  # 2) 소문자 변경
    sms_corpus = tm_map(sms_corpus, removeNumbers) # 3) 숫자 제거 
    sms_corpus = tm_map(sms_corpus, removePunctuation) # 4) 문장부호(콤마 등) 제거 
    sms_corpus = tm_map(sms_corpus, removeWords, stopwords("SMART")) # 5) stopwords(the, of, and 등) 제거  
        # stopwords("english") # stopwords("SMART")
    sms_corpus = tm_map(sms_corpus, stripWhitespace) # 6) 여러 공백 제거(stopword 자리 공백 제거)   
    sms_corpus = tm_map(sms_corpus, stemDocument) # 7) 유사 단어 어근 처리 
    sms_corpus = tm_map(sms_corpus, stripWhitespace) # 8) 여러 공백 제거(어근 처리 공백 제거)   
    sms_dtm = DocumentTermMatrix(sms_corpus) # 9) 문서와 단어 집계표 작성
    
    
    t(sms_dtm) # <<TermDocumentMatrix (terms: 6822, documents: 5558)
    
    sms_mt <- as.matrix(t(sms_dtm)) # 행렬 변경 
    sms_mt
    
    # 행 단위(단어수) 합계 -> 내림차순 정렬   
    rsum <- sort(rowSums(sms_mt), decreasing=TRUE) 
    
    # vector에서 칼럼명(단어명) 추출 
    myNames <- names(rsum) # rsum 변수에서 칼럼명(단어이름) 추출  
    myNames # 단어명 
    
    # 단어와 빈도수를 이용하여 df 생성 
    df <- data.frame(word=myNames, freq=rsum) 
    head(df) # word freq
    
    
    # 단어 구름 시각화 
    library(wordcloud) # 단어 시각화(빈도수에 따라 크기 달라짐)
    
    # 색상 12가지 적용 
    pal <- brewer.pal(12,"Paired") # RColorBrewer 패키지 제공(wordcloud 설치 시 함께 설치됨)
    
    # wordCloud(단어(word), 빈도수(v), 기타 속성)
    # random.order=T : 위치 랜덤 여부(F 권장) , rot.per=.1 : 회전수, colors : 색상, family : 글꼴 적용 
    wordcloud(df$word, df$freq, min.freq=2, random.order=F, scale=c(4,0.7),
              rot.per=.1, colors=pal, family="malgun") 


    
########################################
### 단어수 조정 - 단어길이, 가중치 적용 
########################################
    
    sms_dtm = DocumentTermMatrix(sms_corpus) # 9)번 과정
    sms_dtm # (documents: 5558, terms: 6767)
    
    # 1. 단어길이 : 1 ~ 8
        sms_dtm1 = DocumentTermMatrix(sms_corpus,
                                      control = list(wordLengths= c(1,8)))
        sms_dtm1 # (documents: 5558, terms: 6422)
        
        # 전체 단어 접근 방법
        sms_dtm1$dimnames$Terms[1:5]
        
        # DTM -> TDM 변경 
        t(sms_dtm1) # (terms: 6156, documents: 5558)
        sms_tdm1 <- as.matrix(t(sms_dtm1))
        str(sms_tdm1)
        table(sms_tdm1[2,])
        #    0    1    2    3 
        # 5334  211   12    1           # 빈도수에 의한 가중치
        
    
    # 2. 가중치 : 단어출현빈도로 가중치(비율) 적용 
        # 출현빈도가 많은 단어는 가중치 조정 
        ?DocumentTermMatrix
        sms_dtm2 = DocumentTermMatrix(sms_corpus,
                                      control = list(wordLengths= c(1,8),  weighting = weightTfIdf))
        sms_dtm2
        
        # DTM -> TDM 변경 
        sms_tdm2 <- as.matrix(t(sms_dtm2))
        str(sms_tdm2)
        table(sms_tdm2[1,])

        rsum2 <- sort(rowSums(sms_tdm2), decreasing = T)
    
        myNames2 <- names(rsum2)
        df2 <- data.frame(word=myNames2, freq=rsum2)
        
        wordcloud(df2$word, df2$freq, min.freq=2, random.order=F, scale=c(4,0.7),
                  rot.per=.1, colors=pal, family="malgun") 
        
        
        
        
        
########################################
### DTM 객체 대상 단어 검색 
########################################
        
    # 1) 단어의 길이로 검색
        
        terms <- sms_dtm1$dimnames$Terms
        terms        
        
        library(stringr)
        result <- terms[str_length(terms) >= 5 & str_length(terms) <= 6]
        result
        length(result)       # 2506
        
    # 2) 단어의 출현빈도수로 검색
        
        result2 <- findFreqTerms(sms_dtm1, lowfreq = 40)
        length(result2)     # 156
        
        
        
        

        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
###############################
## chap01_ML_Data(2)
###############################
# 기계학습을 위한 데이터 셋 생성 
    
    # 1. sms_spam.csv 가져오기 - stringsAsFactors = FALSE : factor형으로 읽지 않음 
        sms_data <- read.csv('C:/NCS/Rwork_II/data/sms_spam.csv', stringsAsFactors = FALSE)
        str(sms_data)
        
        names(sms_data) <- c('type', 'text')
    
    # 2. 분석을 위한 데이터 처리 : sms 문장을 단어 단위로 생성해야 한다. 
        library(tm)
        library(SnowballC) # stemDocument()함수 제공 
        sms_corpus = Corpus(VectorSource(sms_data$text)) # 1) 말뭉치 생성(vector -> corpus 변환) 
        sms_corpus = tm_map(sms_corpus, content_transformer(tolower))  # 2) 소문자 변경
        sms_corpus = tm_map(sms_corpus, removeNumbers) # 3) 숫자 제거 
        sms_corpus = tm_map(sms_corpus, removePunctuation) # 4) 문장부호(콤마 등) 제거 
        sms_corpus = tm_map(sms_corpus, removeWords, stopwords("SMART")) # 5) stopwords(the, of, and 등) 제거  
        sms_corpus = tm_map(sms_corpus, stripWhitespace) # 6) 여러 공백 제거(stopword 자리 공백 제거)   
        sms_corpus = tm_map(sms_corpus, stemDocument) # 7) 유사 단어 어근 처리 
        sms_corpus = tm_map(sms_corpus, stripWhitespace) # 8) 여러 공백 제거(어근 처리 공백 제거)   
    
    
################################
### DTM 생성 -> X변수 생성 
################################
    
    # 1. DTM 생성 -> 단어길이 : 2 ~ 8, 출현빈도수로 가중치 
        sms_dtm = DocumentTermMatrix(sms_corpus,
                                     control = list(wordLengths= c(2,8),  weighting = weightTfIdf)) 
        sms_dtm 
    
    
    # 2. DTM -> 원본 변경 
        sms_dtm_mat <- as.matrix(sms_dtm)
        sms_dtm_mat[1,1:10]
    
    
    # 3. 가중치 -> Factor('YES', 'NO')
        convert_Func <- function(x){
            x <- ifelse(x > 0, 1, 0)
            f <- factor(x, levels = c(0,1), labels = c('NO','YES'))
        }
    
    # 4. sms_dtm 사용자 함수 적용 
        sms_dtm_mat_text <- apply(sms_dtm_mat, 2, convert_Func)
        
        sms_dtm_mat_text[,1] # 첫번째 단어  
        table(sms_dtm_mat_text[,1])
        
        
    # 5. type과 sms_dtm_mat_text -> data.frame
        
        str(sms_data$type)
        str(sms_dtm_mat_text)
        
        class(sms_dtm_mat_text)
        class(sms_data$type)
        mode(sms_dtm_mat_text)
        mode(sms_data$type)
        
        
        
        sms_data_df <- data.frame(sms_data$type, sms_dtm_mat_text)
        str(sms_data_df)
        
        
        head(sms_data_df)
        
        
        
        
        