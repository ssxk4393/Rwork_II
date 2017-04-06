# chap01_ML_Data

# 텍스트 마이닝(Text Mining)
# - 정형화 되지 않은 텍스트를 대상으로 유용한 정보를 찾는 기술
# - 토픽분석, 연관어 분석, 감성분석 등
# - 텍스트 마이닝에서 가장 중요한 것은 형태소 분석
# - 형태소 분석 : 문장을 분해 가능한 최소한의 단위로 분리하는 작업 

# 1. sms_spam.csv 가져오기 - stringsAsFactors = FALSE : factor형으로 읽지 않음 
sms_data <- read.csv('C:/Rwork-II/data/sms_spam.csv', stringsAsFactors = FALSE)
str(sms_data)
#'data.frame':	5558 obs. of  2 variables:
# $ type: chr  "ham" "ham" "ham" "spam" ...
# $ text: chr

# 2. 분석을 위한 데이터 처리 : sms 문장을 단어 단위로 생성해야 한다. 
library(tm)
#install.packages('SnowballC')
library(SnowballC) # stemDocument()함수 제공

sms_corpus = Corpus(VectorSource(sms_data$text)) # 1) 말뭉치 생성(vector -> corpus 변환) 
sms_corpus
str(sms_corpus) # List of 5558
str(sms_corpus[1])
# Corpus 객체의 평서문(PlainTextDocument) 보기 
inspect(head(sms_corpus)) # 앞부분 6개 보기 
#[[6]]
#Aiya we discuss later lar... Pick u up at 4 is it?

sms_corpus[[6]]
# Aiya we discuss later lar... Pick u up at 4 is it?
# sms_corpus[[6]]$content -> versio : 3.3.2

sms_corpus[[1]]
# "Hope you are having a good week. Just checking in"
sms_corpus[[4]]
# [1] "complimentary 4 STAR Ibiza Holiday or 짙10,000 cash needs your 

sms_corpus = tm_map(sms_corpus, tolower)  # 2) 소문자 변경
sms_corpus[[1]]
# [1] "hope you are having a good week. just checking in"

sms_corpus = tm_map(sms_corpus, removeNumbers) # 3) 숫자 제거
sms_corpus[[4]]

sms_corpus = tm_map(sms_corpus, removePunctuation) # 4) 문장부호(콤마 등) 제거 
sms_corpus[[1]] # . 제거 -> hope you are having a good week just checking in

# english vs SMART
stopwords('english') # 174
stopwords("SMART") # 571

sms_corpus = tm_map(sms_corpus, removeWords, stopwords("SMART")) # 5) stopwords(the, of, and 등) 제거  
sms_corpus[[1]] # hope     good week  checking 

sms_corpus = tm_map(sms_corpus, stripWhitespace) # 6) 여러 공백 제거(stopword 자리 공백 제거)   
sms_corpus[[1]] # hope good week checking

# [실습] 유사 단어 어근 처리 
text <- 'love loving loved'
cor <- Corpus(VectorSource(text))
tm_cor <- tm_map(cor, stemDocument)
tm_cor[[1]] #  "love love love"

sms_corpus = tm_map(sms_corpus, stemDocument) # 7) 유사 단어 어근 처리 
sms_corpus = tm_map(sms_corpus, stripWhitespace) # 8) 여러 공백 제거(어근 처리 공백 제거)   
sms_corpus[[1]] # hope good week chec

sms_dtm = DocumentTermMatrix(sms_corpus) # 9) 문서와 단어 집계표 작성
sms_dtm
# <<DocumentTermMatrix (documents: 5558, terms: 6822)>>
# Maximal term length: 40 <- 단어 최대 길이 
# Weighting          : term frequency (tf) <- 가중치 : 단어 빈도수 

# DTM 보기 
as.matrix(sms_dtm) # matrix 형변환 

# DTM -> TDM(전치행렬 적용)
sms_tdm <- t(sms_dtm) # <<TermDocumentMatrix (terms: 6822, documents: 5558)

sms_mt <- as.matrix(sms_tdm) # 행렬 변경 
sms_mt
str(sms_mt)
#num [1:6822, 1:5558] 0 0 0 0 0 0 0 0 0 0 ...
#- attr(*, "dimnames")=List of 2
#..$ Terms: chr [1:6822] "aah" "aaniy" "aaooooright" "aathilov" ...
#..$ Docs : chr [1:5558] "1" "2" "3" "4" ...

# 전체 단어보기 
sms_tdm$dimnames$Terms[1:10]

# 1행 : aah(단어), 빈도수 : 3개 -> 희소행렬 
table(sms_mt[1,])
#   0    1 
#5555    3

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
### 단어수 조정 : 단어길이, 가중치 적용 
########################################
sms_dtm = DocumentTermMatrix(sms_corpus) # 9)
sms_dtm 
# documents: 5558, terms: 6822)
# Maximal term length: 40

# 1. 단어길이 : 1 ~ 8,
sms_dtm1 = DocumentTermMatrix(sms_corpus,
                control = list(wordLengths= c(1,8)))
sms_dtm1
#A document-term matrix (5558 documents, 6156 terms)
6822 - 6156 # 666 축소 

# DTM -> TDM 변경 
t(sms_dtm1) # (terms: 6156, documents: 5558)
sms_tdm1 <- as.matrix(t(sms_dtm1))
sms_tdm1
str(sms_tdm1)

table(sms_tdm1[2 ,]) # "aah"
#    0    1 
# 5555    3

# 2. 가중치 : 단어출현빈도의 비율로 가중치 조정
sms_dtm2 = DocumentTermMatrix(sms_corpus,
          control = list(wordLengths= c(1,8), weighting = weightTfIdf))
sms_dtm2
# Weighting  : term frequency - inverse document frequency (normalized) (tf-idf)

# DTM -> TDM 변경 
sms_tdm2 <- as.matrix(t(sms_dtm2)) # TDM -> 원본 Matrix
str(sms_tdm2) # "aah"

table(sms_tdm2[2, ]) # 가중치가 비율로 수정됨(빈도수 증가에 따른 비율 증가) 
#               0 1.20615417983104 2.17107752369588 2.71384690461985 
# 5555                1                1                1


# 3. 단어길이 : 4~8, 가중치 : 단어출현빈도로 가중치(비율)
# 출현빈도가 많은 단어는 가중치 조정 
sms_dtm3 = DocumentTermMatrix(sms_corpus,
           control = list(wordLengths= c(4,8), weighting = weightTfIdf)) # wordLengths= c(4,Inf) # 4자 이상 
sms_dtm3 # A document-term matrix (5558 documents, 5184 terms)


################################
### DTM 대상 단어 검색 
################################

# 1) 단어 길이로 검색 
terms <- sms_dtm1$dimnames$Terms
terms

# 길이가 5~6자 이상 단어 검색 
library(stringr)
result <- terms[str_length(terms) >= 5 & str_length(terms) >= 6]
result
length(result) # 2578

# 2) 단어의 빈도수로 검색 : 단어 빈도수로 가중치가 만들어진 객체 대상
findFreqTerms(sms_dtm1, lowfreq = 43) # 최소빈도수 : 43 

# 3) show 단어와 연관있는 단어 검색(0.7 피어슨 상관계수)
findAssocs(sms_dtm1, "show", corlimit = 0.3) 
# 단어와 단어의 빈도수 간의 상관분석을 적용하여 지정한 상관계수 이상 단어 검색  

###############################
## chap01_ML_Data(2)
###############################
# 기계학습을 위한 데이터 셋 생성 : 모든 데이터는 컴퓨터가 처리할 수 있는 상태로 변환 

# 1. sms_spam.csv 가져오기 - stringsAsFactors = FALSE : factor형으로 읽지 않음 
sms_data <- read.csv('C:/Rwork-II/data/sms_spam.csv', stringsAsFactors = FALSE)
str(sms_data)
# type : ham/ spam
# text : message 원문 

# 2. 분석을 위한 데이터 처리 : sms 문장을 단어 단위로 생성해야 한다. 
library(tm)
library(SnowballC) # stemDocument함수 제공 
sms_corpus = Corpus(VectorSource(sms_data$text)) # 1) 말뭉치 생성(vector -> corpus 변환) 
sms_corpus = tm_map(sms_corpus, tolower)  # 2) 소문자 변경
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
sms_dtm # (5558 documents, 6156 terms)


# 2. DTM -> 원본 변경 
sms_dtm_mat <- as.matrix(sms_dtm)
sms_dtm_mat


# 3. 가중치 -> Factor('YES', 'NO')
convert_Func <- function(x){
  x <- ifelse(x > 0, 1, 0)
  f <- factor(x, levels = c(0,1), labels = c('NO','YES'))
}

# 4. sms_dtm 사용자 함수 적용 
sms_dtm_mat_text <- apply(sms_dtm_mat, 2, convert_Func) # Term 단위 

sms_dtm_mat_text[,1] # 첫번째 단어  
table(sms_dtm_mat_text[,1])
#  NO  YES 
#5557    1

# 5. DF 객체 생성 
# type, sms_dtm_mat_text

#  1칼럼(y변수) 2칼럼 ~ n(x변수)
# type    sms_dtm_mat_text
sms_data_df <- data.frame(sms_data$type, sms_dtm_mat_text) 
str(sms_data_df)
# 'data.frame':	5558 obs. of  6157 variables:
# $ sms_data.type: Factor w/ 2 levels "ham","spam": 1 1 1 2 2 1 1 1 2 1 ...
# $ aa           : Factor w/ 2 levels "NO","YES": 1 1 1 1 1 1 1 1 1 1 ...


# 6. file 저장 
write.csv(sms_data_df, 'C:\\Rwork-II\\data\\sms_data_test.csv', quote=F, row.names=F)
