# chap05_Recommender







############################
# UBCF 기반 추천시스템 
############################
# - 사용자 기반 추천시스템은 사용자 간의 유사성을 바탕으로 추천하는 방식
# - 예) 영화관람 취향이 나와 가장 유사한 사용자들 중에서 Ton N 선정    

install.packages('recommenderlab')
library(recommenderlab)


##############################
##  1. 영화 추천 시스템 예
##############################

    # 첫번째 칼럼 : 사용자
    # 두번째 칼럼 ~ 여섯번째 칼럼 : 영화 평점(5점 척도)
    movie <- read.csv(file.choose(), header = T) # movie.csv
    movie # user movie1 movie2 movie3 movie4 movie5
    
    
    # ## 내가 그냥 해본거.
    # movie_t <- movie[,2:6]
    # rownames(movie_t) <- movie[,1]
    
    
    
    # 해결과제) 사용자 a는 미관람 영화 movie4와 movie5중 하나를 추천 받고자 한다. 
    
    # 1. 사용자별 영화 취향 유사도 평가 
        
        # 1) 전치행렬 : movie(3) X user(5) 
    
            # 3개 칼럼 제외(user, movie4, movie5)  a 기준 movie4, movie5를 아직 안봤으니까.
            movie_t <- t(movie[,-c(1,5,6)]) 
            movie_t # movie(3) x user(5)
            class(movie_t) # "matrix"
            colnames(movie_t) <- c("a","b","c","d","e") 
            movie_t
    
        # 2) 상관계수 이용 사용자별 영화 취향 유사도 평가 
            cor(movie_t) # a사용자 기준 세번째(c)와 네번째(d) 사용자 가장 높음 


    # 2. 추천모델에 맞게 data 구조변경 
    
        # 1) data 가져오기 
            movie <- read.csv(file.choose(), header = T) # movie.csv
            movie
        
        # 2) UBCF(User based CF)의 데이터 구성 
            # - user(row) x item(column) = rating(등급, 시청률)
            
            library(reshape2)
            
            # realRatinMatrix를 사용하려면 1:N을 1:1관계로 만들어 줘야 한다. ID당 1개의 아이템!
            # 그래서 long포맷으로 만들어 주는 것! 
            movie_long <- melt(id=1, movie) # movie(원본 data 구조변경)  id=1은 첫번째 컬럼을 기준으로 하겠다는 의미
            movie_long 
            
            str(movie_long) # 'data.frame':	25 obs. of  3 variables:
            
            # column명 수정 : 사용자(row) X item(column) = rating
            names(movie_long) <- c('user','movie','rating')
            
            # 별점(rating) 0 제외 
            movie_long <- subset(movie_long, rating != 0)
            movie_long
            length(movie_long$rating) # 21(4개 제거됨)


    # 3. 추천 모델 생성을 위한 객체 생성 : recommenderlab 패키지에서 제공하는 class  
            
        movie_real <- as(movie_long, "realRatingMatrix") # 5명 사용자의 영화별점 데이터 
        dim(movie_real) # 5 5
        movie_real 
        
        
        # realRatingMatrix의 내용을 보려면 matrix형태로 변환하고 봐야한다.
        # 객체 내용 : user별로 movie의 rating 표시 
        as(movie_real, "matrix")  # 2단계에서 0으로 제거된 부분은 NA 처리됨 

        
        
    # 4. 추천 모델 생성 
        
        # 1) data set 생성 : train Set : b~e, recomm Target : a  
            trainSet <- movie_real[c(2:5),] # 학습데이터 생성  
            trainSet # 4 x 5 rating matrix of class ‘realRatingMatrix’ with 5 ratings.
            as(trainSet, 'matrix')
            
        # 2) 추천 대상자 선정 : a 사용자  
            recommTarget <- movie_real[1,] # a 사용자 대상 영화 추천 예측  
            recommTarget # 1 x 5 rating matrix of class ‘realRatingMatrix’ with 3 ratings.
            as(recommTarget, 'matrix')
        
            
        # 3) 추천 모델생성 : 유사도 코사인 방식 
            ?Recommender
            # Recommender(data, method, parameter=NULL)
            recom_model <- Recommender(trainSet, method="UBCF", parameter ="Pearson") # 유사도 : 상관계수 방식
            recom_model # learned using 4 users.


            
    # 5. 영화 추천 : 사용자a 에게 영화추천(예측치)
            
        # 1) 예측치 생성 : n = ‘topNList’ : 추천 대상자에게 추천할 item 수
            recomm_list <- predict(recom_model, recommTarget, n=2) # 사용자 당 2개씩 추천
        
        # 2) 추천받은 리스트 보기
            recom_result <- as(recomm_list,"list")
            recom_result # 전체사용자별(model) ->  a 사용자 추천 예측(predict)



            
            
            
            
#################################
# 2. 음식 추천 시스템 예
#################################
            
# 해결과제) 음식주문 패턴에 따른 음식 추천 
            
    # 1. 데이터 셋 가져오기 
        gloseri <- read.csv(file.choose(), header = T) # gloseri.csv
        head(gloseri)
        
    # 2. realReatingMatrix 변환 
        realData <- as(gloseri, 'realRatingMatrix')
        as(realData, 'matrix') # user*item(7*5)  
        
        
    # 3. 훈련데이터 생성 
        trainIdx <- sample(1:7, 6)
        trainSet <- realData[trainIdx]
        trainSet # 6 x 5 rating matrix of class ‘realRatingMatrix’ with 19 ratings.
        as(trainSet, 'matrix') 
        
        # - 3가지 이상 음식을 주문한 사용자만 학습데이터 선정  
        trainSet2 <- trainSet[rowCounts(trainSet) >= 3 ]
        as(trainSet2, 'matrix') # 전체 6명중 5명 선정 
        
        
    # 4. 추천 모델 생성 : 사용자기반 협업필터링, 코사인 유사도 
        recomm_model_gl <- Recommender(trainSet2, method="UBCF", parameter ="Cosine")
        recomm_model_gl # learned using 5 users.
        
        recomm_target_gl <- realData[-trainIdx ]  # 1 x 5 rating matrix
        recomm_target_gl # 1 x 5 rating matrix of class ‘binaryRatingMatrix’ with 3 ratings.
        as(recomm_target_gl, 'matrix')
        
        # - 추천받은 음식수 지정 : TopN = 2
        recommandList <- predict(recomm_model_gl, recomm_target_gl, n=2) # 사용자 당 Top2개 추천 
        
        # 추천받은 리스트 보기 
        recomm_result <- as(recommandList, 'list')
        recomm_result
            
        
        
        
        

        
        
############################
## IBCF 기반 추천시스템 
############################
        
# - item 기반 추천시스템은 아이템 간의 유사성을 바탕으로 추천하는 방식
# - 예) a상품을 구매하는 사람은 b상품을 함께 구매하는 경향이 있는 경우   
# - IBCF 방식은 횟수 보다는 주문(구매) 여부가 유사도를 계산하는데 더 중요   
# - 주문 여부는 이진화 함수 이용 : binarize() -> (NA->FALSE, 1 이상 ->TRUE)
# - 이진화 데이터는 Jaccard 방식으로 유사도 계산
#  
# 해결과제) 음식주문 횟수에 상관없이 주문 여부에 따른 item 추천  
        
        library(recommenderlab)
        
        realData
        realData_b <- binarize(realData, minRating=1) # minRating : 몇번 이상을 True로 볼것인가. 
        as(realData_b, 'matrix') # FALSE(NA)/TRUE(숫자) 이진화 
        
        #학습데이터/검정 데이터 구성  
        trainIdx <- sample(1:7, 6) # 7명중 6명 랜덤 선정 
        trainSet <- realData_b[trainIdx ]  
        trainSet # 6 x 5 rating matrix of class ‘binaryRatingMatrix’ with 19 ratings.
        as(trainSet, 'matrix') 
        
        # 추천대상자 선정 
        recomm_target_gl <- realData_b[-trainIdx ]  # 1 x 5 rating matrix
        recomm_target_gl # 1 x 5 rating matrix of class ‘binaryRatingMatrix’ with 3 ratings.
        as(recomm_target_gl, 'matrix')
        
        
        # model 생성 : Jaccard 유사도 적용  
        recomm_model_gl <- Recommender(trainSet, method="IBCF", parameter ="Jaccard") # Jaccard : 대표적인 binary 유사도
        recomm_model_gl # learned using 6 users.
        
        # - 추천받은 음식수 지정 : TopN = 2
        recommandList <- predict(recomm_model_gl, recomm_target_gl, n=2)
        
        # 추천받은 리스트 보기 
        recomm_result <- as(recommandList, 'list')
        recomm_result
        
        
        
        
        
        
#################################
# UBCF 기반 영화 추천 모델 실습 
#################################
    library(recommenderlab)
    
    # 해결과제) 사용자별 영화 평가 점수 데이터셋으로 추천 모델 생성  
    
    data(MovieLense) # recommenderlab 패키지에서 제공 
    MovieLense # 943 x 1664 rating matrix of class ‘realRatingMatrix’ with 99392 ratings.
    # user : 943 x item : 1664
    
    # 사용자 6명의 5개 영화 평점 행렬구조   
    head(as(MovieLense, "matrix")[, c(1:5)])
    
    
    help(MovieLense)
    # 개인 사용자별 영화 평점 리스트 구조  
    head(as(MovieLense[1,], "list")[[1]]) # 1번 사용자의 6개 영화 평점 보기
    head(as(MovieLense[2,], "list")[[1]]) # 2번 사용자의 6개 영화 평점 보기
    
    
    # 전체 사용자별 영화평점 matrix : class 멤버 이용  
    str(MovieLense) # class 구조 보기 
    MovieLense@data # data 보기 
    getRatingMatrix(MovieLense) # 사용자 별 영화평점을 matrix로 제공  

        
    # 각 사용자별 행 단위 합계 : 사용자별 본 영화 수 합계 
    rowCounts(MovieLense) # 943명 사용자별 본 영화 수 보기 
    
    table(rowCounts(MovieLense) >= 50)
    
    head(rowCounts(MovieLense) >= 50)
    
    head(rowCounts(MovieLense))
    
    table(colCounts(MovieLense) >= 100)
    
    # 이미지 시각화로 전체 view 보기 
    image(MovieLense[1:100,1:100]) # 이미지 개괄확인 : 1 row는 모두 채워짐  
    image(sample(MovieLense, 500), main = "Raw ratings")

    rowSums(MovieLense[1,]) 
    rowSums(MovieLense)
    
    # 과적합 문제 해결을 위해서 영화의 행과 열수 지정 
    # - 사용자(user)별 영화 관림 횟수가 50번 이상 사용자(user) 선정 
    # - 영화(item)별 본 사람의 수가 100명 이상인 영화(item) 선정 
        ratings_movies = MovieLense[ rowCounts(MovieLense) >= 50, colCounts(MovieLense) >= 100]
    
    # rowCounts() : 사용자 필터링, colCounts() : 영화 필터링 -> recommenderlab 패키지에서 제공
        ratings_movies # 565 x 336 rating matrix of class 
    
    
    #  ratings_movies 에서 랜덤으로 training 데이터셋을 추출
        idx <-sample(1:nrow(ratings_movies), nrow(ratings_movies)*0.9)
        trainSet <-ratings_movies[idx, ] # 90%
        recomm_target <-ratings_movies[-idx, ] # 10%
        dim(trainSet) # 508 336
        dim(recomm_target) # 57 336
    
    
    # UBCF 방식 : 사용자 기반 협업 필터링, 코사인 유사도   
    recomm_model <- Recommender(trainSet, method="UBCF", parameter ="Cosine") # 508명 대상 모델 생성 
    recomm_list <- predict(recomm_model, recomm_target, n=5) # 5개씩 영화 추천 
    recomm_list
    
    
    # 57명 사용자에게 추천영화 5개씩 제공  
    as(recomm_list, "list")
    
    length(as(recomm_list, "list")) 
        
        
    
    
    
    

#####################################
# IBCF 기반 영화 추천 모델 실습
#####################################
    
    library(recommenderlab)
    
    # 해결과제) 아이템별 영화 평가 점수 데이터셋((binary data)으로 추천 모델 생성  
    
        data(MovieLense) # recommenderlab 패키지에서 제공 
        MovieLense # 943 x 1664 rating matrix of class ‘realRatingMatrix’ with 99392 ratings.
    
        
    # 과적합 문제 해결을 위해서 영화의 행과 열수 지정 : 50편 이상 영화를 본 사용자, 100번 이상 선택된 영화 
        ratings_movies = MovieLense[ rowCounts(MovieLense) >= 50, colCounts(MovieLense) >= 100]
        ratings_movies # 560 x 336 rating matrix of class 
        as(ratings_movies,'matrix')
        
    # 이진화 변환 
        ratings_movies_b <- binarize(ratings_movies, minRating=1) # minRating : 몇번 이상을 True로 볼것인가.
        as(ratings_movies_b, 'matrix') # FALSE(NA)/TRUE(숫자) 이진화 
    
    #  ratings_movies 에서 랜덤으로 training 데이터셋을 추출
        idx <- sample(1:nrow(ratings_movies), nrow(ratings_movies)*0.9)
        trainSet <- ratings_movies_b[idx, ] # 90%
        recomm_target <- ratings_movies_b[-idx, ] # 10%
        dim(trainSet) # 508 336
        dim(recomm_target) # 57 336
    
    
    # IBCF 방식 : 아이템 기반 협업 필터링, Jaccard 유사도   
        recomm_model <- Recommender(trainSet, method="IBCF", parameter ="Jaccard") # 508명 대상 모델 생성 
        recomm_list <- predict(recomm_model, recomm_target, n=5) # 57명 사용자에게 5개씩 영화 추천 
        recomm_list  # Recommendations as ‘topNList’ with n = 5 for 57 users. 
    
        
    # 57명 사용자에게 추천영화 5개씩 제공  
        head(as(recomm_list, "list")) # 6명 추천영화 보기 
    
    
        

        
        
        
#####################################
# 추천 모델 평가(evaluation) 
#####################################
        
    # 해결과제) 추천 모델 평가(최적의 유사도와 이웃의 크기를 갖는 추천 모델 생성과 분류정확도)  
    
    # 1. 추천시스템 데이터 생성 : rating matrix 생성  
        data(MovieLense) # recommenderlab 패키지에서 제공 
        MovieLense # 943 x 1664 rating matrix of class ‘realRatingMatrix’ with 99392 ratings.
        
        # 과적합을 위해서 영화의 행과 열수 지정 
        ratings_movies = MovieLense[ rowCounts(MovieLense) >= 50, colCounts(MovieLense) >= 100]
        # rowCounts(); colCounts() - recommenderlab 패키지에서 제공
        ratings_movies # 566 x 336 rating matrix of class 
    
        
    # 2. 추천 정확도 평가를 위한 시뮬레이션 생성 :  추천 정확도 측정을 위한 스키마  
        eval_scheme = evaluationScheme(data=ratings_movies,
                                       method="split", train=0.8, given=15,
                                       goodRating=5, k=5)
        # data=ratings_movies : 추천시스템 데이터(rating matrix)
        # method="split" : 지정한 비율로 분할하여 검정 ("split, cross-validation, bootstrap)
        # train=0.8 : 학습 데이터 셋 80% 비율("split", "bootstrap") 
        # given=15 : 사용자 당 학습할 item 수
        # goodRating=5 : 사용자가 5명 이상인 item을 평가 대상으로 지정(Neighbor로 지정하는 기준) 
        # k=5 : 시뮬레이션 횟수(5회 시뮬레이션 결과의 평균)  
        
        eval_scheme
    
        
    # 3. 시뮬레이션에 사용할 협업필터링 방식과 유사도 지정 
        ubcf_method <- list("UBCF_cosine" = list(name="UBCF", param=list(method="Cosine")),
                            "UBCF_pearson" = list(name="UBCF", param=list(method="Pearson")) )
        ubcf_method
        
        
    # 4. 추천시스템 평가 및 결과 
        result <- evaluate(eval_scheme, ubcf_method, type="topNList", n=c(1,3,5)) # Top10에서 1,3,5번째 출력 
        result # 평가 객체 정보 
        
        # k=5회 시뮬레이션 결과의 평균 Top10을 대상으로 1,3,5번째 평가 결과 출력 
        avg(result) # recommenderlab 패키지 제공 함수 : 혼돈matrix 정보 제공 
        
        # confusion matrix
        #  참  부정(TN)   거짓긍정(FP)
        #  거짓부정(FN)   참  긍정(TP)
        # 정확률(precision)
        # 재현율(recall)c
    
    