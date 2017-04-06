#############################
## 제5장 Recommander 연습문제 
#############################

# 문제2) MovieLense 데이터 셋을 대상으로 IBCF 방식, Jacaord 지표를 이용하여 추천모델를 생성하시오.

        library(recommenderlab)
        data(MovieLense)
        MovieLense      # realRatingMatrix
        
    # <단계1> 사용자(user)별 관람 영화 100편 이상, 영화(item)별 관람자 수 합계 50명 이상 선정 
        
        movie_ch <- MovieLense[rowCounts(MovieLense) >= 100, colCounts(MovieLense) >= 50]
        movie_ch     # 362 x 601
        
    # <단계2> 90%  사용자 샘플링으로 10% 사용자에게 10개씩 영화 추천
        
        idx <- sample(1:nrow(movie_ch), nrow(movie_ch)*0.9)
        train_m <- movie_ch[idx,]
        recom_m <- movie_ch[-idx,]    
            
        train_m # 325 x 601
        recom_m # 37 x 601
        
        
        ij_model <- Recommender(train_m, method='IBCF', parameter='Jacaord')
        ij_recom <- predict(ij_model, recom_m, n=10)    
        ij_recom  # Recommendations as ‘topNList’ with n = 10 for 37 users. 
        
        
    # <단계3> 추천받은 영화중에서 빈도수가 가장 높은 영화 찾기  
        
        ij_list <- as(ij_recom, 'list')
        
        ij_unlist <- unlist(ij_list)
        max(table(ij_unlist))
        unlist(ij_list)[6]


    
    
# 문제3) MovieLense 데이터 셋을 대상으로 다음과 같이 UBCF 방식의 추천 모델을 평가하시오.
        
    library(recommenderlab)
    data(MovieLense)
    MovieLense     
        
    
    # <단계1> 사용자(user)별 본 영화 수 합계 50점 이상, 영화(item)별 본 사람 수 50명 이상 선정 
    
        model_u <- MovieLense[rowCounts(MovieLense)>=50, colCounts(MovieLense) >= 50]
    
    # <단계2> 시뮬레이션 생성 : 교차검정(cross-validation) 방법,  사용자 당 학습할 item 수=20, 시뮬레이션 3회
        
        ?evaluationScheme
        eval_scheme <- evaluationScheme(data=model_u, method='cross-validation', train=0.8, given=20, goodRating=5, k=3)
        
        eval_scheme = evaluationScheme(data=ratings_movies,
                                       method="split", train=0.8, given=15,
                                       goodRating=5, k=5)
        
        
        ubcf_method <- list("UBCF_cosine" = list(name="UBCF", param=list(method="Cosine")),
                            "UBCF_pearson" = list(name="UBCF", param=list(method="Pearson")) )
        
    # <단계3> Top10에서 2,4,6의 Cosine, Pearson 방식의 분류정확도 계산하기 

    
        result <- evaluate(eval_scheme, ubcf_method, type="topNList", n=c(2,4,6))
        
        avg(result)
        
        
        
        evaluate(eval_scheme, ubcf_method, type="tplNList", n=c(2,4,6))
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    