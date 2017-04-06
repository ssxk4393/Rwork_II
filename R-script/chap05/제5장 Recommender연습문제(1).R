#############################
## 제5장 Recommander 연습문제 
#############################

# 문제1) MovieLense 데이터 셋을 대상으로 다음과 같이 UBCF 방식, Cosine 유사도 방식으로 추천모델를 생성하시오.

    head(as(MovieLense, 'matrix')[,c(1:3)])
    
# <단계1> 사용자(user)별 본 영화 수 100편 이상, 영화(item)별 본 사람 수 150명 이상 선정 

    MovieLense_s <- MovieLense[rowCounts(MovieLense)>=100, colCounts(MovieLense) >= 150]
    MovieLense_s  # 362 x 202
    
# <단계2> 95%  사용자 샘플링으로 5% 사용자에게 3개씩 영화 추천
    
    idx <- sample(1:nrow(MovieLense_s), nrow(MovieLense_s)*0.95)
    
    train_movie <- MovieLense_s[idx,]
    recomm_movie <- MovieLense_s[-idx,]    
        
    reco_model <- Recommender(train_movie, method='UBCF', parameter='Cosine')
    reco_list <- predict(reco_model, recomm_movie, n=3)
    reco_list
    # Recommendations as ‘topNList’ with n = 3 for 19 users.  
    
    
# <단계3> 추천받은 결과 data.frame 저장(행:사용자 수, 열: 추천받은 3개 영화제목) 
    
    recomm <- as(reco_list, 'list')
    recomm_df <- as.data.frame(recomm)
    recomm_ma <- as.matrix(recomm_df)
    recommend <- as.data.frame(t(recomm_ma))

    recommend
        
# <단계4> movie 테이블을 생성하여 추천결과 저장(MariaDB 이용)  

    
    
    