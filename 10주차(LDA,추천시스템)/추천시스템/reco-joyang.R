######################################################################################################
## 많은 추천 패키지 중 recommenderlab 사용 
## 패키지 설치
install.packages('recommenderlab')
if (!require(lsa)) install.packages("ggplot2")
if (!require(lsa)) install.packages("stringr")
if (!require(lsa)) install.packages("tidyverse")
install.packages('matrixStats')
library(tidyverse)
library(recommenderlab)
library(ggplot2)
library(stringr)

### 1. EDA

## 내장된 데이터 셋을 불러온다.
data_packages <- data(package = "recommenderlab")
data_packages$results[, c("Item", "Title")]

# 영화와 사용자들의 평점이 기록된 데이터 셋
data(MovieLense)

# 살펴보면 realRatingMatrix 라는 포맷으로 되어있으며 희소평점 매트릭스를 포함한다.
# 다른 데이터 가져와 recommenderlab에 돌리기 위해선 해당포맷으로 맞춰줘야한다.
class(MovieLense)

# 처음 다섯명의 사용자가 얼마나 유사한지, 거리계산
similarity_users <-  similarity(MovieLense[1:4,] , method = "cosine", which = 'users')

# dist로 확인된다
class(similarity_users)


## 매트릭스로 변환후 시각화 -> 적색일수록 유사도가 높다
image(as.matrix(similarity_users), main = "유저 유사도")

## realRatingMatrix 는 S4 클래스 이므로 슬롯을 통해 접근한다고 한다
class(MovieLense@data)
vector_ratings = as.vector(MovieLense@data)
unique(vector_ratings) # 5점만점 그리고 최저가 0점인것을 볼 수 있다.

# 집계 해서 살펴보면
table_ratins  = table(vector_ratings);table_ratins

# 점수가 0점으로 매겨진것은 누락된 값이므로 제거
vector_ratings = vector_ratings[vector_ratings != 0 ]

# 간단한 시각화
vector_ratings = factor(vector_ratings)
qplot(vector_ratings) + ggtitle("점수에 대한 분포")
# 4점이 제일 빈번하다


## 가장 많이 본 영화탐색
views_per_movie = colCounts(MovieLense)
table_views = data.frame(movie = names(views_per_movie), view= views_per_movie)
table_views = table_views[order(table_views$view, decreasing = T),]

ggplot(table_views[1:6,], aes(x = movie , y = view)) + geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45 ,hjust = 1))+
  ggtitle("top 조회수 영화 목록")

## 히트맵 그려보기 -> 행과 열이 정렬되었기 때문에 평점이 없거나 낮은 영화가 몰려있는 위쪽이 비어있다 
image(MovieLense, main = "Heatmap of the rating matrix")

# 희소 영역제외하고 보자
image(MovieLense[1:10, 1:15], main = "Heatmap of the first rows and columns")

# 사용자와 영화의 백분위 수
min_n_movies  = quantile(rowCounts(MovieLense), 0.99)
min_n_users  = quantile(colCounts(MovieLense), 0.99)
image(MovieLense[rowCounts(MovieLense) > min_n_movies,
                 colCounts(MovieLense) > min_n_users], main = "Heatmap of the top rows and columns")


## 2. 데이터 전처리

# 데이터 살펴본 결과 얻은 결론
# 1) 시청횟수가 적은 영화는 편향을 줄 수 있다.
# 2) 평가를 거의 남기지 않은 사용자도 제거해도 되겠다.

# 영화를 적어도 50편이상 평가하고 적어도 100명이상이 평가한 영화로만 자르자
ratings_movies = MovieLense[rowCounts(MovieLense) > 50, colCounts(MovieLense) > 100]
# 원본의 20% 정도가 남음

# 평점 분포를 그려보자 
library('qqplot2')
average_rating_per_user = rowMeans(ratings_movies)
qplot(average_rating_per_user) + stat_bin(binwidth = 0.1) +
  ggtitle("Dist of the average rating per user")

# 정규화를 고려한다면   
rating_movies_norm = normalize(rating_movies)
sum(rowMeans(rating_movies_norm)>0.00001) # 0으로 정규화 ok


## 3. CF모델 구축
# IBCF
# train / test set
which_train = sample(x = c(T,F), size = nrow(ratings_movies), 
                     replace = T, prob = c(0.8,0.2))
recc_data_train = ratings_movies[which_train,] 
recc_data_test = ratings_movies[!which_train,]

recommender_models = recommenderRegistry$get_entries(dataType  = "realRatingMatrix")
recommender_models$IBCF_realRatingMatrix$parameters
# k: k개의 가장유사한 아이템을 선별하겠다(유사도 계산을 위해 고려 할 가장 가까운 이웃의 개수)
# method : 유사도 함수

#### 모델 생성
recc_model = Recommender(data = recc_data_train, method = "IBCF", 
                         parameter = list(k = 30))


# 모델 탐색
model_details = getModel(recc_model)
model_details$description
model_details$sim # 이안에 유사도 매트릭스 포함하고 있다 

# 히트맵 그리기 가능
image(model_details$sim[1:20, 1:20])

# 테스트 셋에 모델 적용
n_recommended = 6
recc_predicted  = predict(object = recc_model , newdata = recc_data_test, 
                          n = n_recommended)

# items : 각 사용자에 대한 추천 아이템들의 색인 목록, itemLabels : 아이템 이름 , n : 추천수
# 첫번째 테스트 유저에 대한 추천 항목

recc_user_1 = recc_predicted@items[[1]];recc_user_1
movies_user_1 = recc_predicted@itemLabels[recc_user_1];movies_user_1

# 이런식으로 각 사용자에 대한 매트릭스 만들 수 있다
recc_matrix = sapply(recc_predicted@items, function(x){colnames(rating_movies)[x]})
recc_matrix[,1:4]


## UBCF
recc_model  = Recommender(data = recc_data_train, method = "UBCF")
recc_model

n_recommended = 6
recc_predicted = predict(object= recc_model , newdata  = recc_data_test, 
                         n= n_recommended)
recc_predicted


# 아이템 추천
recc_matrix  = sapply(recc_predicted@items , function(x){colnames(ratings_movies)[x]})
recc_matrix[,1:4]


# 시각화 코드
number_of_items = factor(table(recc_matrix))
chart_title = "dist of the number of items for UBCF"
qplot(number_of_items) + ggtitle(chart_title)

# 다른영화 보다 훨씬 자주 추천되는 영화가있구나
number_of_items_sorted = sort(number_of_items , decreasing = T)
number_of_items_top = head(number_of_items_sorted, n = 4)
table_top = data.frame(names(number_of_items_top), number_of_items_top)
table_top


# 이진데이터 binarize(rating_movies , minRating = 1)
# 자카드 거리 사용

###  모델 평가
percentage_training = 0.8 # 트레이닝 셋 80%
items_to_keep = 15 # 아이템 수 15개(추천 생성에 사용되는 아이템 최소 개수)
rating_threshold = 3 # 좋고 나쁜 평점의 임계값
n_eval = 1
# 단순히 split 할수도있고
eval_sets = evaluationScheme(data = ratings_movies, method = "split", train = percentage_training,
                             given = items_to_keep, goodRating =rating_threshold , k = n_eval) #단순히
eval_sets

# 붓스트랩 사용할수있다
eval_sets = evaluationScheme(data = ratings_movies, method = "bootstrap", train = percentage_training,
                             given = items_to_keep, goodRating =rating_threshold , k = n_eval)

# k-fold cv사용
n_fold = 4
items_to_keep = 15
rating_threshold = 3
eval_sets = evaluationScheme(data = ratings_movies, method = 'cross-validation', k= n_fold,
                             given = items_to_keep, goodRating = rating_threshold)
# 모델 생성
eval_recommender = Recommender(data = getData(eval_sets, "train"), method = 'IBCF',
                               parameter = NULL)
eval_prediction = predict(object = eval_recommender , newdata = getData(eval_sets, "known"),
                          n= 10 , type = "ratings")
# 정확도 평가하기
eval_accuracy = calcPredictionAccuracy(x = eval_prediction, data = getData(eval_sets, "unknown"), byUser =T)
head(eval_accuracy)

# confusion matrix 확인
results = evaluate(x = eval_sets, method = "IBCF", n= seq(10,100,10))
head(getConfusionMatrix(results)[[1]])

# ROC 곡선 그려보기
plot(results, annotate = T, main = "ROC curve") #별 차이없다



### 모델 별 비교하기
model_to_evaluate = list( #다양한 parameter 고려
  IBCF_cos = list(name = "IBCF", param = list(method = "cosine")),
  IBCF_cor = list(name = "IBCF", param = list(method = "pearson")),
  UBCF_cos = list(name = "UBCF", param = list(method = "cosine")),
  UBCF_cor = list(name = "UBCF", param = list(method = "pearson")),
  random = list(name = "RANDOM", param = NULL)
  
)

n_recommedations = c(1,5, seq(10, 100, 10))

list_results = evaluate(x = eval_sets , method = model_to_evaluate, n=n_recommedations)

# 평균 confusion matrix 추출
avg_matrices = lapply(list_results, avg)
head(avg_matrices$IBCF_cos[,5:8])

# ROC 곡선-> UBCF_cor가 젤 좋다 (파란색)
plot(list_results, annotate = 1, legend = "topleft")
title("ROC curve")

# precision / recall 도표
plot(list_results , "prec/rec", annotate = 1, legend = "bottomright")
title("precision-recall")

## 모델 하나를 잡고 
## 하이퍼파라미터 튜닝할 수 있다 
# k: k개의 가장유사한 아이템을 선별하겠다(유사도 계산을 위해 고려 할 가장 가까운 이웃의 개수)
vector_k = c(5,10,20,30,40)
models_to_evaluate = lapply(vector_k, function(k){
  list(name = "IBCF", param = list(method = "cosine", k = k))
})
names(models_to_evaluate) = paste0("IBCF_k_", vector_k)
n_recommendations = c(1,5, seq(10,100,10))
list_results = evaluate(x = eval_sets, method = models_to_evaluate, n = n_recommendations)

# 위 조건대로 k에 따라 ROC 곡선 개형 살펴보자
plot(list_results, annotate = 1 , legend = "topleft", title("ROC curve")) #k가 제일 클 때 가장 좋음
title("ROC curve") 

# precision / recall 그리기
plot(list_results, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-recall")




################################################################################################
# SVD 예시 
# xwMOOC 참조 
library(tidyverse)
library(stringr)
library(matrixStats)
if (!require(lsa)) install.packages("lsa")
library(lsa)
# 1. 데이터 ------------------------------

rating_df <- tribble(
  ~ user, ~배트맨,  ~겟아웃,  ~위대한쇼맨, ~인터스텔라,
  "현경",     1 ,      2 ,      8 ,     10,
  "인호",     10,       7,       8,      3,
  "유리",     8 ,      9 ,      9 ,     2,
  "용재",     4 ,      5 ,      9 ,     7)

rating_mat <- rating_df %>% 
  remove_rownames() %>% 
  column_to_rownames(var="user") %>% 
  as.matrix()

DT::datatable(rating_mat)

# 2. 행렬분해 ------------------------------
## 2.1. 행렬 근사
rating_svd <- svd(rating_mat)
D <- diag(rating_svd$d)


# 가장 고유치 2개만 뽑고 나머지 버리겠다 (충분한 설명력 가졌다 판단) (고유값을 모두 하지 않아도됨)
plot(cumsum(rating_svd$d^2/sum(rating_svd$d^2)), type="l",
     xlab = "Singular vector", ylab = "Cumulative percent of variance explained") #2개만 해도 90퍼넘는 정확도
abline(v = 2, col="red") 

D <- diag(rating_svd$d[1:2])
U <- rating_svd$u[1:4,1:2]
V <- rating_svd$v[1:4,1:2]


# 영화(item)간 유사도
cor(U%*%D%*%t(V))  # 분해 결과 
cor((rating_mat)) # 원본 

rating_approx <- U %*% D %*% t(V) #  X = U D V'
sum(rating_mat - rating_approx)


## 2.2. 영화 추천

rating_mat[2,c(2,4)] <- NA # 인호가 영화 두개를 보지 않았다 
rating_mat

# 빈값에 임의로 평균을 주고 

rating_mat[2,c(2,4)] <- 9

# svd 를 통해 행렬을 분해한다
rating_svd2 <- svd(rating_mat)

# 고유행렬 Σ
D <- diag(rating_svd2$d)

# 행렬을 분해한 뒤, Σ에서 두 벡터 0으로 만든다. 
D[3:4, 3:4] <- 0 # 고유값 두개만 선택 -> 차원 축소 
rating_mat
after <- rating_svd2$u %*% D %*% t(rating_svd2$v) #  X = U D V'
sum(rating_mat-after)
movie_which = which(after[2,]==max(after[2,c(2,4)]))
colnames(rating_mat)[movie_which] # 인호를 위한 영화 추천
