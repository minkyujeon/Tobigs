
### 구현 1.KNN 가중치를 주는 함수 : 유사도 = 1/거리 사용해서 
### 구현 2.최적의 k 찾기 (5-fold cv)로 구현하기
### 함수 매개변수랑 리턴값은 맘대로 해도됩니다!

rm(list = ls())
getwd()
wdbc <- read.csv('wisc_bc_data.csv', stringsAsFactors = F)
str(wdbc)
wdbc <- wdbc[-1] #id제거
wdbc$diagnosis <- factor(wdbc$diagnosis, level=c("B","M"))

set.seed(1)
idx <- sample(1:nrow(wdbc), 0.8*nrow(wdbc))
wdbc_train <- wdbc[idx,]
wdbc_test <- wdbc[-idx,]
# 이 윗부분 까지만 동일하게 해주세요!

normalize <- function(x){ #min max 정규화(변수별로 scale이 다르므로)
  return( (x-min(x))/(max(x)-min(x)) )
}

wdbc_normal<-as.data.frame(lapply(wdbc[-1],normalize)) #데이터 정규화
wdbc_train_n<-wdbc_normal[idx,] #train set
wdbc_test_n<-wdbc_normal[-idx,] #test set

wdbc_train_n$diagnosis<-wdbc_train$diagnosis #diagnosis 추가
wdbc_test_n$diagnosis<-wdbc_test$diagnosis 

#################################################################################
Weighted_knn <- function(train_x, test_x, k){
  a = 0
  weigh=c()
  rst=c()
  df=data.frame()
  
  for(i in 1:nrow(test_x)){
    
    dist=head(dist(rbind(test_x[i,-31],train_x[-31])),nrow(train_x)) #dist 결과 계단 구조에서 첫 열 뽑아오기
    weigh = 1/dist #유사도 = 1/거리
    weigh = weigh / sum(weigh) #가중치
    top_k=head(weigh,k) #가중치 중 상위 k개 뽑기
    
    df=cbind(top_k,train_x[weigh %in% top_k,]$diagnosis)
    
    B=0;M=0
    for(j in 1:k){
      if(df[j,2]=="1"){ #B일 경우
        B=B+top_k[j]
      }
      else{ #M일 경우
        M=M+top_k[j]
      }
    }
    a = ifelse(B>M, "B", "M")
    rst = c(rst, a)
  }
  return(rst)
}
result= Weighted_knn(wdbc_train_n,wdbc_test_n,3)
table(result, wdbc_test_n[,2])
Confusion<-confusionMatrix(result, wdbc_test_n$diagnosis)
############################################################################

CrossValidation <- function(train){
  
  list=createFolds(train$diagnosis,k=5) #5개로 나누기
  rst_1=c()
  df=c()
  for(k in c(3,5,7)){
    sum=0
    mean=0
    for (i in 1:5){
      trainingset<-train[which(!(c(1:length(train$diagnosis))%in%list[[i]])),] #training set설정
      testset<-train[list[[1]],] #test set 설정
      result_1=Weighted_knn(trainingset[-32],testset[-32],k) #가중치 준 결과
      Confusion_1<-confusionMatrix(result_1,testset$diagnosis) #confusion
      sum=sum+Confusion_1$overall[1] #정확도들 더하기
    }
    mean=sum/5 #더한 정확도들 평균
    rst_1=c(rst_1,mean) #평균들 벡터로 만들기
  }
  df=cbind(c(3,5,7),rst_1) #인덱스 붙이기
  
  for(t in 1:3){ #정확도가 최대인 것의 k인덱스 뽑기
    if(df[t,2]==max(df[,2])){
      return(df[t])
    }
  }
}
warnings()
CrossValidation(wdbc_train_n)
best_k <- CrossValidation(wdbc_train_n)
pred <- Weighted_knn(wdbc_train_n, wdbc_test_n, best_k)
library(caret)
confusionMatrix(pred, wdbc_test_n$diagnosis)
