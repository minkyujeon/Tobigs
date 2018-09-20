rm(list=ls())
setwd('C:/Users/good/Desktop/투빅스/4주차(의사결정나무,앙상블,나이브베이즈)/앙상블')
dat=read.csv("dat.csv")
str(dat) #총 569행, 32열 데이터
dat$is_cancer=as.factor(dat$is_cancer)
set.seed(1)

idx=sample(nrow(dat),nrow(dat)*0.7)
train=dat[idx,]
test=dat[-idx,]


#총 32개의 변수가 있고 종속변수 빼면 31개
train_data=train[,-32]
test_data=test[,-32]

n_estimator=10 #트리 개수
max_features=ncol(train) #최대 변수 개수

n_variable<-sample(max_features,1) #random개의 변수 뽑기

library(tree)

my_rf<-function(train_data,test_data,n_estimator,max_features){
  lst_pred=c()
  rst=c()
  for(i in 1:n_estimator){
    a<-sort(sample(nrow(train_data),nrow(train_data),replace=T)) #행뽑기
    b<-sort(sample(ncol(train_data),n_variable)) #변수 인덱스 (n_variable)개씩 n_estimator번
    is_cancer<-train[a,]$is_cancer 
    classifier<-cbind(train_data[a,b],is_cancer) #분류기 생성
    
    treemodel<-tree(is_cancer~.,data=classifier,split="gini") #tree함수에 넣어서 treemodel만들기
    tree_pred<-predict(treemodel,test_data,type="class") #predict함수 사용해서 treemodel data를 classifier랑 돌림
    tree_pred=as.numeric(tree_pred)-1
    lst_pred<-cbind(lst_pred,tree_pred)
  }
  #각 행별로 더하기-> 1이 5보다 크면 1 아니면 0으로 분류
  for(j in 1:nrow(lst_pred)){
    x=0
    for(k in 1:ncol(lst_pred)){
      if(lst_pred[j,k]==0){
        x
      }else{
        x=x+1
      }
    }
    y=ifelse(x>ncol(lst_pred)/2,1,0)
    rst=c(rst,y)
  }
  return(rst)
}

result=my_rf(train_data,test_data,n_estimator,max_features)








