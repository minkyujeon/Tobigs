## tree 분석을 하는게 이번 과제입니다! ##
rm(list=ls())
library(mlbench)
data(BreastCancer)
str(BreastCancer)

# 위의 data를 이용해서 Class (benign/mlignant) 를 예측하는 tree 분석을 하시면 됩니다. $id는 빼주세요
# 복습 차원의 과제이니, 배운 package는 모두 사용해 주세요. 
# parameter 값을 조정해가면서 분석해보면 더 재밌겠죠? (우수과제로 가는길..★)
# 추가로 본인이 공부한 package를 추가하셔도 됩니다! 

data<-BreastCancer[-1]
str(data)

data<-na.omit(data) #결측치 제거

#train data / test data 나누기
library(caret)
set.seed(1)
idx<-createDataPartition(data$Class, p=0.7, list=F)
train<-data[idx,];str(train)
test<-data[-idx,];str(test)

table(train$Class)/dim(train)[1]
table(test$Class)/dim(test)[1] #65:35 비율로 나뉘었다

#1. tree package
library(tree)

##################### 기본 tree ###########################

treemodel1<-tree(Class~.,data=train)
treemodel1 #*은 끝마디를 의미함
plot(treemodel1) # tree형태 생성(train으로 만들어진 tree)
text(treemodel1) # tree 변수들 입력

tree_pred1<-predict(treemodel1,test,type="class") #위에서 id를 이미 빼줬으므로 test[-1]을 할 필요가 없음(강의자료와 다름!)
confusionMatrix(tree_pred1,test$Class)
#accuracy : 0.9461

###########################################################
##################gini 계수 기준 tree##################

treemodel2<-tree(Class~.,data=train,split="gini") #gini계수로 예측
treemodel2
plot(treemodel2)
text(treemodel2) #parameter 조정 or pruning이 필요

tree_pred2<-predict(treemodel2,test,type="class")
confusionMatrix(tree_pred2,test$Class) #0.9461 tree가 더 복잡해졌지만 정확도는 위와 동일

tree_pred2_train<-predict(treemodel2,train,type="class")
confusionMatrix(tree_pred2_train,train$Class)
# 정확도 : 0.9812 -> test data보다 더 크다 (overfit이 발생!)

############# control 값 조정 ##################

#실습때 한 것처럼 mincut=100, minsize=200으로 해보기 -> 마디에 최소 200개가 있어야 split
my_control<-list(mincut=100,minsize=200, nmax=100)
treemodel3<-tree(Class~.,data=train,split="gini",control=my_control)
plot(treemodel3);text(treemodel3)

#mincut=100, minsize=400으로 해보기 -> 마디에 최소 400개가 있어야 split
my_control<-list(mincut=100, minsize=400, nmax=100)
treemodel4<-tree(Class~.,data=train,split="gini",control=my_control)
treemodel4
plot(treemodel4);text(treemodel4)


###################################################
#overfitting이 일어난 treemodel2에 대한 pruning 진행
treemodel2
treemodel2_prune<-cv.tree(treemodel2,FUN=prune.misclass)
plot(treemodel2_prune) #r그림을 보고 오분류율을 최저로 하는 가지수를 찾아주기.  대략 5~7정도
treemodel2_prunung_f<-prune.misclass(treemodel2,best=7)
plot(treemodel2_prunung_f);text(treemodel2_prunung_f) #pruning을 한 결과

tree_prune_pred1<-predict(treemodel2_prunung_f,test,type="class")
confusionMatrix(tree_prune_pred1,test$Class) #5일때 : 0.9363, 7일때 : 0.9461
# => 7일때 위 prunung하기 전과 정확도가 같다(더 나아지진 않았어도 tree는 훨씬 더 간단해졌는데 정확도가 유지되므로 굿)

tree_prune_pred2<-predict(treemodel2_prunung_f,train,type="class")
confusionMatrix(tree_prune_pred2,train$Class) #0.977 (역시나 overfitting이 있지만 pruning을 하기전에 정확도의 차이가 0.9812-0.9461 = 0.0351 , 0.977-0.9461=0.0309로 줄었음!)

#######################################################

############2. rpart package #############
library(rpart)

rpartmodel1<-rpart(Class~.,data=train)
rpartmodel1
plot(rpartmodel1);text(rpartmodel1)

library(rpart.plot)
prp(rpartmodel1, type=4,extra=2,digits=3)
prp(rpartmodel1, type=1,extra=2,digits=3)
prp(rpartmodel1, type=1,extra=1,digits=3)
prp(rpartmodel1, type=1,extra=1,digits=4) #위와 다른게 없음
prp(rpartmodel1, type=1,extra=1,digits=2) #위와 다른게 없음
prp(rpartmodel1, type=3,extra=3,digits=3)

rpart_pred1<-predict(rpartmodel1,test,type="class")
confusionMatrix(rpart_pred1,test$Class) #0.9461


############### parameter 조정하면서 예측해보기 ################
#minsplit 조정
rpartmodel2<-rpart(Class~.,data=train,minsplit=50)
rpartmodel2
par(mfrow=c(1,2))
prp(rpartmodel1, type=4, extra=2, digits=3);prp(rpartmodel2, type=4, extra=2, digits=3)
#tree가 minsplit=20(default)일 때(왼쪽)보다 (오른쪽이)더 간단해짐

#maaxdept 조정
rpartmodel3<-rpart(Class~., data=train, maxdepth=1)
rpartmodel3
par(mfrow=c(1,1))
prp(rpartmodel3, type=4, extra=2, digits=3) 
#tree 깊이가 최대 1이여서 한번만 split된다.

#minsplit, cp조정
set.seed(1)
rpartmodel4<-rpart(Class~., data=train, minsplit=13, cp=0.003) #최소 관측값과 cp값을 낮춤->tree가 더 복잡해짐 
rpartmodel4
prp(rpartmodel4, type=4, extra=2, digits=3)
rpart_pred4<-predict(rpartmodel4,test,type="class")
confusionMatrix(rpart_pred4,test$Class) #0.951

rpart_pred4_train<-predict(rpartmodel4,train,type="class")
confusionMatrix(rpart_pred4_train,train$Class) #0.977
#overfit이 생김(pruning을 진행한 pred2보다 정확도의 차이가 작음!) -> 이게 더 성능이 좋은건가..?

#overfitting이 일어난 rpartmodel4에 대한 pruning 진행!

#################### pruning ########################
printcp(rpartmodel4)
plotcp(rpartmodel4) #cp값이 대략 0.04~0.21사이?

rpartmodel4_prune<-prune(rpartmodel4, cp=rpartmodel4$cptable[which.min(rpartmodel4$cptable[,"xerror"])],"cp") #=> cp값 0.003

prp(rpartmodel4_prune, type=4, extra=2, digits=3)
rpart_pred4_prune<-predict(rpartmodel4_prune,test,type="class")
confusionMatrix(rpart_pred4_prune,test$Class) #0.951 (pruning을 해도 정확도 동일 -> 더 효율적)
rpart_pred4_prune_train<-predict(rpartmodel4_prune,train,type="class")
confusionMatrix(rpart_pred4_prune_train,train$Class) #0.977 (동일)

############ 3. party package #########
library(party)

# party package는 p-test를 거친 significance를 사용하므로 pruning과정이 따로 필요없음

#############기본 ctree tree################
partymodel1<-ctree(Class~.,data=train)
partymodel1

party_pred1 <- predict(partymodel1,test,type="response")
confusionMatrix(party_pred1,test$Class) #0.9608 (이전보다 더 높다) => 가장 높음

party_pred1_train <- predict(partymodel1, train, type="response")
confusionMatrix(party_pred1_train,train$Class) #0.9749 (test와의 차이 0.0141 => 차이가 가장 작다)

#############parameter 조정하면서 예측해보기 ###############
set.seed(1)
partymodel2<-ctree(Class~.,data=train, controls=ctree_control(mtry=3))
#Tree에서 가지를 뻗어나갈 때 임의로 변수 3개를 선택해서 그 중 가장 적당한 것 선택

partymodel2
plot(partymodel2)

party_pred2<-predict(partymodel2,test,type="response")
confusionMatrix(party_pred2,test$Class) #0.9461

party_pred2_train<-predict(partymodel2,train,type="response")
confusionMatrix(party_pred2_train,train$Class) #0.9582















































