############### 데이터 분석 과제입니다. ################
# install.packages("UsingR")
# 데이터
library(UsingR)
rm(list=ls())
data("babies")
babies_copy<-babies
##### 1.회귀분석하기 #####
### wt를 예측해주세요
##추가적으로, ridge/lasso 써도 됩니다.

#결측치 검사
sum(is.na(babies_copy)) #없음

plot(babies_copy)
#딱히 선형관계를 찾기 어려움

#상관관계 분석
attach(babies_copy)
cor(wt,id) #음의 상관관계, -0.06
cor(wt,pluralty) #표준편차가 0 => 데이터 빼기
cor(wt,outcome) #표준편차가 0 => 데이터 빼기
cor(wt,date) #양의 상관관계, 0.05
cor(wt,gestation) #양의 상관관계, 0.06
cor(wt,sex) #표준편차가 0 => 데이터 빼기
cor(wt,parity) #양의 상관관계, 0.02
cor(wt,race) #음의 상관관계, -0.11
cor(wt,age) #양의 상관관계, 0.02
cor(wt,ed) #양의 상관관계, 0.03
cor(wt,ht) #양의 상관관계,0.12
cor(wt,wt1) #양의 상관관계, 0.04
cor(wt,drace) #음의 상관관계, -0.01
cor(wt,dage) #양의 상관관계, 0.01
cor(wt,ded) #양의 상관관계, 0.02
cor(wt,dht) #양의 상관관계, 0.02
cor(wt,dwt) #양의 상관관계, 0.01
cor(wt,marital) #음의 상관관계, -0.04
cor(wt,inc) #양의 상관관계, 0.05
cor(wt,smoke) #양의 상관관계, 0.006
cor(wt,time) #양의 상관관계, 0.03
cor(wt,number) #음의 상관관계, -0.005
detach(babies_copy)
# => 그다지 상관관계가 없는것처럼 보임

#NA(즉 값이 하나로 일정한 것들)+id 데이터 필요없음
summary(babies_copy)
unique(babies_copy$pluralty) #실제로 값이 하나로 일정한지 확인
unique(babies_copy$outcome) #실제로 값이 하나로 일정한지 확인
unique(babies_copy$sex) #실제로 값이 하나로 일정한지 확인
babies_copy<-babies_copy[,c(-1,-2,-3,-6)] #지우기
str(babies_copy)
head(babies_copy)
unique(babies_copy$drace)
#가변수 만들기(범주형 데이터 - race,drace/ed,ded/marital/smoke/time/number)
#1.race,drace
babies_copy$races1<-ifelse(babies_copy$race=="0",1,0)
babies_copy$races1<-ifelse(babies_copy$race=="1",1,0)
babies_copy$races1<-ifelse(babies_copy$race=="2",1,0)
babies_copy$races1<-ifelse(babies_copy$race=="3",1,0)
babies_copy$races1<-ifelse(babies_copy$race=="4",1,0)
babies_copy$races1<-ifelse(babies_copy$race=="5",1,0)
babies_copy$races1<-ifelse(babies_copy$drace=="0",1,0)
babies_copy$races1<-ifelse(babies_copy$drace=="1",1,0)
babies_copy$races1<-ifelse(babies_copy$drace=="2",1,0)
babies_copy$races1<-ifelse(babies_copy$drace=="3",1,0)
babies_copy$races1<-ifelse(babies_copy$drace=="4",1,0)
babies_copy$races1<-ifelse(babies_copy$drace=="5",1,0)
babies_copy$races2<-ifelse(babies_copy$race=="6",1,0)
babies_copy$races2<-ifelse(babies_copy$drace=="6",1,0)
babies_copy$races3<-ifelse(babies_copy$race=="7",1,0)
babies_copy$races1<-ifelse(babies_copy$drace=="7",1,0)
babies_copy$races4<-ifelse(babies_copy$race=="8",1,0)
babies_copy$races4<-ifelse(babies_copy$drace=="8",1,0)
head(babies_copy)

#2.ed,ded
babies_copy$eds1<-ifelse(babies_copy$ed=="0",1,0)
babies_copy$eds1<-ifelse(babies_copy$ded=="0",1,0)
babies_copy$eds2<-ifelse(babies_copy$ed=="1",1,0)
babies_copy$eds2<-ifelse(babies_copy$ded=="1",1,0)
babies_copy$eds3<-ifelse(babies_copy$ed=="2",1,0)
babies_copy$eds3<-ifelse(babies_copy$ded=="2",1,0)
babies_copy$eds4<-ifelse(babies_copy$ed=="3",1,0)
babies_copy$eds4<-ifelse(babies_copy$ded=="3",1,0)
babies_copy$eds5<-ifelse(babies_copy$ed=="4",1,0)
babies_copy$eds5<-ifelse(babies_copy$ded=="4",1,0)
babies_copy$eds6<-ifelse(babies_copy$ed=="5",1,0)
babies_copy$eds6<-ifelse(babies_copy$ded=="5",1,0)


#3.marital
babies_copy$marital1<-ifelse(babies_copy$marital=="1",1,0)
babies_copy$marital2<-ifelse(babies_copy$marital=="2",1,0)
babies_copy$marital3<-ifelse(babies_copy$marital=="3",1,0)
babies_copy$marital4<-ifelse(babies_copy$marital=="4",1,0)

#4.smoke
babies_copy$smoke1<-ifelse(babies_copy$smoke=="0",1,0)
babies_copy$smoke2<-ifelse(babies_copy$smoke=="1",1,0)
babies_copy$smoke3<-ifelse(babies_copy$smoke=="2",1,0)

#5.time
babies_copy$time1<-ifelse(babies_copy$time=="0",1,0)
babies_copy$time2<-ifelse(babies_copy$time=="1",1,0)
babies_copy$time3<-ifelse(babies_copy$time=="2",1,0)
babies_copy$time4<-ifelse(babies_copy$time=="3",1,0)
babies_copy$time5<-ifelse(babies_copy$time=="4",1,0)
babies_copy$time6<-ifelse(babies_copy$time=="5",1,0)
babies_copy$time7<-ifelse(babies_copy$time=="6",1,0)
babies_copy$time8<-ifelse(babies_copy$time=="7",1,0)
babies_copy$time9<-ifelse(babies_copy$time=="8",1,0)

#6.number
babies_copy$number1<-ifelse(babies_copy$number=="0",1,0)
babies_copy$number2<-ifelse(babies_copy$number=="1",1,0)
babies_copy$number3<-ifelse(babies_copy$number=="2",1,0)
babies_copy$number4<-ifelse(babies_copy$number=="3",1,0)
babies_copy$number5<-ifelse(babies_copy$number=="4",1,0)
babies_copy$number6<-ifelse(babies_copy$number=="5",1,0)
babies_copy$number7<-ifelse(babies_copy$number=="6",1,0)
babies_copy$number8<-ifelse(babies_copy$number=="7",1,0)
babies_copy$number9<-ifelse(babies_copy$number=="8",1,0)

#가변수 처리한 데이터 열 지우기
babies_copy<-babies_copy[,c(-5,-10,-7,-12,-15,-17,-18,-19)]
str(babies_copy)


summary(babies_copy)#남은 변수 중 최대가 9, 99, 999 등 자료에서 unknown이나 not asked등의 이상치를 지움
babies_copy<-babies_copy[babies_copy$wt!=999,]
babies_copy<-babies_copy[babies_copy$parity!=99,]
babies_copy<-babies_copy[babies_copy$ht!=99,]
babies_copy<-babies_copy[babies_copy$dht!=99,]
babies_copy<-babies_copy[babies_copy$wt1!=999,]
babies_copy<-babies_copy[((babies_copy$inc!=98) & (babies_copy$inc!=99)),]
babies_copy<-babies_copy[babies_copy$dwt!=999,]
babies_copy<-babies_copy[babies_copy$age!=99,]
babies_copy<-babies_copy[babies_copy$dage!=99,]
babies_copy<-babies_copy[babies_copy$gestation!=999,]

#전체 full 결과 보기
fit.full<-lm(wt~.,babies_copy)
summary(fit.full) #Adjusted R-squared : 0.2877
par(mfrow=c(2,2))
plot(fit.full)
#그래프
#Residuals vs Fitted : 딱히 잔차와 예측값 사이에 규칙을 발견할 수 없음
# Q-Q plot이 직선에 가깝게 분포하니 정규성을 만족한다고 볼 수 있다.
#scale location : 흩어진 정도가 치우치기 않다 -> 등분산성에 문제가 없다고 볼 수 있음
#stepwise 
fit.con<-lm(wt~1,babies_copy)
fit.both<-step(fit.con,list(lower=fit.con,upper=fit.full),direction="both")
summary(fit.both)

e<-resid(fit.both) #잔차
#정규성 검정
shapiro.test(e) #p=0.001 (정규성 만족)

#독립성
par(mfrow=c(1,1))
plot(predict(fit.both),e) #패턴이 
#################################################################################
##### 2.로지스틱 회귀분석하기 #####
babies_copy$new_wt <- ifelse(babies_copy$wt > median(babies_copy$wt),1,0)
babies_copy <- babies_copy[,-which(names(babies_copy)=='wt')] 
### 위 두 줄을 실행시키고 new_wt를 예측(분류)해주세요.
### 추가적으로, ridge/lasso 써도 됩니다.
head(babies_copy)
#전처리는 이미 위에서 함
#train set, test set 나누기
library(nnet)
dim(babies_copy)
index<-sample(1:dim(babies_copy)[1],dim(babies_copy)*0.8,replace=F)
train<-babies_copy[index,]
test<-babies_copy[-index,]

#glm 적용
Model_1<-glm(new_wt~.,data=train,family=binomial(link='logit'))
summary(Model_1)
coef(Model_1)
str(Model1)
pred_model_1<-predict(Model_1,newdata=test,type="response")
yhat<-ifelse(pred_model_1>0.5,1,0) #각 확률들을 0,1로 분류
table(test$new_wt,yhat)
sum(diag(table(test$new_wt,yhat)))/length(test$new_wt) #0.7

#다시 점검!(값)

Model_full<-glm(new_wt~.,data=train,family="binomial")
Model_non<-glm(new_wt~.,data=train,family="binomial")
Model_forward<-step(Model_non,list(lower=Model_non,upper=Model_full),direction="forward")
pred_forward<-predict(Model_forward,newdata=test,type="response")
table(test$new_wt,round(pred_forward)) #위에 test case랑 동일한 값이 나옴
Model_backward<-step(Model_full,list(lower=Model_non,upper=Model_full),direction="backward")
pred_backward<-predict(Model_backward,newdata=test, type="response")
table(test$new_wt,round(pred_backward)) #forward 방법이랑 동일값

Model_step<-step(Model_non,list(lower=Model_non,upper=Model_full),direction="both")
pred_step<-predict(Model_step,newdata=test,type="response")
table(test$new_wt,round(pred_step)) #backward방법이랑 동일값

#roc 커브 그리기
library(pROC)
par(mfrow=c(1,1))
curve<-roc(test$new_wt,pred_step,direction="<")
head(curve)
curve$auc #0.6861
plot(curve)
