setwd("C:/Users/good/Desktop/투빅스/설과제/설 데이터 분석 과제")
rm(list=ls())
taxi_test<-read.csv("taxi_test.csv")
taxi_train<-read.csv("taxi_train.csv")


library(pROC)
library(mlbench)
library(glmnet)
library(car)
library(caret)
library(nnet)

#데이터 형태 보기
head(taxi_test)
head(taxi_train)
str(taxi_train)
summary(taxi_train)

unique(taxi_train$vendor_id)
unique(taxi_train$passenger_count)
unique(taxi_train$store_and_fwd_flag)

#결측치 검사
sum(is.na(taxi_train))

#passenger_count=="7"일 경우 / trip_duration>=86400일경우가 2개만 존재(이상치로 봐도 됨. max가 3526282 이므로) 빼보기 

taxi_train_1<-taxi_train[,c(-1,-2)] 

#이상치 확인
sum(taxi_train_1$passenger_count=="7") #2
sum(taxi_train_1$trip_duration>=86400) #2

#이상치 빼주기
taxi_train_1<-taxi_train_1[taxi_train_1$passenger_count!="7",]
taxi_train_1<-taxi_train_1[taxi_train_1$trip_duration<=86400,]

################################################################
attach(taxi_train_1)
cor(trip_duration,vendor_id) #0.03603122 , 양의 상관관계
cor(trip_duration,passenger_count) #0.01527381, 양의 상관관계
cor(trip_duration,pickup_longitude) #0.06484359, 양의 상관관계
cor(trip_duration,pickup_latitude) #-0.04484247, 음의 상관관계
cor(trip_duration,dropoff_longitude) #0.03982492, 양의 상관관계
cor(trip_duration,dropoff_latitude) #-0.03353716, 음의 상관관계
cor(trip_duration,as.numeric(store_and_fwd_flag)) #0.00302539, 양의 상관관계
#별로 상관관계가 없는것처럼 보임
cor(pickup_longitude,pickup_latitude) #-0.1213869
cor(dropoff_latitude,dropoff_longitude) #0.05926046
cor(pickup_longitude,dropoff_longitude) #0.4295142 -> 상관관계가 매우 높음
cor(pickup_latitude,dropoff_latitude) #0.5379023 -> 상관관계가 매우 높음
#->경도끼리, 위도끼리의 상관관계가 높아 다중 공선성의 문제가 발생하여 추정 정밀도가 높아질 수도 있음 -> 변수 빼자
par(mfrow=c(4,2))
hist(vendor_id)
hist(passenger_count) #"7"인 경우가 2개밖에 없음
hist(pickup_longitude)
hist(pickup_latitude)
hist(dropoff_longitude)
hist(dropoff_latitude)
hist(as.numeric(store_and_fwd_flag))

plot(trip_duration,vendor_id)
plot(trip_duration,passenger_count)
plot(trip_duration,pickup_longitude)
plot(trip_duration,pickup_latitude)
plot(trip_duration,dropoff_longitude)
plot(trip_duration,dropoff_latitude)
plot(trip_duration,store_and_fwd_flag)
detach(taxi_train_1)
#################################################################
summary(taxi_train_1)
taxi_train_1<-taxi_train_1[,-7] #상관관계가 낮은 변수를 지움
str(taxi_train_1)

#범주화, 가변수처리
taxi_train_1$vendor_id<-as.factor(taxi_train_1$vendor_id) #factor로 바꿔줌
taxi_train_1$passenger_count_1<-as.factor(ifelse(taxi_train_1$passenger_count=='0',1,0))
taxi_train_1$passenger_count_2<-as.factor(ifelse(taxi_train_1$passenger_count=='1',1,0))
taxi_train_1$passenger_count_3<-as.factor(ifelse(taxi_train_1$passenger_count=='2',1,0))
taxi_train_1$passenger_count_4<-as.factor(ifelse(taxi_train_1$passenger_count=='3',1,0))
taxi_train_1$passenger_count_5<-as.factor(ifelse(taxi_train_1$passenger_count=='4',1,0))
taxi_train_1$passenger_count_6<-as.factor(ifelse(taxi_train_1$passenger_count=='5',1,0))
taxi_train_1<-taxi_train_1[,-which(names(taxi_train_1)=='passenger_count')]

#case1(선형 회귀분석)
fit.con<-lm(trip_duration~1,taxi_train_1) #설명변수를 넣지 않은 모델
fit.full<-lm(trip_duration~.,taxi_train_1) #모든 변수가 포함된 모델
summary(fit.full) #adjusted R-squared : 0.007463

#전진선택법
fit.forward<-step(fit.con,list(lower=fit.con,upper=fit.full),direction="forward")
summary(fit.forward) #0.007463 -> 0.008251 (이상치 제거 후)
#후진선택법
fit.backward<-step(fit.full,list(lower=fit.con,upper=fit.full),direction="backward")
summary(fit.backward) #0.006893 -> 0.008251 (이상치 제거 후)
#단계적 회귀방법(stepwise)
fit.both <- step(fit.con, list(lower=fit.con, upper=fit.full),direction="both")
summary(fit.both) #0.006893 -> 0.008251 (이상치 제거 후)

#이상치 제거
a<-outlierTest(fit.both)
# taxi_train_1[671741,]
# taxi_train_1[850378,]
# taxi_train_1[938833,]
taxi_train_1<-taxi_train_1[c(-500360,-152333,-481003,-314214,-69142,-671741,-850378,-768273,-468537,-938833),]

##############################################################
#case 2 (kmeans)
library(flexclust)
taxi_train_2<-taxi_train_1
#elbow point 기법 이용하여 k 정하기 -> '거리'이므로 'numeric'상태여야 함
visual<-NULL
for(i in 5:10){
  result<-kmeans(taxi_train_2,i,nstart=10)
  visual[i]<-result$tot.withinss
}
par(mfrow=c(1,1))
plot(visual,type="l",xlab="k") #6~7정도

#kmeans 함수 적용
kmeans1<-kmeans(taxi_train_2,7,nstart=10)
trip_index<-kmeans1$cluster #각 개체별 할당된 군집번호 1~7까지 군집숫자
taxi_train_1$trip_index<-as.factor(trip_index)

#train데이터에서 또 train과 test로 나눔(성능 측정을 위해)
set.seed(1)
idx=sample(nrow(taxi_train_2),nrow(taxi_train_2)*0.7)
train_2=taxi_train_2[idx,]
test_2=taxi_train_2[-idx,]
summary(train_2)

#로지스틱 회귀(multinom)
model1<-multinom(trip_index~.,data=train_2,link='logit')
coef(model1)
pred_multi<-predict(model1,newdata=test_2,type="class")
result_table<-table(test_2$trip_index,pred_multi)
sum(diag(result_table)) / sum(result_table) #0.9861611
#->1~7구간의 인덱스로 구분해서 인덱스 구간별로 예측을 하려했지만 이런식으로 하면 안될것같음(test데이터가 들어오면 그것의 index도 만들어줘야...?)

########################################################################
#case3 -> trip_duration자체를 범주화해보기
#elbow_point에 따라 7등분으로 나누기 위해 최소,1사분위,중위수,3사분위,최대에서 그 사이를 또 반으로 나눠서 범주화를 함
taxi_train_3<-taxi_train_1
summary(taxi_train_3)
taxi_train_3<-within(taxi_train_3,{
  trip_duration_1=character(0)
  trip_duration_1[trip_duration<=199]="~199"
  trip_duration_1[trip_duration>199 & trip_duration<=397]="199~397"
  trip_duration_1[trip_duration>397 & trip_duration<=662]="397~662"
  trip_duration_1[trip_duration>662 & trip_duration<=868.5]="662~868.5"
  trip_duration_1[trip_duration>868.5 & trip_duration<=1075]="868.5~1075"
  trip_duration_1[trip_duration>1075 & trip_duration<=43733.5]="1075~43733.5"
  trip_duration_1[trip_duration>43733.5 & trip_duration<=max(trip_duration)]="43733.5~"
  trip_duration_1=factor(trip_duration_1,
                         level=c("~199","199~397","397~662","662~868.5","868.5~1075","1075~43733.5","43733.5~"))
})

taxi_train_3<-taxi_train_3[,-6] #trip_duration 뺌
summary(taxi_train_3)

#train데이터에서 또 train과 test로 나눔(성능 측정을 위해)
set.seed(1)
idx_3=sample(nrow(taxi_train_3),nrow(taxi_train_3)*0.7)
train_3=taxi_train_3[idx_3,]
test_3=taxi_train_3[-idx_3,]


#multinom이용하기
model_multinom_3<-multinom(trip_duration_1~.,data=train_3,link='logit')
pred_multi_3<-predict(model_multinom_3,newdata=test_3,type="class")
result_table_3<-table(test_3$trip_duration_1,pred_multi_3)
sum(diag(result_table_3)) / sum(result_table_3) #0.319427


################################################################
#case 4 randomforest -> 데이터가 많아서 돌아가지 않음 / Error: cannot allocate #vector of size 5.3 Gb

library(randomForest)
fit<-randomForest(trip_duration_1~.,importance=T,data=train_3)
pred<-predict(fit,newdata=test_3[,-12])
confusionMatrix(pred,test_3[,12])