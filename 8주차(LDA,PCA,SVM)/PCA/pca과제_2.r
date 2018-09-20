data_1<-read.csv("C:/Users/good/Desktop/투빅스/8주차(LDA,SVM,PCA)/PCA/2015_7차_직접측정데이터.csv")

#install.packages("mlbench")
library(mlbench)
str(data_1)
summary(data_1)
#na를 모두 없애면 남성 데이터가 사라지므로 없애면 안됨

#남성인지 여성인지!
unique(data_1$성별)

data_1$성별 <- as.character(data_1$성별)
data_1$성별[data_1$성별=="남"] <- 0
data_1$성별[data_1$성별=="여"] <- 1
data_1$성별 <- as.factor(data_1$성별)

str(data_1)
data_1[is.na(data_1)] <- 0 #결측치를 0으로 대체(다 지우면 남성이 사라짐)
set.seed(100)
index<-sample(1:nrow(data_1),nrow(data_1)*0.7, replace=F)

test<-data_1[-index,-2]
train<-data_1[index,-2]
test_label<-data_1[-index,2]
train_label<-data_1[index,2]

data.pca<-prcomp(train,center=T,scale=T)

plot(data.pca,type="l") #elbow point : 3정도
summary(data.pca) #pc3까지 60%정도까지 설명 가능

min(which(summary(data.pca)[[6]][3,]>=0.8)) #누적비율을 통해 pc10까지 80% 설명가능

trainPRC<-as.matrix(train) %*% data.pca$rotation
testPRC<-as.matrix(test) %*% data.pca$rotation

train_data<-cbind(as.data.frame(trainPRC),train_label)
test_data<-cbind(as.data.frame(testPRC),test_label)

colnames(train_data)[135]<-"label";colnames(test)[135]<-"label"

fit<-glm(label~.,family="binomial",data=train_data)
summary(fit)

fit_pred<-predict(fit,type="response",newdata=test_data)
test_pred<-round(fit_pred)

table<-table(test_label,test_pred)
table
sum(diag(table))/sum(table)
