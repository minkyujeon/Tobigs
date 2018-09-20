rm(list=ls())
data<-read.csv("C:/Users/good/Desktop/투빅스/8주차(LDA,SVM,PCA)/PCA/cereals.csv")

#install.packages("mlbench")
library(mlbench)

## data 파악 및 전처리
str(data)
names(data)[1]<-'name' #name이 깨져있었음(한자로)

# 16개 변수로 이루어진 77개의 데이터
#범주형 데이터인 name, mfr, type중 mfr이나 type으로 회귀분석을 해보자

unique(data$mfr) #7개
unique(data$type) #2개

# C, H 총 2개의 범주로 이루어진 변수인 type를 예측!

#name은 분석에 필요없음(빼주기)
data<-data[-1]
#data의 종속변수 type을 0,1 값으로 바꿔줌
str(data)
data$type <- as.character(data$type)
data$type[data$type=="C"] <- 0
data$type[data$type=="H"] <- 1
data$type <- as.factor(data$type)

#scale해주기 위해 numeric으로 바꾸기
data$mfr<-as.numeric(data$mfr)
str(data)
## train, test 7:3으로 나누기
set.seed(100)
index <- sample(1:nrow(data), nrow(data)*0.7, replace = F)
test <- data[-index,-2] #종속변수인 'type'변수 빼주기
train <- data[index,-2]
test_label <- data[-index,2]
train_label <- data[index,2]

scale(train,center=T,scale=T) #scale가능한지 확인
str(train)


data.pca<-prcomp(train,center=T,scale=T)
?prcomp
# SVD(Singular Value Decomposition) 이용
# prcomp(x, retx = TRUE, center = TRUE, scale. = FALSE, tol = NULL, rank. = NULL, ...)
# x : data, retx : 변수축 회전 여부, center=zero:원점 설정 여부, scale : 표준화여부
# prcomp : sdev, rotation(eigenvetors), center, scale, x(principal component)

## 변수의 개수 결정(elbow point & Cumulative Proportion)
plot(data.pca,type="l") #elbow point : 대략 4정도
summary(data.pca)
# Elbow point인 PC4까지로는 72%정도까지 설명할 수 있다
min(which(summary(data.pca)[[6]][3,] >= 0.8))
# Cumulative Proportion(누적비율)을 통해 PC6까지 80%를 설명할 수 있다
# summary(sonar.pca)[[6]]  =>  Importance of components 부분
# summary(sonar.pca)[[6]][3,]  =>  Cumulative Proportion 부분
# 누적설명력이 0.8이상인것들의 인덱스 뽑아준다

# 기존 data matrix와 Eigenvetor 내적
# 행렬곱을 통해 새로운 주성분을 만드는 과정
# 새로운 주성분 PC의 성분의 구성 값 계산
# rotation이 계수값
# 주성분 구성할때의 pc1 <- a*x1 + b*x2 +... 에서 a,b
trainPRC<-as.matrix(train) %*% data.pca$rotation #내적
testPRC<-as.matrix(test) %*% data.pca$rotation

train_data<-cbind(as.data.frame(trainPRC),train_label)
test_data<-cbind(as.data.frame(testPRC),test_label)

colnames(train_data)[15] <- "label"; colnames(test)[15] <- "label"

str(train_data)
## 만든 주성분으로 회귀분석
fit <- glm(label~., family = "binomial", data=train_data)
summary(fit)

## 예측
fit_pred <- predict(fit,type="response",newdata = test_data)
test_pred<- round(fit_pred)

table<-table(test_label,test_pred)
table
sum(diag(table))/sum(table)
