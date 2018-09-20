############## 연관성 분석 과제 ##############
rm(list=ls())
install.packages("mlbench")
library(mlbench)
data("BostonHousing")
str(BostonHousing)
boston_data <- BostonHousing
summary(boston_data)
summary(boston_data$medv)
#   Min. 1st Qu.  Median    Mean  3rd Qu.    Max. 
#  5.00   17.02   21.20    22.53   25.00    50.00 
boston_data$medv<-cut(boston_data$medv,c(0,17.02,25.00,50.00),labels = c("inexpensive","middle","expensive"))

#### 위의 boston_data 에서 medv 변수의 각 클래스에 연관을 주는 top5(lift순,confidence순)를 각각 제시하세요 ####  

#rhs에다가 위 값들을 넣으면 
#범주화 시키기 -> 범주화 시킬때 자신만의 기준을 제시해주기 (반 잘라서~ 그림을 그려보니~ 등등)
summary(boston_data)
str(boston_data)
#####################################################################
boston_data2<-boston_data[,!colnames(boston_data) %in% c("medv","chas")]
windows()
par(mfrow=c(3,4))
for(i in 1:length(boston_data2)){
  hist(boston_data2[,i],main=colnames(boston_data2[i]))
  abline(v=quantile(boston_data2[,i],0.25),col="red",lwd=3,lty=5)
  abline(v=quantile(boston_data2[,i],0.75),col="blue",lwd=3,lty=5)
}
#데이터 범주화 시키기
#summary에서 최소,1,3사분위수,평균,중위수,최댓값과 histogram의 데이터 분포를 참고

hist(boston_data$crim) #0~20에 대부분 모여있음
boston_data$crim<-cut(boston_data$crim,c(min(boston_data$crim),0.25651,3.67708,max(boston_data$crim)),include.lowest=T,right=F,labels=c("낮음","중간","높음"))
#그림을 보니 거의 한군데 몰려있어서 최소, 3사분위수,  max로 나눔

hist(boston_data$zn)
boston_data$zn<-cut(boston_data$zn,c(0,11.36,55,max(boston_data$zn)),include.lowest=T,right=F,labels=c("낮음","중간","높음"))
#그림을 보니 최솟값 이후에 평균 정도부터 두번째 구간, 그 다음 역시 100과 12.5 사이인 55 정도로 구간을 잡음 

hist(boston_data$indus)
boston_data$indus<-cut(boston_data$indus,c(0.46,5.19,18.10,max(boston_data$indus)),include.lowest=T,right=F,labels=c("낮음","중간","높음"))
#데이터가 비교적 고르게 분포하는 편이라 최소,1,3사분위수,최대로 나눔

hist(boston_data$nox)
boston_data$nox<-cut(boston_data$nox,c(0.3850,0.4490,0.6240,max(boston_data$nox)),include.lowest=T,right=F,labels=c("낮음","중간","높음"))
#데이터가 비교적 고르게 분포하는 편이라 최소,1,3사분위수,최대로 나눔

hist(boston_data$rm)
boston_data$rm<-cut(boston_data$rm,c(3.561,5.886,6.623,max(boston_data$rm)),include.lowest=T,right=F,labels=c("낮음","중간","높음"))
#정규분포 모양이여서 최소, 1,3사분위수, 최대로 나눔

hist(boston_data$age)
boston_data$age<-cut(boston_data$age,c(2.90,45.02,94.08,max(boston_data$age)),include.lowest=T,right=F,labels=c("낮음","중간","높음"))
#대체로 골고루 있어서 최소,1,3사분위수, 최대로 나눔

hist(boston_data$dis)
boston_data$dis<-cut(boston_data$dis,c(min(boston_data$dis),2.100,5.188,max(boston_data$dis)),include.lowest=T,right=F,labels=c("낮음","중간","높음"))
# 데이터 분포를 보아 최소, 1,3사분위수,최대로 나눔

hist(boston_data$rad)
boston_data$rad<-cut(boston_data$rad,c(1.0,4.0,9.549,max(boston_data$rad)),include.lowest=T,right=F,labels=c("낮음","중간","높음"))
#그림을 보니 10 이하에 데이터들이 뭉쳐있고 24 지점에 뭉쳐있다. 따라서 최소값, 1분위수, 평균, 최대값으로 나눴다

hist(boston_data$tax)
boston_data$tax<-cut(boston_data$tax,c(187,279.0,408.2,max(boston_data$tax)),include.lowest=T,right=F,labels=c("낮음","중간","높음"))
#그림을 보니 500~700사이는 거의 비어있으므로 최솟값, 1사분위수, 중앙값,최댓값으로 나눔(굳이 3사분위수를 할 필요가 없으니)

hist(boston_data$ptratio)
boston_data$ptratio<-cut(boston_data$ptratio,c(12.60,17.40,20.02,max(boston_data$ptratio)),include.lowest=T,right=F,labels=c("낮음","중간","높음"))
#1,3사분위수로 나눔

hist(boston_data$b)
boston_data$b<-cut(boston_data$b,c(0.32,356.67,max(boston_data$b)),include.lowest=T,right=F,labels=c("낮음","높음"))
#뒷부분에 몰려있어서 2부분으로 나눔 최소, 평균값, 최댓값

hist(boston_data$lstat)
boston_data$lstat<-cut(boston_data$lstat,c(1.73,6.95,16.95,max(boston_data$lstat)),include.lowest=T,right=F,labels=c("낮음","중간","높음"))
#최소,1,3사분위수, 최대로 나눔(데이터 분포)

#####################################################################
#transactions 형태로 바꾸기
library(arules)
boston_data.trans<-as(boston_data,"transactions") #transactions 형태로 바꾸기
inspect(boston_data.trans) #바뀐 데이터 보기

#top5 lift순, 신뢰도 순 (medv변수에 영향을 주는) => apriori 함수 적용
rule_top<-apriori(boston_data.trans,parameter = list(support=0.1),appearance = list(default="lhs",rhs='medv=middle'))

inspect(sort(rule_top,by="confidence")[1:5]) #신뢰도 순
inspect(sort(rule_top,by="lift")[1:5]) #향상도 순

























