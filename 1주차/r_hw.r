############################# 과제 ############################################

# 1. 주어진 cow_data에서 is_edible 이라는 새로운 열을 추가하고
# 나이(age)가 50(개월)이상이면서 등급(grade)이 "3" 또는 "등외"이라면 "폐기용", 아니면 "식용"을 기입하는 함수를 작성해주세요.
# 함수의 매개변수는 대상 데이터 1개 ex) my_function(cow_data)

# 2."1++" 등급이 가장 많은 세 지역(변수 address)을 구하고 / 각 지역별로 "1++"등급이 츙 몇 마리인지 보여주세요 (시/군/구 단위로 구해주세요)

# 3. 위 세 도시 별로 각 등급마다 소의 평균가격(price)을 구해주세요

# 4. 위 세 도시 별로 총 몇 마리의 소가 도축됐는지 월 단위로 구하고 그래프로 표현해주세요 (변수 slaughter_date가 도축된 날짜를 의미함)


setwd("C:/Users/good/Desktop/투빅스/1주차(R,파이썬,git)/R 전처리 수업")
rm(list=ls())
cow_data=read.csv("cow_data.csv")
head(cow_data)

#------------------------------------------------------
#1번

library(dplyr)
cow_data_1=cow_data #복사
my_function=function(cow_data_1){
  cow_data_1 %>%
    mutate(is_edible=ifelse((cow_data_1$age>=50 & (cow_data_1$grade=="3" | cow_data_1$grade=="등외")),"폐기용","식용"))
}
cow_data_1<-my_function(cow_data_1)
head(cow_data_1)

#################################
attach(cow_data)
cow_data$is_edible=ifelse((age>=50 & (grade=="3" | grade=="등외")),"폐기용","식용")
cow_data
#################################

#------------------------------------------------------
#2번
library(dplyr)
library(stringr)
rm(list=ls())
cow_data=read.csv("cow_data.csv")
cow_data1=cow_data #복사
head(cow_data1)

cow_data1$address<-str_split(cow_data1$address, pattern=" ",n=3,simplify=T) #공백을 기준으로 3개의 덩어리로 나누기
cow_data1$address<-cow_data1$address[,2] #주소의 두번째 덩어리만 주소로 바꾸기
cow_data2<-subset(cow_data1,grade=="1++") #등급이 1++인것들만 뽑기
a<-head(sort(table(cow_data2$address),decreasing=T),3) #address를 table을 이용하여 빈도표를 만들고 거기서 위에서 3번째까지 head

#------------------------------------------------------
#3번
head(cow_data1)
b<-subset(cow_data1,cow_data1$address=="정읍시")  #주소가 정읍시인 데이터 뽑기
c<-subset(cow_data1,cow_data1$address=="고흥군") 
d<-subset(cow_data1,cow_data1$address=="안성시") 

b$price <- as.numeric(b$price)
c$price <- as.numeric(c$price)
d$price <- as.numeric(d$price)

lapply(split(x=b$price,f=b$grade),mean) #가격을 grade를 그룹으로 평균구하기
lapply(split(x=c$price,f=c$grade),mean)
lapply(split(x=d$price,f=d$grade),mean)

#------------------------------------------------------
#4번
cow_data1$slaughter_date=as.character(cow_data1$slaughter_date)

a<-table(substr(cow_data1$slaughter_date,5,6))
a
plot(a,col="blue",
     axes=F,xlab="Month",ylab="마리 수")
axis(1,at=c(1:12),las=1,
     labels=c(1:12))
axis(2,las=1)



