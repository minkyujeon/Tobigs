################################################################################
## 0. 시행해 주세요
rm(list=ls())
setwd('C:/Users/good/Desktop/투빅스/4주차(의사결정나무,앙상블,나이브베이즈)/NBC/과제')
work1 <- read.table('work1.txt', header = TRUE)
work2 <- read.table('work2.txt', header = TRUE)
work3 <- read.table('work3.txt', header = TRUE)
test <- read.table('test.txt', header = TRUE)

converting <- function(mydata) {
  for(i in c(1, 3, 4, 6, 7)) mydata[,i] <- factor(mydata[,i])
  return(mydata)
}

work1 <- converting(work1)
work2 <- converting(work2)
work3 <- converting(work3)
test <- converting(test)

#################################################################################
## 1. 초기화
PARAMS <- list(wife_edu_C1 = vector(mode = 'numeric', 4), #wife_work가 0일때
               husband_edu_C1 = vector(mode = 'numeric', 4),
               husband_job_C1 = vector(mode = 'numeric', 4),
               living_index_C1 = vector(mode = 'numeric', 4),
               wife_age_C1 = c(0, 8.2272),
               children_C1 = c(0, 2.3585),
               wife_edu_C2 = vector(mode = 'numeric', 4), #wife_work가 1일때
               husband_edu_C2 = vector(mode = 'numeric', 4),
               husband_job_C2 = vector(mode = 'numeric', 4),
               living_index_C2 = vector(mode = 'numeric', 4),
               wife_age_C2 = c(0, 8.2272),
               children_C2 = c(0, 2.3585),
               prior = vector(mode = 'numeric', 2),
               prev_cnt = c(0, 0))

##################################################################################
## 2. 모수 업데이트 함수

ParmasUpdate <- function(D, PARAMS) {
  #MLE방식으로 (연속형 : 평균) / (범주형 : 각 범주의 확률과 prior의 확률)
  do<-sum(D$wife_work=="0") #전체 일을 하는 개수
  do_not<-sum(D$wife_work=="1") #전체 일을 안하는 개수
  
  D_do<-D[D$wife_work=="0",]
  D_do_not<-D[D$wife_work=="1",]
  
  PARAMS$prev_cnt[1]=PARAMS$prev_cnt[1]+do #prev 누적 업데이트
  PARAMS$prev_cnt[2]=PARAMS$prev_cnt[2]+do_not
  
  PARAMS$prior<-c(PARAMS$prev_cnt[1]/sum(PARAMS$prev_cnt),PARAMS$prev_cnt[2]/sum(PARAMS$prev_cnt)) #prior 누적된 prev로 업데이트
  
  pre<-PARAMS$prev_cnt[1]-do #일을 하는 경우에서 이전 상황(누적을 위해)
  total<-PARAMS$prev_cnt[1]
  
  pre_2<-PARAMS$prev_cnt[2]-do_not #일을 하지 않는 경우에서 이전 상황(누적을 위해)
  total_2<-PARAMS$prev_cnt[2]
  
  ##########################일을 하는(=0) 경우 #######################
  PARAMS$wife_edu_C1[1]<-((PARAMS$wife_edu_C1[1]*pre)+(sum(D_do$wife_edu=="1")))/total
  PARAMS$wife_edu_C1[2]<-((PARAMS$wife_edu_C1[2]*pre)+(sum(D_do$wife_edu=="2")))/total
  PARAMS$wife_edu_C1[3]<-((PARAMS$wife_edu_C1[3]*pre)+(sum(D_do$wife_edu=="3")))/total
  PARAMS$wife_edu_C1[4]<-((PARAMS$wife_edu_C1[4]*pre)+(sum(D_do$wife_edu=="4")))/total
  
  PARAMS$husband_edu_C1[1]<-((PARAMS$husband_edu_C1[1]*pre)+(sum(D_do$husband_edu=="1")))/total
  PARAMS$husband_edu_C1[2]<-((PARAMS$husband_edu_C1[2]*pre)+(sum(D_do$husband_edu=="2")))/total
  PARAMS$husband_edu_C1[3]<-((PARAMS$husband_edu_C1[3]*pre)+(sum(D_do$husband_edu=="3")))/total
  PARAMS$husband_edu_C1[4]<-((PARAMS$husband_edu_C1[4]*pre)+(sum(D_do$husband_edu=="4")))/total
  
  PARAMS$husband_job_C1[1]<-((PARAMS$husband_job_C1[1]*pre)+(sum(D_do$husband_job=="1")))/total
  PARAMS$husband_job_C1[2]<-((PARAMS$husband_job_C1[2]*pre)+(sum(D_do$husband_job=="2")))/total
  PARAMS$husband_job_C1[3]<-((PARAMS$husband_job_C1[3]*pre)+(sum(D_do$husband_job=="3")))/total
  PARAMS$husband_job_C1[4]<-((PARAMS$husband_job_C1[4]*pre)+(sum(D_do$husband_job=="4")))/total
  
  PARAMS$living_index_C1[1]<-((PARAMS$living_index_C1[1]*pre)+(sum(D_do$living_index=="1")))/total
  PARAMS$living_index_C1[2]<-((PARAMS$living_index_C1[2]*pre)+(sum(D_do$living_index=="2")))/total
  PARAMS$living_index_C1[3]<-((PARAMS$living_index_C1[3]*pre)+(sum(D_do$living_index=="3")))/total
  PARAMS$living_index_C1[4]<-((PARAMS$living_index_C1[4]*pre)+(sum(D_do$living_index=="4")))/total
  
  PARAMS$wife_age_C1[1]<-((PARAMS$wife_age_C1[1]*pre)+(sum(D_do$wife_age)))/total
  
  PARAMS$children_C1[1]<-((PARAMS$children_C1[1]*pre)+(sum(D_do$children)))/total
  
  ##########################일을 안 하는(=1) 경우 #######################
  
  PARAMS$wife_edu_C2[1]<-((PARAMS$wife_edu_C2[1]*pre_2)+(sum(D_do_not$wife_edu=="1")))/total_2
  PARAMS$wife_edu_C2[2]<-((PARAMS$wife_edu_C2[2]*pre_2)+(sum(D_do_not$wife_edu=="2")))/total_2
  PARAMS$wife_edu_C2[3]<-((PARAMS$wife_edu_C2[3]*pre_2)+(sum(D_do_not$wife_edu=="3")))/total_2
  PARAMS$wife_edu_C2[4]<-((PARAMS$wife_edu_C2[4]*pre_2)+(sum(D_do_not$wife_edu=="4")))/total_2
  
  PARAMS$husband_edu_C2[1]<-((PARAMS$husband_edu_C2[1]*pre_2)+(sum(D_do_not$husband_edu=="1")))/total_2
  PARAMS$husband_edu_C2[2]<-((PARAMS$husband_edu_C2[2]*pre_2)+(sum(D_do_not$husband_edu=="2")))/total_2
  PARAMS$husband_edu_C2[3]<-((PARAMS$husband_edu_C2[3]*pre_2)+(sum(D_do_not$husband_edu=="3")))/total_2
  PARAMS$husband_edu_C2[4]<-((PARAMS$husband_edu_C2[4]*pre_2)+(sum(D_do_not$husband_edu=="4")))/total_2
  
  PARAMS$husband_job_C2[1]<-((PARAMS$husband_job_C2[1]*pre_2)+(sum(D_do_not$husband_job=="1")))/total_2
  PARAMS$husband_job_C2[2]<-((PARAMS$husband_job_C2[2]*pre_2)+(sum(D_do_not$husband_job=="2")))/total_2
  PARAMS$husband_job_C2[3]<-((PARAMS$husband_job_C2[3]*pre_2)+(sum(D_do_not$husband_job=="3")))/total_2
  PARAMS$husband_job_C2[4]<-((PARAMS$husband_job_C2[4]*pre_2)+(sum(D_do_not$husband_job=="4")))/total_2
  
  PARAMS$living_index_C2[1]<-((PARAMS$living_index_C2[1]*pre_2)+(sum(D_do_not$living_index=="1")))/total_2
  PARAMS$living_index_C2[2]<-((PARAMS$living_index_C2[2]*pre_2)+(sum(D_do_not$living_index=="2")))/total_2
  PARAMS$living_index_C2[3]<-((PARAMS$living_index_C2[3]*pre_2)+(sum(D_do_not$living_index=="3")))/total_2
  PARAMS$living_index_C2[4]<-((PARAMS$living_index_C2[4]*pre_2)+(sum(D_do_not$living_index=="4")))/total_2
  
  PARAMS$wife_age_C2[1]<-((PARAMS$wife_age_C2[1]*pre_2)+(sum(D_do_not$wife_age)))/total_2
  
  PARAMS$children_C2[1]<-((PARAMS$children_C2[1]*pre_2)+(sum(D_do_not$children)))/total_2
  
  return(PARAMS)
}

##################################################################################
## 3. predict 함수

predict.1 <- function(newdata, params) {
  rst=c()
  
  for(i in 1:nrow(newdata)){
    
    multi_c1=1
    multi_c2=1
    c1=0;c2=0
    
    ####################c1인경우####################
    c1=dnorm(as.numeric(newdata[i,2]),mean=params$wife_age_C1[1],sd=params$wife_age_C1[2])
    c1=c(c1,dnorm(as.numeric(newdata[i,5]),mean=params$children_C1[1],sd=params$children_C1[2])) #children(연속형)
    c1=c(c1,params$wife_edu_C1[newdata$wife_edu[i]])
    c1=c(c1,params$husband_edu_C1[newdata$husband_edu[i]])
    c1=c(c1,params$husband_job_C1[newdata$husband_job[i]])
    c1=c(c1,params$living_index_C1[newdata$living_index[i]])
    c1=c(c1,params$prior[1])
    for(j in 1:ncol(test)){
      multi_c1<-multi_c1*c1[j]
    }
    #####################c2인경우######################
    c2=dnorm(as.numeric(newdata[i,2]),mean=params$wife_age_C2[1],sd=params$wife_age_C2[2])
    c2=c(c2,dnorm(as.numeric(newdata[i,5]),mean=params$children_C2[1],sd=params$children_C2[2]))
    c2=c(c2,params$wife_edu_C2[newdata$wife_edu[i]])
    c2=c(c2,params$husband_edu_C2[newdata$husband_edu[i]])
    c2=c(c2,params$husband_job_C2[newdata$husband_job[i]])
    c2=c(c2,params$living_index_C2[newdata$living_index[i]])
    c2=c(c2,params$prior[2])
    for(j in 1:ncol(test)){
      multi_c2<-multi_c2*c2[j]
    }
    rst<-c(rst,ifelse(multi_c1>multi_c2,"0","1"))
  }
  return(as.factor(rst))
}

###################################################################################
## 4. 결과 확인
library(naivebayes)

# 모수 추정이 같은가?
PARAMS_1 <- ParmasUpdate(work1, PARAMS)
model.1 <- naive_bayes(work1[,-1], work1[,1])

#
PARAMS_2 <- ParmasUpdate(work2, PARAMS_1)
work12<-rbind(work1,work2)

table(work1$wife_work)
model.2<-naive_bayes(work12[,-1],work12[,1])
PARAMS_3 <- ParmasUpdate(work3, PARAMS_2)

work123 <- rbind(work1, work2, work3)
model.3 <- naive_bayes(work123[,-1], work123[,1] )

## 예측값이 같은가?
predict.1(test[,-1], PARAMS_3)
predict(model.3, newdata=test[,-1])

