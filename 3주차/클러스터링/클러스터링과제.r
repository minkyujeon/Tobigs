# 9기 과제 k-means 구현

rm(list=ls())
data <- iris[,1:4]
str(data)
head(data)

k=5

k_means<-function(data,k) # data랑 군집 몇개로 나눌지 k를 받아서
{
  centroid_idx<-sample(1:nrow(data),k) #forgy 방식으로 데이터 인덱스 뽑기
  centroid<-data[centroid_idx,] #centroid 만들기
  
  while(1){
    a<-rbind(centroid,data) #거리를 재기 위해 centroid와 data를 합침
    dist<-as.matrix(dist(a)) #거리를 잼
    dist<-dist[,1:k] #centroid 개수 이후는 버림
    idx<-apply(dist,1,which.min) #각 거리 중 최소가 되는 index를 부여(centroid1,2,...k개 중에 어떤 centroid와의 거리가 가장 가까운지 index부여)
    
    added<-cbind(a,idx) #idx열 추가
    added<-added[(1+k):(150+k),] #맨 위에 합친 k개의 centroid빼고 평균을 구하기 위함
    
    #각각의 평균들 구하기
    new_1<-aggregate(Sepal.Length~idx,added,mean)
    new_2<-aggregate(Sepal.Width~idx,added,mean)
    new_3<-aggregate(Petal.Length~idx,added,mean)
    new_4<-aggregate(Petal.Width~idx,added,mean)
    
    new_centroid<-cbind(new_1,new_2,new_3,new_4)[c(2,4,6,8)] #idx 1, 2, 3제거(의미 없음, centroid와 형태 맞추기 위해)
    
    if(centroid-new_centroid<1e-5){ #이전것과 차이가 거의 없다면(완전히 같은것이 없을경우 무한루프를 돌 수 있기때문에 입실론만큼의 차이를 허용)
      return(list(added$idx)) #idx 리턴
    }
    else{
      centroid=new_centroid #update
    }
  }
}

k_means(data,5)
