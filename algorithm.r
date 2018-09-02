#알고리즘
#------------------------------------------------------
#1번

#(1)
fun_1=function(){ #이기면 1 return 지면 0 return 함수
  a=sum(sample(1:6,2,replace=T)) #첫번째 시도의 합
if(a==7 | a==11){ #승리
  return(1)
  } else if(a==2 | a==3 | a==12){ #패배
    return(0)
  } else { #합이 7 or a와 같을때 까지 반복
    while(TRUE){
      b = sum(sample(1:6,2,replace=T)) #한번 더 추출
      if(b==a) { #승리
        return(1)
      } else if(b==7){ #패배
        return(0)
      }
    }
  }
}
fun_1()
b=0
i=0
while(i<=100){
  b=fun_1()+b
  i=i+1
}
b/i
#(2)-----------------------------
rm(list=ls())

i=0
b=0

for(i in 1:10000){
  ky=12
  sy=9
  while(ky!=0 & sy!=0){
    print(ky)
    if(fun_1()==1){
      ky=ky+1
      sy=sy-1
    } else{
      ky=ky-1
      sy=sy+1
    }
  }
  if(ky>0)
    b=b+1
}
i
b
b/i

#------------------------------------------------------
#2번

Mat1<-matrix(c(1,0,1,1,1,
               0,0,0,1,1,
               0,1,1,1,1,
               0,1,1,1,1,
               0,1,1,1,1),5,5,byrow=T)
Mat2<-matrix(c(1,0,1,1,1,
               1,1,1,1,1,
               0,1,1,1,1,
               0,1,1,1,1,
               0,1,1,1,1),5,5,byrow=T)

Largest=function(M){
  k=0
  for(i in 2:length(M[,1])){
    for(j in 2:length(M[,1])){
      if(M[i,j]==1){ #1을 만났을때
        M[i,j]=min(M[i-1,j-1],M[i-1,j],M[i,j-1])+1 #그림상 2*2행렬에서 그 1을 가장 오른쪽 아래 기준으로 두고 위,왼쪽 대각선, 왼쪽 중 최소값+1한 값을 채워넣으며 누적해감
        k=ifelse(k>M[i,j],k,M[i,j]) # 계산된 값보다 k가 크면 k, 아니면 값(가장 큰 값으로 갱신됨)
      }
    }
  }
  return(k*k)
}

Largest(Mat1)

#------------------------------------------------------
#3번

x=c(10,20,5,30,15)
x=c(20,1,30,10,10)
x=c(5,2,3,4,6,7,8)

chopchop=function(x){
  n=length(x)-1
  M=matrix(100000,n,n)
  for(i in 1:n){
    for(j in 1:n){
      if(i==j){
        M[i,j]=0
      }
    }
  }
  
  for(a in 1:n-1){ #a : 대각선
    for(i in 1:(n-a)) {  #i : 행
      j = i + a #j : 열
      for(k in i:j-1){
        M[i,j] = min(M[i,j],M[i,k]+M[k+1,j]+x[i]*x[k+1]*x[j+1]) #DP알고리즘
      }
    }   
  }
  return(M[1,n]) #오른쪽 아래로 향하는 대각선을 기준으로 계속 누적해서 오른쪽 위쪽 대각방향으로 올라가며 오른쪽 위의 맨끝 행렬의 원소를 리턴
}
chopchop(x)

#------------------------------------------------------
#4번

#A의 역행렬 = 1/det(A) * adj(A)
#adj(A) = t(Cofactor(A))
#Cofactor(A) = (-1)^(i+j)*(Minor(A,i,j))

A=matrix(c(1,0,1,
           0,1,2,
           -1,0,4),3,3,byrow=T)

Minor=function(A,i,j){ #minor matrix 함수
  x=0
  n=sqrt(length(A))
  B=matrix(0,n-1,n-1)
  for(a in 1:n){
    for(b in 1:n){
      if(a!=i & b!=j){ #Minor matrix : 해당 행,열이 포함된 줄은 제외하고 나머지로 구성된 행렬
        x<-c(x,A[a,b]) #우선 벡터로
      }
    }
  }
  B=matrix(x[2:((n-1)^2+1)],n-1,n-1,byrow=T) #행렬로 바꾸기
  return(B)
}

Cofactor=function(A){ #Cofactor 함수
  y=0
  n=sqrt(length(A))
  C=matrix(0,n,n)
  for(i in 1:n){
    for(j in 1:n){
      y<-c(y,((-1)^(i+j))*det(Minor(A,i,j)))
    }
  }
  C=matrix((y[2:(n^2+1)]),n,n,byrow=T)
  return(C)
}


b=A%*%x
x=(matrix(c(1,2,3)))

solveEquation<-function(A,x,b){
  adj=t(Cofactor(A))
  if(x==0){#input으로 A,b가 들어올경우 -> x 반환
    return((1/det(A))* adj %*% b)
  } else if(b==0){#input으로 A,x가 들어올 경우 -> b반환
    return(A%*%x)
  } else if(x!=0 & b!=0){ #input으로 A,x,b가 들어옴
    if(A%*%x==b){
      print("Correct")
    } else{
      print("Incorrect")
    }
  }
}

solveEquation(A,x,b)
















