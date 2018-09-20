##################### 1.Gradient Descent 구현 과제입니다. ##########################
# 시각화용 코드입니다.
smoothing <- function(vec)
{
  vec1 <- c(vec[-1], vec[length(vec)])
  vec2 <- c(vec[1], vec[-length(vec)])
  return((vec1 + vec2 + vec) / 3)
}

visualize_loss <- function(loss_log)
{
  for(i in 1:100)
  {
    loss_log <- smoothing(loss_log)
    plot(loss_log)
    Sys.sleep(0.01)
  }
}
# 여기까지 그냥 실행시켜 주세요!

##############################################################################################################
#                                                                                                            #
#   이번 과제는 gradient descent를 이용한, 선형 회귀 구현 입니다. 아래에 비어있는 식을 채워주시면 됩니다!    #
#                                                                                                            #
##############################################################################################################
# 단순회귀 구현
x <- rnorm(1000, 0)
y <- 2 * x + 1
w <- 0.001 
b <- 0.001
lr <- 0.01
loss_log <- c()
######################################
for(i in 1:length(x)){
  w=w-lr*(-2*(y[i]-(w*x[i]+b))*x[i])
  b=b-lr*(-2*(y[i]-(w*x[i]+b)))
  loss<-(y[i]-(w*x[i]+b))^2
  loss_log[i] <- loss
}
######################################
visualize_loss(loss_log)
if(max(abs(w-2), abs(b-1)) < 0.1)
{
  print("정답입니다!")
}else{
  print("모델을 수정하거나, 초기값, 파라미터를 수정해보세요!")
}

#다중회귀 구현(변수 11개)
x <- as.data.frame(matrix(rnorm(5000,0), nrow = 500, ncol = 10))
y <- x$V1 * 1 + x$V2 * 2 + x$V3 * 3 + x$V4 * 4 + x$V5 * 5 + x$V6 * 6 + x$V7 * 7 + x$V8 * 8 + x$V9 * 9 + x$V10 * 10 + 11
w <- rnorm(10,0)
b <- rnorm(1,0)
lr <- 0.01
loss_log <- c()

wb=as.matrix(c(b,w)) #b 아래에 w데이터
z=as.matrix(cbind(1,x)) #x데이터 앞에 1로 이루어진 열 추가

############################################
for(i in 1:nrow(x))
{
  wb=wb-lr*((-2*t(z)%*%(y-(z%*%wb)))/length(y)) 
  loss<-mean(((y[i]-(z[i,]%*%wb))^2))
  loss_log[i] <- loss
}

############################################
visualize_loss(loss_log)
if(max(abs(wb[2:11]-1:10), abs(wb[1]-11)) < 0.5)
{
  print("정답입니다!")
}else{
  print("모델을 수정하거나, 초기값, 파라미터를 수정해보세요!")
}
a <- matrix(c(1,2,3,4), ncol = 2)
b <- c(1,2)
c <- rep(b,2)
a*c
#다중회귀 구현(변수 n개)
linear_regression <- function(n)
{
  x <- as.data.frame(matrix(rnorm(50*n*n,0), nrow = 50*n, ncol = n))
  y <- rep(0, 50*n)
  for(i in 1:(50*n))
  {
    y[i] <- sum(x[i,]*(1:n)) + (n+1)
  }
  w <- rnorm(n,0)
  b <- rnorm(1,0)
  lr <- 0.01
  loss_log <- c()
  
  wb=as.matrix(c(b,w)) #b 아래에 w데이터
  z=as.matrix(cbind(1,x)) #x데이터 앞에 1로 이루어진 열 추가
  
  for(i in 1:nrow(x))
  {
    wb=wb-lr*((-2*t(z)%*%(y-(z%*%wb)))/length(y)) #위에 11개일때랑 같음
    loss<-mean(((y[i]-(z[i,]%*%wb))^2))
    loss_log[i] <- loss
  }
  
  visualize_loss(loss_log)
  #if(max(abs(w-1:n), abs(b-n-1)) < 0.5)
  if(max(abs(wb[2:(n+1)]-1:n), abs(wb[1]-(n+1))) < 0.5) #wb가 맨 위가 b이고 그 아래가 w인 데이터이므로 
    #범위를 wb를 기준으로 재설정했다 맨 위는 b이므로 2번째부터 n+1행까지
  {
    print("정답입니다!")
  }else{
    print("모델을 수정하거나, 초기값, 파라미터를 수정해보세요!")
  }
  return(list(w = w, b = b))
}

linear_regression(10)
linear_regression(15)
linear_regression(20)

  
  
  
  





