### Exercise 1 ###

library('MASS') #Loaded library and data
fix("Boston")

simple_regression <- function(data){
  
  y=data["medv"]  #y as medv
  x=data["lstat"] #x as lstat
  
  exp_y <- colMeans(y)     #expected values of x and y
  exp_x <- colMeans(x)
  
  lm.fit=lm(medv~lstat, data=Boston)
  lm.fit
  coef(lm.fit)
  #Solution for least squares approach
  
  RSS<-deviance(lm.fit) #RSS
  RSS
  
  n<-nrow(data)       #computation for RSE
  RSE<-sqrt(RSS/(n-2))
  RSE
  
  y_list <- setNames(split(y, seq(nrow(y))), colnames(y))  #Computation for TSS
  TSS <- 0
  for (val in y_list){
    TSS=(TSS+(val-exp_y)^2)
  }
  TSS
  
  R2 <- 1-(RSS/TSS)   #computation for R2
  R2
  return (coef(lm.fit))

}

simple_regression(Boston) 

### Exercise 2 ###

training <- Boston[1:354,]    #spliting the training and testing of dataset
testing <- Boston[354:n,]


solution <- simple_regression(training)
  

y=testing["medv"]  #y as medv
x=testing["lstat"] #x as lstat
exp_y <- colMeans(y)
n <- nrow(y)
output = 0

for (i in seq(from=1, to=n, by=1)){
  output <- output + (y[1,i] - solution[1] - (solution[2]*x[1,i]))
}