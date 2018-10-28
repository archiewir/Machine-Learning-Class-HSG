###### Question 1 ######

### Input Data ###
library(MASS)
fix(Boston)
data<-Boston

### Mini-batch Gradient Descent ###
MBGD <-function(x, y, b, e, eta, rows, beta, title, subtitle){
  RSEs = c(0)
  durations = c(0)
  for (i in seq(1:e)){
    startTime = Sys.time()
    datas <- data[sample(nrow(data)),]
    x = datas$lstat
    y = datas$medv
    
    beta = update_beta(x, y, b, beta)
    RSEs[i] = (RSE_f(x, y, rows, beta)) 
    EndTime = Sys.time()
    durations[i] = EndTime - startTime
    if (i>1){
      durations[i] = durations[i] + durations[i-1]
    }
    
  }
  print (paste("Batch = ", toString(b), ", Epoch = ", toString(e)))
  print (paste("B0 = ", toString(round(beta[1], 2)), " and B1 = ", toString(round(beta[2], 2))))
  print (paste("Final RSE = ", toString(round(RSEs[e], 3))))
  print (paste("Cumulative duration = ", toString(round(sum(durations), 6))))
  print ("-------------------------------")
  plot(RSEs, main = title, sub = subtitle, col.sub ="blue", xlab = "epoch")
  plot (durations, main = title, col.sub ="blue", sub = subtitle, xlab = "epoch", ylab = "cumulative duration")
  return (RSEs[e])
}

### RSE Function ###
RSE_f<- function(x, y, rows, beta){
  RSS = 0 
  for (i in seq(1:rows)){
    RSS = RSS + (y[i] - beta[1] - (beta[2]*x[i]) )^2
  }
  out = sqrt(RSS/(rows-2))
  return (out)
}

### Gradient function ###
update_grad<-function(x,y,b, beta){
  grad = rep(0, length(beta))
  for (i in seq(1:b)){
    e0 = y[i] - beta[1] - (beta[2]*x[i])
    e1 = x[i]*e0
    grad[1] = grad[1] + e0
    grad[2] = grad[2] + e1
  }
  return (grad)
}

### Beta Function ### 
update_beta <- function (x, y, b, beta ){
  gradient = update_grad(x, y, b, beta)
  gradient = (-2)*gradient
  beta = beta  - eta*gradient
  return (beta)
}

###### Question 2 and 3 ######

B0 <- 30
B1 <- 0
betas = c(30, 0)
eta <- 0.00001
rows <- nrow(Boston)
b<- 1
epoch <- 20
title = "Q2 and Q3"
subtitle = paste("batch =", toString(b), sep=" ")

MBGD (x, y, b, epoch, eta, rows, betas, title, subtitle)

###### Question 4 and 5 ######
B0 <- 30
B1 <- 0
betas = c(30, 0)
eta <- 0.00001
rows <- nrow(Boston)
b<- 32
epoch <- 20
title = "Q4 and Q5"
subtitle = paste("batch =", toString(b), sep=" ")

MBGD (x, y, b, epoch, eta, rows, betas, title, subtitle)

###### Question 6 and 7 ######
B0 <- 30
B1 <- 0
betas = c(30, 0)
eta <- 0.00001
rows <- nrow(Boston)
b<- c(1,2,4,8,16,32,64)
epoch <- 40
title = "Q6 and Q7"

RSEs = c(0)
for (B in b){
  subtitle = paste("batch =", toString(B), sep=" ")
  RSEs[B] = MBGD (x, y, B, epoch, eta, rows, betas, title, subtitle)
}

plot(RSEs, main = title, sub = "Epoch = 40", col.sub ="blue", xlab = "Batch")
axis(1, at = 1:7, lab = b)
###### Question 8 ######

# We would recommend batch = 16
# As shown in the graph, with the assumed hyperparameters, the RSEs do not decrease substatially when batch > 16.
# It stays near 6.6 ± 0.1, after batch > 16.