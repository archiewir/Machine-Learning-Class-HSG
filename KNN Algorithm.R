### Exercise 1 ###
library(class)

wbdc <- get(load("C:\\Users\\Archie Wiranata\\Documents\\University\\Semester 6\\8330 Machine Learning\\wdbcData.RData"))
## change location of file accordingly ##

wbdc$diagnosis <- as.character(wbdc$diagnosis)
wbdc$diagnosis[which(wbdc$diagnosis=="M")] <- "1"
wbdc$diagnosis[which(wbdc$diagnosis=="B")] <- "-1"
wbdc$diagnosis <- as.numeric(wbdc$diagnosis)

## Question 1.1 (Split the Data) ##
split_data <- function(data, w){
  set.seed(1) # Remove if unnecessary 
  wbdc = wbdc[sample(nrow(wbdc)),]
  rows = nrow(wbdc)
  index = round(rows*w)
  train = wbdc[1:index,]
  test = wbdc[(index+1):rows,]
  return (list(train, test))
}

## Question 1.2 (K-NN Algorithm) ##
KNN_f <- function(data, K){
  perimeter_mean = data.frame(data[1])$perimeter_mean
  concaveP_mean = data.frame(data[1])$concaveP_mean
  train = data.frame(perimeter_mean, concaveP_mean)
  
  perimeter_mean = data.frame(data[2])$perimeter_mean
  concaveP_mean = data.frame(data[2])$concaveP_mean
  test = data.frame(perimeter_mean, concaveP_mean)
  
  diagnosis.train = data.frame(data[1])$diagnosis
  diagnosis.test = data.frame(data[2])$diagnosis
  
  knn.pred = knn(train, test, diagnosis.train , k=K)
  result = table(knn.pred, diagnosis.test)
  accuracy= round((result[1] + result[4]) / sum(result), 4)
  print (paste("Accuracy for (k =", toString(i), "): ", toString(accuracy), 
               " || Error rate = ", toString(1-accuracy), sep=""))
  return (result)
}

## Question 1.3 (ADALINE Algorithm) ##
adaline <- function (data, n, iter){
  perimeter_mean = data.frame(data[1])$perimeter_mean # Preparing training data
  concaveP_mean = data.frame(data[1])$concaveP_mean
  train = data.frame(perimeter_mean, concaveP_mean)
  train = data.frame(scale(train)) # data standardization
  
  perimeter_mean = data.frame(data[2])$perimeter_mean # preparing testing data
  concaveP_mean = data.frame(data[2])$concaveP_mean
  test = data.frame(perimeter_mean, concaveP_mean)
  test = data.frame(scale(test)) # data standardization
  
  diagnosis.train = data.frame(data[1])$diagnosis # y values for training data
  diagnosis.test = data.frame(data[2])$diagnosis # y values for testing data
  
  w = rep(0, 3)
  adaline.pred = c()
  
  for (i in seq(1:(iter%%nrow(train)))){ # training step, determine weights
    error = diagnosis.train[i] - w[1] - (w[2]*(train$perimeter_mean[i])) - (w[3]*(train$concaveP_mean[i]))
    w[1] = w[1] + (n*error)
    w[2] = w[2] + (n*error*train$perimeter_mean[i])
    w[3] = w[3] + (n*error*train$concaveP_mean[i])
  }
  
  for (j in seq(1:nrow(test))){ # testing step, determine accuracy
    pred = w[1] + (w[2]*(test$perimeter_mean[j])) + (w[3]*(test$concaveP_mean[j]))
    if (pred >= 0){
      adaline.pred[length(adaline.pred)+1] = 1 
    }
    else{
      adaline.pred[length(adaline.pred)+1] = -1 
    }
  }
  result = table(adaline.pred, diagnosis.test)
  accuracy= round((result[1] + result[4]) / sum(result), 4)
  print (paste("Accuracy for ADALINE algorithm = ", toString(accuracy), 
               " || Error rate = ", toString(1-accuracy), sep=""))
  return (result)
}

## Question 1.4 Comparing results ##

# Hyperparameters#
k = c(1, 10, 25) # k-values
n = c(0.1, 0.05, 0.02) # learning rates
iter = 750 # Iterations
w = 0.7

data = split_data(wbdc, w)

for (i in k ){
  KNN_f(data, i)
}

for (j in n){
  adaline(data, j, iter)
}

## Adaline explanation ##
## Muliple learning rates were used to compensate of the randomness of the data splitting process.
## This allows us to choose the optimum learning rate and produce better results. 

## Conclusion ##
## From the accuracy and the error rate given from both algorthms, assuming the current hyperparameters,
## it can concluded that the KNN does not perform as well as the ADALINE algorithm.

## ---------------------------------------------------------------------------------------------------------- ##

### Exercise 2 ###
L.olympics=t(data.frame(c(1896,12),c(1900,11),c(1904,11),c(1906,11.2),c(1908,10.8),
                        c(1912,10.8),c(1920,10.8),c(1924,10.6),c(1928,10.8),c(1932,10.3),
                        c(1936,10.3),c(1948,10.3),c(1952,10.4),c(1956,10.5),c(1960,10.2),
                        c(1964,10),c(1968,9.95),c(1972,10.14),c(1980,10.25),c(1984,9.99),
                        c(1988,9.92),c(1992, 9.96),c(1996, 9.84),c(2000, 9.87),c(2004,9.85),
                        c(2008, 9.69),c(2012, 9.61),c(2016, 9.83)))

## Split the data ##
index_split <- function(data, index){
  x_train<- c()
  y_train <- c()
  x_test <- c()
  y_test <- c()
  
  for (i in seq(1:nrow(data))){
    if (match(data[i], year, nomatch=-1) > 0){
      x_test[[length(x_test)+1]] = data[i,1]
      y_test[[length(y_test)+1]] = data[i,2]
    }
    else{
      x_train[length(x_train)+1] = data[i,1]
      y_train[length(y_train)+1] = data[i,2]
    }
  }
  
  train = data.frame(x_train, y_train)
  colnames(train) = c("year", "time")
  
  test = data.frame(x_test, y_test)
  colnames(test) = c("year", "time")
  return (list("train" = train, "test" = test))

}

## Find two smallest values in array
two_mins <- function (arr){
  m1 = min(arr)
  arr = arr[-which.min(arr)]
  m2 = min(arr)
  return(c(m1,m2))
}

## locate minimum value's indexes ##
mins_idx <- function(diff, m){ 
  m1 = match(m[1], diff)
  m2 = match(m[2], diff)
  if (m1 == m2){ # if both minimum value is the same, return the correct index
    m2 = m2+1
  }
  return (c(m1,m2))
}

## Locate possible closest neighbours ##
closeN <- function(data, y){
  diff = c()
  for (j in seq(1:nrow(data$train))){
    diff[j] = (data$train$year[j] - data$test$year[y])
  }
  diff = abs(diff)
  m = two_mins(diff) # Find two minumum distances
  return (mins_idx(diff, m)) # return first and second shortest distance indexes
}

## predict from the 2 closest neighbours ##
twoN <- function (data, idx){
  out = (data$train$time[idx[1]] + data$train$time[idx[2]])/2
  return (out)
}

## Compute RSS for KNN ##
RSS_KNN <- function (data, pred){
  r = 0
  for (i in seq(length(pred))){
    r = r + (data$test$time[i] - pred[i])^2
  }
  return (r)
}

## 2-NN Algorithm ##
NN_f <- function (data){
  pred = c()
  for (i in seq(nrow(data$test))){
    idx = closeN(data, i)
    pred[i] = twoN(data, idx)
  }
  RSS = RSS_KNN(data, pred)
  print (paste("The 2-NN regression RSS =", toString(round(RSS, 6))))
}

## Compute RSS for Linear Regression ##
RSS_regress <- function(data, sol){
  r = 0
  for (i in seq(nrow(data$test))){
    pred = sol[1]+(sol[2]*data$test$year[i])
    r = r + (data$test$time[i] - pred)^2
  }
  return (r)
}

## Linear regression function ##
regress_f <- function (data){
  lm.fit = lm(time ~ year, data = data$train)
  sol = coef(lm.fit)
  RSS = RSS_regress(data, sol)
  print (paste("The linear regression RSS =", toString(round(RSS, 6))))
}  

year = c( 1920, 1948, 1980, 2000)
  
data = index_split(L.olympics, year)
NN_f(data)
regress_f(data)
## 2-NN regression performs better than Linear Regression in this case.

