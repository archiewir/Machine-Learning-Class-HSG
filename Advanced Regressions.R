### Data Source ###
L.olympics=t(data.frame(c(1896,12),c(1900,11),c(1904,11),c(1906,11.2),c(1908,10.8),
                        c(1912,10.8),c(1920,10.8),c(1924,10.6),c(1928,10.8),c(1932,10.3),
                        c(1936,10.3),c(1948,10.3),c(1952,10.4),c(1956,10.5),c(1960,10.2),
                        c(1964,10),c(1968,9.95),c(1972,10.14),c(1980,10.25),c(1984,9.99),
                        c(1988,9.92),c(1992, 9.96),c(1996, 9.84),c(2000, 9.87),c(2004,9.85),
                        c(2008, 9.69),c(2012, 9.61),c(2016, 9.83)))

### Split Data ###
index_split <- function(data){
  testing = c(2,6,10,14,18,22,26)
  x_train<- c()
  y_train <- c()
  x_test <- c()
  y_test <- c()

  for (i in seq(1:nrow(data))){
    if (match(i, testing, nomatch = -1)> 0){
      x_test[[length(x_test)+1]] = L.olympics[i,1]
      y_test[[length(y_test)+1]] = L.olympics[i,2]
    }
    else{
      x_train[length(x_train)+1] = L.olympics[i,1]
      y_train[length(y_train)+1] = L.olympics[i,2]
    }
  }

  train = rbind(x_train, y_train)
  train = t(train)
  train = data.frame(train)
  colnames(train) = c("year", "time")
  
  test = rbind(x_test, y_test)
  test= t(test)
  test = data.frame(test)
  colnames(test) = c("year", "time")
  return (list("train" = train, "test" = test))
}

random_split <- function(data){
  datas <- data[sample(nrow(data)),]
  index = 7
  x_train<- c()
  y_train <- c()
  x_test <- c()
  y_test <- c()
  
  for (i in seq(1:nrow(datas))){
    if (i <= index){
      x_test[[length(x_test)+1]] = datas[i,1]
      y_test[[length(y_test)+1]] = datas[i,2]
    }
    else{
      x_train[length(x_train)+1] = datas[i,1]
      y_train[length(y_train)+1] = datas[i,2]
    }
  }
  
  train = rbind(x_train, y_train)
  train = t(train)
  train = data.frame(train)
  colnames(train) = c("year", "time")
  
  test = rbind(x_test, y_test)
  test= t(test)
  test = data.frame(test)
  colnames(test) = c("year", "time")
  return (list("train" = train, "test" = test))
}

RSE_f <- function(test,sol){
  RSS = 0
  n = nrow(test)
  cf = rep (0, 11)
  
  for(j in seq(1:length(sol))){
    cf[j] = sol[j]
  }
  
  for (i in seq(1:n)){
    pred = 0
    for (k in seq(1:length(sol))){
      pred = pred + sol[k]*(test$year[i])^(k-1)
    }
    RSS = RSS + (test$time[i] - pred)^2
  }
  out = sqrt(RSS/(n-2))
  return (out)
}

### Exercise 1 ###
df <- index_split(L.olympics)

### Question 1.1 (Linear Regression) ###

lm.fit = lm(time ~ year, data = df$train)
solution1 = coef(lm.fit)
solution1[is.na(solution1)] = 0
RSE = RSE_f(df$test, solution1)
print (paste("RSE = ", toString(round(RSE,8))))

### Question 1.2 (Quadratic Regression) ###

lm.fit = lm(time~poly(year, 2, raw = TRUE), data = df$train )
solution2 = coef(lm.fit)
solution2[is.na(solution2)] = 0
RSE = RSE_f(df$test, solution2)
print (paste("RSE = ", toString(round(RSE,8))))

### Question 1.3 (degree-3 polynomial) ###

lm.fit = lm(time~poly(year, 3, raw = TRUE), data = df$train )
solution3 = coef(lm.fit)
solution3[is.na(solution3)] = 0
RSE = RSE_f(df$test, solution3)
print (paste("RSE = ", toString(round(RSE,8))))

### Question 1.4 (degree-4 polynomial) ###

lm.fit = lm(time~poly(year, 4, raw = TRUE), data = df$train )
solution4 = coef(lm.fit)
solution4[is.na(solution4)] = 0
RSE = RSE_f(df$test, solution4)
print (paste("RSE = ", toString(round(RSE,8))))

### Question 1.5 (degree-10 polynomial) ###

lm.fit = lm(time~poly(year, 10, raw = TRUE), data = df$train )
solution10 = coef(lm.fit)
solution10[is.na(solution10)] = 0
RSE = RSE_f(df$test, solution10)
print (paste("RSE = ", toString(round(RSE,8))))

### Question 1.6 (Cross-Validation) ###

RSE_LN = c()
RSE_QD = c()

for (c in seq(1:2)){    
  ### to produce better results for cross-validation, a higher number of epoch should be used (eg. 100, not 2).
  df = random_split(L.olympics)
  
  lm.fit = lm(time ~ year, data = df$train)
  solution1 = coef(lm.fit)
  solution1[is.na(solution1)] = 0
  RSE_LN[c] = RSE_f(df$test, solution1)
  
  lm.fit = lm(time~poly(year, 2, raw = TRUE), data = df$train )
  solution2 = coef(lm.fit)
  solution2[is.na(solution2)] = 0
  RSE_QD[c] = RSE_f(df$test, solution2)
  
}

print (paste("RSE for Linear Regression = ", toString(round(mean(RSE_LN),8))))
print (paste("RSE for Quadratic Regression = ", toString(round(mean(RSE_QD),8))))

## The model with the lower RSE, would be said to have performed better. But due to the small number of epoch in 
## running this cross-validation test, we cannot provide a convincing answer. On the average, when running this 
## cross validaion test at epoch = 100, quadratic regression performs better.

### Exercise 2 ###

wbdc <- get(load("C:\\Users\\Archie Wiranata\\Documents\\University\\Semester 6\\8330 Machine Learning\\wdbcData.RData"))
## change location of file accordingly ##

### Question 2.1 (Transform classes to binary) ###

wbdc$diagnosis <- as.character(wbdc$diagnosis)
wbdc$diagnosis[which(wbdc$diagnosis=="M")] <- "1"
wbdc$diagnosis[which(wbdc$diagnosis=="B")] <- "0"
wbdc$diagnosis <- as.numeric(wbdc$diagnosis)

### Question 2.2 (Logistic Regression) ###

glm.fit = glm(diagnosis ~ perimeter_mean + concaveP_mean, family = binomial, data = wbdc)
summary(glm.fit)

### Question 2.3 (List of predictions) ###

pred = predict(glm.fit, type="response")
m = mean(pred)
v = var(pred)
SD = sd(pred)

print (pred[1:10]) ## showing the first ten predicions
print(paste("Mean = ", toString(round(m,4))))
print(paste("Variance = ", toString(round(v,4))))
print(paste("Standard Deviation = ", toString(round(SD,4))))

## These probabilities, stored in variable pred, represent the probability of success in predicting a patient's 
## diagnosis using the glm.fit model. With a mean and standard deviation of 32.76% and 42.42%, the model is not 
## performing so well. Perhaps substituting the variable used in the our predictor model (glm.fit) with other 
## variables in WBDC dataset, we may be able to produce better results than that of now.