### Exercise 1 ##

## Dataset ##
x = c(1,1)
n7 = data.frame(c(0,0,3,2,-3,0,5), c(2,-2,-1,3,1,5,4))
colnames(n7) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7")
y = c(1,1,2,3,3,2,2)
cl = c(1,2,3)

## Compute the distances ## 
distance<- function(x, n7){
  d = c()
  for (i in seq(1:nrow(n7))){
    d[length(d)+1] = sqrt((x[1] - n7[i,1])^2 + (x[2] - n7[i,2])^2)
  }
  return (d)
}

d = distance(x,n7)

## Compute weight 3 ##
weight3 <- function (d){
  o = c()
  for (i in seq(length(d))){
    o[length(o)+1]= (0.3973557/d[i])
  }
  return (o)
}

## Weights ##
w1 = rep((1/7), 7)
w2 = c((9/34), (9/34), (6/34), (4/34), (3/34), (2/34), (1/34))
w3 = weight3(sort(d))
weight = data.frame(w1,w2,w3)

## Sort distances descending ##
df = data.frame(d,y)
df.sorted = df[order(df[,1]),]

## Compute probabilty function ## 
prob <- function (df, w, cl){
  n = 0
  for (i in seq(length(w))){
     if (df.sorted[i, 2] == cl){
       n = n + w[i]
     }
  }
  n = n/sum(w)
  return (n)
}

## weight = 1, class = 1 ##
p=prob(df.sorted, w1, 1)
print (paste("Probabilty of x being ", toString(c), " (using weight ", toString(w), ") = ", 
             toString(round(p,6), sep ="")))

## weight = 1, class = 2 ##
p=prob(df.sorted, w1, 2)
print (paste("Probabilty of x being ", toString(c), " (using weight ", toString(w), ") = ", 
             toString(round(p,6), sep ="")))

## weight = 1, class = 3 ##
p=prob(df.sorted, w1, 3)
print (paste("Probabilty of x being ", toString(c), " (using weight ", toString(w), ") = ", 
             toString(round(p,6), sep ="")))

## weight = 2, class = 1 ##
p=prob(df.sorted, w2, 1)
print (paste("Probabilty of x being ", toString(c), " (using weight ", toString(w), ") = ", 
             toString(round(p,6), sep ="")))

## weight = 2, class = 2 ##
p=prob(df.sorted, w2, 2)
print (paste("Probabilty of x being ", toString(c), " (using weight ", toString(w), ") = ", 
             toString(round(p,6), sep ="")))

## weight = 2, class = 3 ##
p=prob(df.sorted, w2, 3)
print (paste("Probabilty of x being ", toString(c), " (using weight ", toString(w), ") = ", 
             toString(round(p,6), sep ="")))

## weight = 3, class = 1 ##
p=prob(df.sorted, w3, 1)
print (paste("Probabilty of x being ", toString(c), " (using weight ", toString(w), ") = ", 
             toString(round(p,6), sep ="")))

## weight = 3, class = 2 ##
p=prob(df.sorted, w3, 2)
print (paste("Probabilty of x being ", toString(c), " (using weight ", toString(w), ") = ", 
             toString(round(p,6), sep ="")))

## weight = 3, class = 3 ##
p=prob(df.sorted, w3, 3)
print (paste("Probabilty of x being ", toString(c), " (using weight ", toString(w), ") = ", 
             toString(round(p,6), sep ="")))

### Exercise 2 ###
## Question 1 ##
w5 = 1
w4 = 2*w5
w3 = 2*w4
w2 = 2*w3
w1 = 2*w2

w = c(w1,w2,w3,w4,w5)
s = sum(w)
w = w/s
print(paste("The corresponding weights are", w ))

## Question 2 ##
## (d*/d1) + (d*/d2) + (d*/d3) + (d*/d4) = 1
## d* = (d1d2d3 + d2d3d4 + d1d3d4 + d1d2d4) / d1d2d3d4