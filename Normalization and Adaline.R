### Exercise 2 ###

#Normalization function
normalize<-function(x) {
  numerator <- x - mean(x)
  denominator <- sd(x)
  ans <- (numerator/denominator)
  return (ans)}
    
#insert dataset into matrix
mx<-matrix(c(2,3,1,3,5,-1,  3,-1,0,1,3,-1,  1,-1,-1,1,1,-1), nrow=3, ncol=6, byrow=TRUE,
             dimnames=list(c("x1","x2","y"),c("D1","D2","D3","D4","D5","D6")))

#normalize
mx<-as.data.frame(t(mx))
norm_mx<-as.data.frame(lapply(mx[1:2], normalize))
norm_mx<-cbind.data.frame(norm_mx,y=mx$y)

#input data for algorithm
x = norm_mx[,c(1,2)]
y = norm_mx[,3]

#algorithm
perceptron<-function(x,y,eta, iter){
    w = rep(0,3)
    error = rep(0,iter)
    
    for (j in 1:iter){
      for (k in 1:length(y)){
        z<- (w[1]+ (w[2]*x[k,1]) + (w[3]*x[k,2]))
      if (z<0){
        pred<--1
      }else{
        pred<-1
      }
        adj<-eta*(y[k]-pred)*c(1,x[k,1], x[k,2])
        w<-w+adj
        if (y[k]-pred!=0.0){
          error[j]<-error[j]+1
      }
    }
    }
    
    print(error)
    print (w)
    return(w)
}

w<-perceptron(x,y, eta=0.1, iter=10)