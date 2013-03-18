#'@title simulation of random walk
#'@description
#'\code{random_walk} generates a random walk by taking the number of steps , 
#'the starting point and the constant as the equation
#'@param n number of steps
#'@param p0 the starting point
#'@param the constant
random_walk<-function(n,p0,c){
  pt<-NULL
  r<-rnorm(n)
  for(i in 1:n){
    l<-0.01+c*p0+r[i]
    pt<-c(pt,l)
    p0<-l
  }
   return(pt)
}
