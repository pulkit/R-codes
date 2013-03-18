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