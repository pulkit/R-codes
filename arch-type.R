arch_type<-function(rt,sigmat,lambda,n){
  e<-rnorm(n)
  r<-NULL
  sigma<-NULL
  r[1]<-rt
  sigma[1]<-sigmat
  for(i in 2:n){
    sig<-(sigmat^2)*(lambda^(i-1))
    for(j in 1:(i-1)){
      sig<-sig+(1-lambda)*(lambda^(i-1-j))*(r[j]^2)
    }
    #print(sig)
    sigma<-c(sigma,sig)
    rr<-r[i-1]+sqrt(sigma[i])*e[i]
    print(rr)
    r<-c(r,rr)
    
  }
  plot(r,type="l")
  print(r)
}