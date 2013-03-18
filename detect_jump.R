#'@title  bipower jump detection algorithm
#'@description
#'\code{detect_jump} function is used to detect jumps using bipower jump detection algorithm using returns as the input
#'@param returns the log returns of the high frequency data
detect_jump<-function(returns){
  i<-0
  sums<-0
  j<-0
  l<-0
  cn<-0
  sn<-0
  k<-0
  jumps<-NULL
  #spec3 = ugarchspec(variance.model=list(model="apARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1), include.mean=TRUE))
  #fit_aparch = ugarchfit(data = ts(returns)[1:18913], spec = spec3)
  #rt<-residuals(fit_aparch)
  index<-NULL
  rt<-coredata(returns)
  for(i in c(1:length(rt))){
    if(i>140){
      sums<-0
      j<-0
      for(j in c((i-140+2):(i-1)) ){
        sums<-sums+(abs(rt[j])*abs(rt[j-1]))
        
      }
      sums<-sums/138
      sums<-sqrt(sums)
      cn<-(sqrt(2*log(76))/0.79)-(log(3.14)+log(log(76)))/(2*0.79*sqrt(2*log(76)))
      sn<-1/(0.79*(sqrt(2*log(76))))
      li<-(rt[i])/sums
      l<-(abs(li)-cn)/sn
      if(l>4.6001){
        jumps[k]<-returns[i]
        index<-c(index,i)
        k<-k+1
      }
      
    }
  }
  #print(jumps)
  return(index)
}
