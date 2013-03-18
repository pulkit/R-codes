#'@title Pricing option using the finite difference method
#'@desciption 
#'\code{findiff} function is used to find the price of the option using the 
#'finite difference method by taking the rate of return , standard deviation,
#'the stock price,the time to maturity,the strike price
#'@param r risk free rate of return
#'@param sd standard deviation
#'@param t time to maturity
#'@param x strike price
findiff<-function(r,sd,s,t,x){
  dy<-(log(100*s)-log(1/s))/200
  y1<-seq(log(1/s),log(2*s),dy)
  nr<-length(y1)
  dt<-t/200
  r<-r/100
  sd<-sd/100
  t1<-seq(0,t,dt)
  nc<-length(t1)
  m1<-matrix(nrow=nr,ncol=nc)
  w1<-0.5*((sd/dy)^2-(r-(sd^2)/2)/dy)*dt
  w3<-0.5*((sd/dy)^2+(r-(sd^2)/2)/dy)*dt
  w2<-1-((sd/dy)^2)*dt
  for(i in 1:nr){
    m1[i,nc]<-max(0,exp(y1[nr+1-i])-x)
  }
  m1[nr,]<-0
  for(i in (nc-1):1){
    for(j in (nr-1):1){
      if(j==1){
        m1[1,i]<-exp(y1[nr])*dy+m1[2,i]
      }
      else{
      m1[j,i]<-(w1*m1[j-1,i+1]+w2*m1[j,i+1]+w3*m1[j+1,i+1])/(1+r*dt)
      }
    }
  }
  nn<-m1[(nr-which(abs(y1-log(s)) == min(abs(y1-log(s))))),1]
  print(nn)
  
}
