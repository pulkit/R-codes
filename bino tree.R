  #usage
  #type=1 for call option type = 0 for put option
  #k-> strike price
  #st-> stock price
  #vol->volatility(e.g 30 for 30%)
  #yield->dividend yield(e,g 3 for  3%)
  #rf->risk free rate(e.g 6.5 for 6.5%)
  #time->time to maturity(e.g 3 for 3 months)
  #n->number of steps(e.g 2 for 2 steps)
bino<-function(type,k,st,vol,yield,rf,time,n){
  
  m_price<-matrix(nrow=n+1,ncol=n+1)
  m_payoff<-matrix(nrow=n+1,ncol=n+1)
  u<-exp((vol/100)*sqrt(time/(12*n)))
  print(u)
  d<-1/u
  print(d)
  deltaT<-time/(12*n)
  p<-(exp((rf-yield)*deltaT/100)-d)/(u-d)
  print(p)
  q<-1-p
  price<-st
  n<-n+1

  for(i in 1:n){
    m_price[i,i]<-st*(d^(i-1))
    if(type==1){
    m_payoff[i,i]<-max(m_price[i,i]-k,0)
    }
    else{
      m_payoff[i,i]<-max(k-m_price[i,i],0)
    }
    price<-m_price[i,i]
    for(j in i:n){
     m_price[i,j]<-price
     if(type==1){
       m_payoff[i,j]<-max(m_price[i,j]-k,0)
     }
     else{
       m_payoff[i,j]<-max(k-m_price[i,j],0)
     }
     price<-price*u
    }
  }
  m_final<-matrix(nrow=n,ncol=n)
  for(i in n:1){
    for(j in 1:(i-1)){
      m_final[j,i-1]<-max(exp(-rf*deltaT/100)*(m_payoff[j,i]*p+m_payoff[j+1,i]*q),m_payoff[j,i-1])
    }
  }
  print(q)
  print(m_payoff)
  print(m_final)
  #if fOptions is not installed
  #install.packages("fOptions")
  library(fOptions)
  BinomialTreePlot(m_final,xlab="n",ylab="Option Value",xlim=c(0,8))
}