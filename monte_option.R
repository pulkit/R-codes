monte_option<-function(type,N,M,TTM,sigma,mu,St,K,r,barrier = 0){
  rr<-rnorm(N*M)
  ss<-matrix(nrow=N,ncol=M)
  sum<-0
  k<-1
  dt<-TTM/N
  
  # Antithetic Variable Technique
  for(i in 1:M/2){
    price<-St
    for(j in 1:N){
        ss[j,2*i-1]<-price*exp((mu-(sigma^2/2))*(dt)+(rr[k]*sigma*sqrt(dt)))
        k<-k+1
        price<-ss[j,2*i-1]
        ss[j,2*i]<-price*exp((mu-(sigma^2/2))*(dt)+(rr[k]*sigma*sqrt(dt)))
        price<-ss[j,2*i]
        k = k+1
    }
    # For Plain Vanilla Option
    if(type=="plainvannila"){
    sum<-sum+max((price-K),0)
    }
    
    # For Asian Option 1
    if(type=="asian1_call"){
      sum<-sum+max((mean(ss[,i])-K),0)
    }
    
    if(type=="asian1_put"){
      sum<-sum+max((mean(ss[,i])-K),0)
    }
    # For Asian Option 2
    if(type=="asian2_call"){
      sum<-sum+max((price-mean(ss[,i])),0)
    }
    
    if(type=="asian2_put"){
      sum<-sum+max((price-mean(ss[,i])),0)
    }
    
    # For Knockin Option
    if(type=="knockin_call"){
      if(ss[,i]>barrier){
        sum<-sum+max(max(price-k,0),0)        
      }
    }

    if(type=="knockin_put"){
      if(ss[,i]>barrier){
        sum<-sum+max(max(k-price,0),0)        
      }
    }
    
    #For Knockout Option
    if(type=="knockout_call"){
      if(ss[,i]<barrier){
        sum<-sum+max(max(price-k,0),0)        
      }
    }
    
    if(type=="knockout_put"){
      if(ss[,i]<barrier){
        sum<-sum+max(max(k-price,0),0)        
      }
    }
  }
  print((sum/M)*exp(-r*TTM))
}