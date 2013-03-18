#'@title realized volatility
#'@description
#'\code{rv} is used to calculate the daily realized volatiliy of the returns calculated using 5 minute data
#'@param x the xts time series
rv<-function(x){
  rv<-NULL
  dates<-NULL
  for( i in 1:12){
    for( j in 1:30){
      if(i<10){
        dt<-paste('2010-0',toString(i),'-',toString(j),sep="")
      }
      else{
        dt<-paste('2010-',toString(i),'-',toString(j),sep="")
      }
      k<-x[dt]
      if(length(k)>0 & length(k)<200) {
        sm<-cumsum(k^2)
        s<-sm[length(sm)]
        rv<-c(rv,s)
        dates<-c(dates,dt)
      }
      
    }
  }
  return(rv)
}
