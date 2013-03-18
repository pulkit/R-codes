#'@title brownian motion simulation
#'@description \code{brownian_motion} generates a brownian motion
#' by taking the time period(T) , the time intervals(dt),volatility(sigma),mean(mean)
#'@param T Time period
#'@param dt time intervals
#'@param sigma the volatility
#'@param mean mean
brownian_motion<-function(T,dt,sigma,mean){
  t<-c(1:(T/dt))
  r<-rnorm((T/dt))
  br<-sigma*r*sqrt(dt)+mean*t
  plot(br,type="l")
}
