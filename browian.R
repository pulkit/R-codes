brownian_motion<-function(T,dt,sigma,mean){
  t<-c(1:(T/dt))
  r<-rnorm((T/dt))
  br<-sigma*r*sqrt(dt)+mean*t
  plot(br,type="l")
}