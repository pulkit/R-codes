wiener<-function(t,dt){
  w<-0
  sq<-seq(0,t-dt,dt)
  r<-cumsum(rnorm(t/dt,0,1)*sqrt(dt))*1.5+0.3*dt
  w<-c(w,r)
  plot(w,type="l")
}
