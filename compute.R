compute<-function(rt_csv){

  library(xts)
  library(xts)
  library(rugarch)
  library(TSA)
#Data entry and cleaning and format conversion

rt_comp<-as.xts(rt_csv[,3],as.POSIXct(strptime(paste(rt_csv$Date,rt_csv$Time),"%m/%d/%Y %H:%M:%S",tz="IST")),header=T)
#rt_comp<-rt_comp["T09:30/T15:30"]

#Jump removal exercise
#rt_comp<-rt_comp[!is.na(index(rt_comp))]
#indexs<-detect_jump(rt_comp)
#rt_comp<-rt_comp[-indexs]


#Calculation of order of ARMA model
eacf(rt_comp)
m<-arma(rt_comp,order=c(1,1))
m
Box.test(residuals(m), lag = 1, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)


#Garch Modelling
spec3 = ugarchspec(variance.model=list(model="apARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(2,2), include.mean=TRUE))
fit_aparch <<-ugarchfit(data = ts(rt_comp)[1:length(rt_comp)], spec = spec3)
show(fit_aparch)
spec3 = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(2,2), include.mean=TRUE))
fit_egarch <<- ugarchfit(data = ts(rt_comp)[1:length(rt_comp)], spec = spec3)
show(fit_egarch)
spec3 = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(2,2), include.mean=TRUE))
fit_gjrgarch <<- ugarchfit(data = ts(rt_comp)[1:length(rt_comp)], spec = spec3)
show(fit_gjrgarch)
spec3 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(2,2)))
fit_sgarch <<- ugarchfit(data = ts(rt_comp)[1:length(rt_comp)], spec = spec3)
show(fit_sgarch)
rt_comp_final<-merge(rt_comp,as.data.frame(fit_aparch)$sigma)

#Calculation of realized volatility
rv_comp<-rv(rt_comp_final[,1])
rv_aparch<-rv(rt_comp_final[,2])

#Final Plot containing realized volatility and estimated volatility 
plot(ty="l",rv_comp,ylab=" Volatility",xlab="Time",main="Infosys Volatlity without jumps");lines(rv_aparch,col=2,main="Aparch");legend(30,29.0,c("Aparch","realized volatility"),c(2,"black"),"solid")

  mz <- lm(rv_comp~rv_aparch) 
  print(summary(mz))
  print(linearHypothesis(mz, c("(Intercept) = 0", "rv_aparch = 1"))) 
return(rt_comp_final)

}