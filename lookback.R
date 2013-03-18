#'@title  Look Back Option 
#' 
#' @description
#' \code{lookback} calculates the option price of a lookback option  
#' of type fixed/floating and call/put option . 
#' 
#' @param Type call or put "c" for call "p" for put
#' @param Type2 fixed or floating "fx" for fixed and "fl" for floating
#' @param Time Time to maturity
#' @param S spot price of the underlying
#' @param SminorMax Max or minimum spot price of the underlying during the duration
#' @param r Risk free rate of return
#' @param q dividend yield
#' @param sigma standard deviation
#' @param K Strike Price to be input for fixed lookback option
#' @keywords maximum drawdown,brownian motion
#' @examples 
#' lookback("c","fix",1000,1100,0.25,0.08,0.01,0.30,900)
#' 
#' @references  Options, Futures and Other Derivatives
#'  
lookback<-function (Type,Type2, S, SminorMax, Time, r,q, sigma,K) 
{
  if(Type2=="fix" & Type=="c"){
    m<-max(SminorMax,K)
    a1 = (log(m/S) + (-r+q + sigma^2/2) * Time)/(sigma * sqrt(Time))
    a2 = a1 - sigma * sqrt(Time)
    a3 = (log(m/S)+(r-q-sigma^2/2)*Time)/(sigma*sqrt(Time))
    Y1 = -(2*(r-q-sigma^2/2)*log(m/S))/sigma^2
    price <- -S*exp(-q*Time)*pnorm(a2) +S*exp(-q*Time)*sigma^2/(2*(r-q))*pnorm(-a2)+m*exp(-r*Time)*(pnorm(a1)-sigma^2/(2*(r-q))*exp(Y1)*pnorm(-a3))
    price=price+S*exp(-q*Time)-K*exp(-r*Time)
  }
  if(Type2=="fix" & Type=="p"){
    m<-min(SminorMax,K)
    a1 <- (log(S/m) + ((r-q) + sigma^2/2) * Time)/(sigma * sqrt(Time))
    a2 <- a1 - sigma * sqrt(Time)
    a3 <- (log(S/m)+((-r+q)+sigma^2/2)*Time)/(sigma*sqrt(Time))
    Y1 <- -(2*(r-q-sigma^2/2)*log(S/m))/sigma^2
    price <- S*exp(-q*Time)*pnorm(a1) - S*exp(-q*Time)*sigma^2/(2*(r-q))*pnorm(-a1)-m*exp(-r*Time)*(pnorm(a2)-sigma^2/(2*(r-q))*exp(Y1)*pnorm(-a3))
    price<-price-S*exp(-q*Time)+K*exp(-r*Time)
  }
  
  if (Type2=="fl" & Type == "c"){
    m<-SminorMax
    a1 <- (log(S/m) + ((r-q) + sigma^2/2) * Time)/(sigma * sqrt(Time))
    a2 <- a1 - sigma * sqrt(Time)
    a3 <- (log(S/m)+((-r+q)+sigma^2/2)*Time)/(sigma*sqrt(Time))
    Y1 <- -(2*(r-q-sigma^2/2)*log(S/m))/sigma^2
    price <- S*exp(-q*Time)*pnorm(a1) - S*exp(-q*Time)*sigma^2/(2*(r-q))*pnorm(-a1)-m*exp(-r*Time)*(pnorm(a2)-sigma^2/(2*(r-q))*exp(Y1)*pnorm(-a3))
  }
  if (Type2=="fl" & Type == "p"){
    m<-SminorMax
  a1 = (log(m/S) + (-r+q + sigma^2/2) * Time)/(sigma * sqrt(Time))
  a2 = a1 - sigma * sqrt(Time)
  a3 = (log(m/S)+(r-q-sigma^2/2)*Time)/(sigma*sqrt(Time))
  Y1 = -(2*(r-q-sigma^2/2)*log(m/S))/sigma^2
  price <- -S*exp(-q*Time)*pnorm(a2) +S*exp(-q*Time)*sigma^2/(2*(r-q))*pnorm(-a2)+m*exp(-r*Time)*(pnorm(a1)-sigma^2/(2*(r-q))*exp(Y1)*pnorm(-a3))
}
  param = list()
  param$Type = Type
  param$S = S
  param$SminorMax = SminorMax
  param$Time = Time
  param$r = r
  param$q = q
  param$sigma = sigma
  write.table(param)
  print(price)
}