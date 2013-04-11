#' @title
#' Triple Penance Rule
#'
#' @description
#' \code{TriplePenance} calculates the Maximum drawdown and the maximum 
#' Time under water for a particular confidence interval. These concepts 
#' are intenately related through the "triple penance" rule which states 
#' that under standard portfolio theory assumptions, it takes three times
#' longer to recover from the expected maximum drawdown than the time it 
#' takes to produce it, with the same confidence level. The framework is
#' generalized to deal with the case of first-order auto-correlated cashflows
#'
#' @param R an xts, vector, matrix, data frame, timeseries , or zoo object of NAVs
#' 
#' @reference Bailey, David H. and Lopez de Prado, Marcos, Drawdown-Based Stop-#' Outs and the ‘Triple Penance’ Rule(January 1, 2013).

TriplePenace<-function(R,confidence,...)
{
    x = checkData(R)
    x = log(x)
    columns = ncol(x)
    columnnames = colnames(x)
    tp = matrix(nrow = 2 ,ncol = columns)
    i = 0 
    for(i in 1:columns){
        column_MinQ = get_minq(x[,i],confidence)
        column_TuW = get_TuW(x[,i],confidence)
        tp[1,i] = column_MinQ
        tp[2,i] = column_TuW
    }  
    return(tp)
}
get_minq<-function(R,confidence){
  
    # DESCRIPTION:
    # A function to get the maximum drawdown for first order serially autocorrelated
    # returns from the quantile function defined for accumulated returns for a 
    # particular confidence interval

    # Inputs:
    # R: The function takes Returns as the input
    #
    # confidence: The confidence interval of the input.
    x = checkData(R)
    x = log(x)
    delta = x - x[-1]
    mu = mean(delta, na.rm = TRUE)
    sigma = StdDev(delta)
    phi = cov(delta[-1],delta[-length(delta)])/(cov(delta[-length(delta)])^2)
         
    dp0 = x[1]
    q = 0
    bets = 0
    while(q <= 0){
        bets = bets + 1
        q = getQ(bets, phi, mu, sigma, dp0, confidence)
    }
    minQ = golden_section(x,0,bets,TRUE,getQ,confidence)
  
    return(minQ$minQ)
}


getQ<-function(bets,phi,mu,sigma,dp0,confidence){

    # DESCRIPTION:
    # A function to get the quantile function for cumulative returns
    # and a  particular confidence interval.
    
    # Inputs:
    # bets: The number fo steps
    #
    # phi: The coefficient for AR[1]
    #
    # mu: The mean of the returns
    #
    # sigma: The standard deviation of the returns
    #
    # dp0: The r0 or the first return
    #
    # confidence: The confidence level of the quantile function

    mean = ((phi^(bets+1)-phi)/(1-phi))*(dp0-mu)+mu*bets
    var = ((sigma/(phi-1))^2)*(((phi^(2*(bets+1))-1)/(phi^2-1))-2*((phi^(bets+1)-1)/(phi-1))+bets +1)
    q = mean + qnorm(confidence)*var^0.5
    return(q)
}


get_TuW<-function(R,confidence){

    # DESCRIPTION:
    # A function to generate the  time under water
    #
    # Inputs:
    # R: The function takes Returns as the input.
    #
    # confidence: Confidence level of the quantile function


    x = checkData(R)
    delta = x - x[-1]
    mu = mean(delta, na.rm = TRUE)
    sigma = StdDev(delta)
    phi = cov(delta[-1],delta[-length(delta)])/(cov(delta[-length(delta)])^2)
    dp0 = delta[1]
    q = 0
    bets = 0
    while(q <= 0){
        bets = bets + 1
        q = getQ(bets, phi, mu, sigma, dp0, confidence)
    }
    TuW = golden_section(x,bets-1,bets,TRUE,diff,confidence)
    return(TuW$TuW)
}

diff<-function(bets,phi,mu,sigma,dp0,confidence){
    return(abs(getQ(bets,phi,mu,sigma,dp0,confidence)))
}

golden_section<-function(R,a,b,minimum = TRUE,function_name,confidence,...){

    # DESCRIPTION
    # A function to perform the golden search algorithm on the provided function

    # Inputs:
    # R: Return series
    #
    # a: The starting point
    #
    # b: The end point
    #
    # minimum: If we want to calculate the minimum set minimum= TRUE(default)
    #
    # function_name: The name of the function 
    x = checkData(R)
    x = log(x)
    delta = x - x[-1]
    mu = mean(delta, na.rm = TRUE)
    sigma = StdDev(delta)
    phi = cov(delta[-1],delta[-length(delta)])/(cov(delta[-length(delta)])^2)
    dp0 = delta[1]  
    FUN = match.fun(function_name)
    tol = 10^-9
    sign = 1 

    if(minimum){
        sign = -1
    }
    N = round(ceiling(-2.078087*log(tol/abs(b-a))))
    r = 0.618033989
    c = 1.0 - r
    x1 = r*a + c*b
    x2 = c*a + r*b
    f1 = sign * FUN(x1,phi,mu,sigma,dp0,confidence)
    f2 = sign * FUN(x2,phi,mu,sigma,dp0,confidence)
    for(i in 1:N){
        if(f1>f2){
            a = x1
            x1 = x2
            f1 = f2
            x2 = c*a+r*b
            f2 = sign*FUN(x2,phi,mu,sigma,dp0,confidence)
        }
        else{
            b = x2
            x2 = x1
            x1 = r*a + c*b
            f1 = sign*FUN(x1,phi,mu,sigma,dp0,confidence)
    }
    }
    if(f1<f2){
        return(list(minQ=sign*f1,TuW=x1))
    }
    else{
        return(list(minQ=sign*f2,TuW=x2))
    }
}   
      
monte_simul<-function(size){
    
  phi = 0.5
  mu = 1
  sigma = 2 
  dp0 = 1
  bets = 25
  confidence = 0.95
  
  q = getQ(bets, phi, mu, sigma, dp0, confidence)
  ms = NULL
  
  for(i in 1:size){
    ms[i] = sum((1-phi)*mu + rnorm(bets)*sigma + delta*phi)
  }
  q_ms = quantile(ms,(1-confidence)*100)
  diff = q - q_ms 

  print(q)
  print(q_ms)
  print(q - q_ms)
}

charts.TriplePenacle<-function(){

}

Table.TriplePenacle<-function(){
}




