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
#' @param R an xts, vector, matrix, data frame, timeseries , or zoo object of asset return
#' @param geometric utilize geometric chaining(TRUE) or simple/arithmetic chaining
#' @param weights weights for the portfolio
#' 
#' @reference Bailey, David H. and Lopez de Prado, Marcos, Drawdown-Based Stop-#' Outs and the ‘Triple Penance’ Rule(January 1, 2013)

TriplePenace<-function(R,geometric = TRUE, weights = NULL,...)
{
    x = checkData(R)
        columns = ncol(x)
        columnnames = colnames(x)
        tp<-matrix(nrow = 2 ,ncol = columns)
        tp$rownames<-c("MinQ","TuW")
        tp$colnames <-columnnames
        for(column in 1:columns){
            column.MinQ<-na.skip(x[,column],FUN = get_minq,geometric = geometric)
            column.TuW<-na.skip(x[,column],FUN = get_TuW,geometric  = geometric)
            tp[1,column] = column.MinQ
            tp[2,column] = column.TuW
         }  
 return(tp)

}
get_minq<-function(R,confidence){

    # DESCRIPTION:
    # A function to get the maximum drawdown for first order serially autocorrelated
    # returns from the quantile function defined for accumulated returns for a 
    # particular confidence interval

    # Inputs:
    # R: The function takes Returns as the input.
    #
    # confidence: The confidence interval of the input.

    x = checkData(R)
    mu = mean(x[,column, na.rm = TRUE)
    sd = StdDev(x)
    phi = ar(x)$ar
    dp0 = x[1] 
    q = 0
    bets = 0
    while(q < 0){
        bets = bets + 1
        q = getQ(bets, phi, mu, sigma, dp0, confidence)
    minQ = golden_section(x,0,bets,TRUE,getQ)
    return(minQ[1])
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


get_TuW<-function(R){

    # DESCRIPTION:
    # A function to generate the  time under water
    #
    # Inputs:
    # R: The function takes Returns as the input.
    #
    # confidence: Confidence level of the quantile function


    x = checkData(R)
    mu = mean(x[,column, na.rm = TRUE)
    sd = StdDev(x)
    phi = ar(x)$ar
    dp0 = x[1] 
    confidence = 0.95
    q = 0
    bets = 0
    while(q < 0){
        bets = bets + 1
        q = getQ(bets, phi, mu, sigma, dp0, confidence)
    TuW = golden_section(x,0,bets,TRUE,diff)
    return(TuW[2])
}

diff<-function(bets,phi,mu,sigma,dp0,confidence){
    return(abs(getQ(bets,phi,mu,sigma,dp0,confidence)))
}

golden_section<-function(R,a,b,minimum = TRUE,function_name,...){

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
        return(c(sign*f1,bets))
    }
    else{
        return(c(sign*f2,bets))
    }
}   
        



charts.TriplePenacle<-function(){
}

Table.TriplePenacle<-function(){
}




