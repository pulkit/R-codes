function(R, geometric = TRUE, weights = NULL, rf, h, ...)
{
    x = checkData(R)
    columns = ncol(x)
    columnnames = colnames(x)
    REDD<-function(x,geometric){
        if(geometric)
            Return.cumulative = cumprod(1+x)
        else Return.cumulative = 1 + cumsum(x)
        REM = max(x*(1+rf)^(length(x)-c(1:length(x))))
        result = 1 - x[length(x)]/REM
    rollingDrawdown<-rollapply(x, width = h, FUN = REDD)
    return(rollingDrawdown)
    }
}








  


