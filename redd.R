function(R, geometric = TRUE, weights = NULL, rf, lookback ...)
{
    x = checkData(R)
    columns = ncol(x)
    columnnames = colnames(x)
    REDD<-function(x,geometric){
        if(geometric)
            Return.cumulative = cumprod(1+x)
        else Return.cumulative = 1 + cumsum(x)



  


