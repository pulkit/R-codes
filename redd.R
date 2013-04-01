rollDrawdown<-function(R, geometric = TRUE, weights = NULL, rf, h,...)
{
    x = checkData(R)
    columns = ncol(x)
    columnnames = colnames(x)
    REDD<-function(x,geometric){
        if(geometric)
            Return.cumulative = cumprod(1+x)
        else Return.cumulative = 1 + cumsum(x)
        l = length(Return.cumulative)
        REM = max(Return.cumulative*(1+rf)^(l-c(1:l)))
        result = 1 - Return.cumulative[l]/REM
    }

    for(column in 1:columns){
        column.drawdown <- na.skip(x[,column],FUN = apply.rolling(x[,column],width = h, FUN = REDD, geometric = geometric), geometric = geometric)
        if(column == 1)
            rolldrawdown = column.drawdown
        else rolldrawdown = merger(rolldrawdown, column.drawdown) 
    }
    colnames(rolldrawdown) = columnnames
    rolldrawdown = reclass(drawdown, x)
    return(rolldrawdown)
}








  


