TriplePenacle<-function(R,geometric = TRUE, weights = NULL,...)
{
    x = checkData(R)

        columns = ncol(x)
        columnnames = colnames(x)


}
get_cumul_return<-function(x,geometric){
    if(geometric)
        Return.cumulative = cumprod(1+x)
    else
        Return.cumulative = 1 + cumsum(x)
            return(Return.cumulative)
}
get_minq<-function(){

}

get_TuW<-function(){

}
golden_search_method<-function(){

}

charts.TriplePenacle<-function(){
}

Table.TriplePenacle<-function(){
}




