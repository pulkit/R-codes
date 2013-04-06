TriplePenacle<-function(R,geometric = TRUE, weights = NULL,...)
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
#get_cumul_return<-function(x,geometric){
#   if(geometric)
#      Return.cumulative = cumprod(1+x)
#  else
#      Return.cumulative = 1 + cumsum(x)
#          return(Return.cumulative)
#}
get_minq<-function(R){

            mu = mean(x[,column, na.rm = TRUE)
                sd = StdDev(x)
                phi = ar(x)$ar



}

get_TuW<-function(){
            mu = mean(x[,column, na.rm = TRUE)
                sd = StdDev(x)
                phi = ar(x)$ar



}
golden_search_method<-function(){

}

charts.TriplePenacle<-function(){
}

Table.TriplePenacle<-function(){
}




