ProbSharpeRatio<-
function(R, Rf = 0, p = 0.95, weights = NULL, annualize = FALSE, ...){
    R = checkData(R)

    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)

    if(annualize){
        freq = periodicity(R)
        switch(freq$scale,
            minute = {stop("Data periodicity too high")},
            hourly = {stop("Data periodicity too high")},
            daily = {scale = 252},
            weekly = {scale = 52},
            monthly = {scale = 12},
            quarterly = {scale = 4},
            yearly = {scale = 1}
            )
    }
    else{
        scale = 1
    }

    psr <- function (R,Rf,p,refSR,...){
        xR = Return.excess(R, Rf)
        sr = srm(R, Rf, p,"StdDev")
        n = nrow(R)
        sd = StdDev(R)
        sk = skewness(R)
        kr = kurtosis(R)
        PSR = pnorm(((sr - refSR)*(n-1)^(0.5))/(1-sr*sk+sr^2*(kr-1)/4)^(0.5))
}
}