#'@title Probabilistic Sharpe Ratio
#'@description
#'
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
        x = checkData(R)
        sr = srm(x, Rf, p,"StdDev")
        n = nrow(x)
        sd = StdDev(x)
        sk = skewness(x)
        kr = kurtosis(x)
        PSR = pnorm(((sr - refSR)*(n-1)^(0.5))/(1-sr*sk+sr^2*(kr-1)/4)^(0.5))
        PSR
}

mintrl <- function(R,Rf,p,refSR,...){
    sk = skewness(R)
    kr =kurtosis(R)
    sr = srm(R, Rf, p, "StdDev")
    MinTRL = 1 + (1 - sk*sr + ((kr-1)/4)*sr^2)*(qnorm(p)/(sr-refSR))^2
    MinTRL

}

}
