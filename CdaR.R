CDD<-function (R, weights = NULL, geometric = TRUE, invert = TRUE, 
          p = 0.95, ...) 
{
  p = .setalphaprob(p)
  if (is.vector(R) || ncol(R) == 1) {
    R = na.omit(R)
    nr = nrow(R)
    # checking if nr*p is an integer
    if((p*nr) %% 1 == 0){
    drawdowns = as.matrix(Drawdowns(R))
    drawdowns = drawdowns(order(drawdowns),decreasing = TRUE)
    # average of the drawdowns greater the (1-alpha).100% largest drawdowns 
    result = (1/((1-p)*nr(R)))*sum(drawdowns[((1-p)*nr):nr])
    else{
      #linear programming problem  has to be solved to calculate the CDaR
    }
    if (invert) 
      result <- -result

    }  
    return(result)
  }
  else {
    R = checkData(R, method = "matrix")
    if (is.null(weights)) {
      result = matrix(nrow = 1, ncol = ncol(R))
      for (i in 1:ncol(R)) {
        result[i] <- CDD(R[, i, drop = FALSE], p = p, 
                         geometric = geometric, invert = invert, ... = ...)
      }
      dim(result) = c(1, NCOL(R))
      colnames(result) = colnames(R)
      rownames(result) = paste("Conditional Drawdown ", 
                               p * 100, "%", sep = "")
    }
    else {
      portret <- Return.portfolio(R, weights = weights, 
                                  geometric = geometric)
      result <- CDD(portret, p = p, geometric = geometric, 
                    invert = invert, ... = ...)
    }
    return(result)
  }
}
