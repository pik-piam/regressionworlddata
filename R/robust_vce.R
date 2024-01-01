#' @title  robust_vce
#' @description returns robust var-cov estimate
#' @param x regression model
#' @return a robust estimte of variance-covariance matrix and corresponding t-value and p-value for estimated coefficients
#' @export
#' @importFrom sandwich sandwich
#' @importFrom lmtest coeftest
#' @author  Xiaoxi Wang
#' 
robust_vce <- function(x){
    covariance <- sandwich(x)
    coef <- coeftest(x,covariance)
    return(robust_estiamte =list(coef = coef, covariance = covariance))
}
