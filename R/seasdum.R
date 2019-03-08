#' F-Test on seasonal dummies
#' 
#' Test for seasonality in a time series based on joint significance seasonal dummies in a non-seasonal ARIMA model.
#' @param x time series
#' @param freq Frequency of the time series
#' @param autoarima Use automatic instead of a (0,1,1) ARIMA model?
#' @details A RegARIMA model is estimated with (0,1,1)+Seasonal dummies if autoarima=FALSE (default) or (p,d,q)+Seasonal dummies if autoarima=TRUE, (p,d,q) selected by Hyndman-Khandakar algorithm with max(p)=max(q) <= 3. Then the tests checks whether the seasonal dummies are jointly different from zero, i.e. whether deterministic seasonality can be detected in the time series.
#' @references Hyndman, R. J. and Y. Khandakar (2008). Automatic Time Series Forecasting: The forecast Package for R. Journal of Statistical Software 27 (3), 1-22.
#' @references Maravall, A. (2011). Seasonality Tests and Automatic Model Identification in TRAMO-SEATS. Bank of Spain. 
#' @author Daniel Ollech
#' @examples seasdum(ts(rnorm(120, 10,10), frequency=12))
#' seasdum(ts(rnorm(70, 10,10), frequency=7))
#' @export


seasdum <- function(x, freq=NA, autoarima=FALSE) {
  if(is.na(freq)) {
    if (any(class(x)=="ts")) {freq <- stats::frequency(x)} else {
      if (any(class(x)=="xts")) {freq <- freq_xts(x)} else {
        stop("Do not know the frequency of the time series.")
      }}}
  
  .seasonaldummies <- function(x) {
    dummies = c(1,rep(c(rep(0,(freq-1)),1), length(x)))[1:((freq-1)*length(x))]
    mat = matrix(dummies, ncol=(freq-1), byrow=T)
    return(mat)
  }

  .chi2 <- function(model) {
    mat = model$var.coef
    t(utils::tail(model$coef, (freq-1))) %*% solve(mat[(nrow(mat)-(freq-1)+1):nrow(mat), (ncol(mat)-(freq-1)+1):ncol(mat)]) %*% utils::tail(model$coef, (freq-1))
  }

  
  if (autoarima) {
    model <- forecast::auto.arima(x, max.p=3, max.q=3, max.d=2, seasonal=FALSE, allowdrift=T, xreg=.seasonaldummies(x))} else {
  model <- stats::arima(x, order=c(0,1,1), xreg=cbind(1:length(x), .seasonaldummies(x)))}
  
  # Test Statistic
  f_krit <-  as.numeric(.chi2(model)/(freq-1) * (length(x) - 1  - freq)/ (length(x) - 1))
  
  Pval <- 1-stats::pf(f_krit, (freq-1), (length(x) - 1  - freq))
  
  # Return
  out <- list(stat=f_krit, Pval=Pval, test="SeasonalDummies", model=model)
  class(out) <- "seastests"
  
  
  return(out)
}



