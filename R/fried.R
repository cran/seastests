#' Friedman Rank test
#' 
#' Test for seasonality in a time series.
#' @param x time series
#' @param freq Frequency of the time series
#' @param diff Shall the differenced series be tested?
#' @param residuals Shall the residuals of an ARIMA model be tested?
#' @param autoarima Use automatic instead of a (0,1,1) ARIMA model?
#' @details If residuals=FALSE the autoarima settings are ignored.
#' @details If residuals=TRUE, a non-seasonal ARIMA model is estimated for the time series. And the residuals of the fitted model are used as input to the test statistic. If an automatic order selection is used, the Hyndman-Khandakar algorithm is employed with max(p)=max(q) <= 3.
#' @references Friedman, M. (1937). The Use of Ranks to Avoid the Assumption of Normality Implicit in the Analysis of Variance. Journal of the American Statistical Association 32 (200), 675-701.
#' @references Hyndman, R. J. and Y. Khandakar (2008). Automatic Time Series Forecasting: The forecast Package for R. Journal of Statistical Software 27 (3), 1-22.
#' @references Ollech, D. and Webel, K. (2020). A random forest-based approach to identifying the most informative seasonality tests. Deutsche Bundesbank's Discussion Paper series 55/2020.
#' @author Daniel Ollech
#' @examples fried(ts(rnorm(120, 10,10), frequency=12))
#' fried(ts(rnorm(1200, 10,10), frequency=7))
#' @export


fried <- function(x, freq=NA, diff=T, residuals=F, autoarima=T) {
  
  # Frequency of time series
  if(is.na(freq)) {
    if (any(class(x)=="ts")) {freq <- stats::frequency(x)} else {
      if (any(class(x)=="xts")) {freq <- freq_xts(x)} else {
        stop("Do not know the frequency of the time series.")
      }}}
  if(freq<2){stop(paste("The number of observations per cycle (usually years) is", freq, "and thus too small."))}
  
  # Transformations
  if (diff & residuals) {warning("The differences of the residuals of a non-seasonal ARIMA model are computed and used. It may be better to either only take the differences or use the residuals.")}
  if (residuals) {
    if (autoarima) {
      maxorder <- ifelse(freq < 8, 1,3)
      driftallow= freq < 8
      model <- tryCatch(forecast::auto.arima(x, max.P=0, max.D=0, max.Q=0, max.p=3, max.q=3, seasonal=F, stepwise=F, max.order=maxorder, allowdrift=driftallow), error=function(e) e)
      if(inherits(model, "error")) {
        model <- tryCatch(stats::arima(x, order = c(0, 1, 1)), error=function(e) e) 
        if(inherits(model, "error")) {
          x <- x; warning("Could not estimate any ARIMA model, original series is used.")}} else {
            x <- stats::resid(model)}} else {
              model <- tryCatch(stats::arima(x, order=c(0,1,1))) 
              if(inherits(model, "error")) {x <- x; warning("Could not estimate any ARIMA model, original series is used.")} else {
                x <- stats::resid(model)}}}  else {model=NA}
  
  if (diff) {y <- diff(x)} else {y <- x}
  
  # Test statistic
  rows <- length(y) %/% freq
  columns <- freq
  
  yMAT <- matrix(xts::last(y, length(y) %/% freq * freq), rows, columns, byrow=T)
  Rank <- apply(yMAT, 1, rank)
  meanRank <- apply(Rank, 1, mean)
  SSD <- sum((meanRank - 0.5*(freq+1))^2)
  
  Chi_krit <- 12 * rows / (columns * (columns+1)) * SSD
  Pval <- 1-stats::pchisq(Chi_krit, columns-1)
  
  
  
  
  
  
  # Return
  out <- list(stat=Chi_krit, Pval=Pval, test="Friedman rank", model=model)
  class(out) <- "seastests"
  
  
  return(out)
}
