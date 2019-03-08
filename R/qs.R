#' QS test
#' 
#' Test for seasonality in a time series.
#' @param x time series
#' @param freq Frequency of the time series
#' @param diff Shall the differenced series be tested?
#' @param residuals Shall the residuals of ARIMA model be tested?
#' @param autoarima Use automatic instead of a (0,1,1) ARIMA model?
#' @details If residuals=FALSE the autoarima settings are ignored.
#' @details If residuals=TRUE, a non-seasonal ARIMA model is estimated for the time series. And the residuals of the fitted model are used as input to the test statistic. If an automatic order selection is used, the Hyndman-Khandakar algorithm is employed with max(p)=max(q) <= 3.
#' @references Hyndman, R. J. and Y. Khandakar (2008). Automatic Time Series Forecasting: The forecast Package for R. Journal of Statistical Software 27 (3), 1-22.
#' @references Maravall, A. (2011). Seasonality Tests and Automatic Model Identification in TRAMO-SEATS. Bank of Spain. 
#' @author Daniel Ollech
#' @examples qs(ts(rnorm(120, 10,10), frequency=12))
#' qs(ts(rnorm(1200, 10,10), frequency=7))
#' @export


qs <- function(x, freq=NA, diff=T, residuals=F, autoarima=T) {
  
  # Frequency of time series
  if(is.na(freq)) {
    if (any(class(x)=="ts")) {freq <- stats::frequency(x)} else {
    if (any(class(x)=="xts")) {freq <- freq_xts(x)} else {
      stop("Do not know the frequency of the time series.")
    }}}
  
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
      x <- stats::resid(model)}}} else {model=NA}
  
  if (diff) {y <- diff(x)} else {y <- x}
  
  # Test statistic
  rho <- stats::acf(as.numeric(y)[stats::complete.cases(as.numeric(y))], lag.max = freq*2, plot=F)$acf[-1,,][c(freq, 2*freq)]
  if (any(rho <=0)) {rho <- c(0,0)}
  
  N <- length(y[stats::complete.cases(y)])
  QS <- N*(N+2) * (rho[1]^2/(N-freq) + rho[2]^2/(N-freq*2))
  
  Pval <- 1-stats::pchisq(QS, 2)
  
  # Return
  out <- list(stat=QS, Pval=Pval, test="QS", model=model)
  class(out) <- "seastests"
  
  
  return(out)
}
