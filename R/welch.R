#' Welch seasonality test
#' 
#' Test for seasonality in a time series using Welch's ANOVA test.
#' @param x time series
#' @param freq Frequency of the time series
#' @param diff Shall the differenced series be tested?
#' @param residuals Shall the residuals of an ARIMA model be tested?
#' @param autoarima Use automatic instead of a (0,1,1) ARIMA model?
#' @param rank Use rank of series instead of actual values?
#' @details If residuals=FALSE the autoarima parameter is ignored.
#' @details If rank=TRUE, the test becomes basically a combination of the Kruskall-Wallis and the Welch test.
#' @details If residuals=TRUE, a non-seasonal ARIMA model is estimated for the time series. And the residuals of the fitted model are used as input to the test statistic. If an automatic order selection is used, the Hyndman-Khandakar algorithm is employed with max(p)=max(q) <= 3.
#' @references Kruskal, W. H. and W. A.Wallis (1952). Use of Ranks in One-Criterion Variance Analysis. Journal of the American Statistical Association 47 (260), 583-621.
#' @references Welch, B. L. (1951). On the Comparison of Several Mean Values: An Alternative Approach. Biometrika 38 (3/4), 330-336.
#' @author Daniel Ollech
#' @examples welch(ts(rnorm(120, 10,10), frequency=12))
#' welch(ts(rnorm(1200, 10,10), frequency=7))
#' @export


welch <- function(x, freq=NA, diff=T, residuals=F, autoarima=T, rank=F) {
  
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
                x <- stats::resid(model)}}}  else {model=NA}
  
  if (diff) {y <- diff(x)} else {y <- x}
  
  # Test statistic
  if (any(class(x)=="ts")) {
  year <- floor(round(zoo::index(y),6))
  intra <- (zoo::index(y) - year)*freq+1}
  
  if (any(class(x)=="xts")) {
    intra <- format(zoo::index(y), "%m-%d") }
  
  if (rank) {y <- rank(y, na.last="keep", ties.method="average")}
  
  a <- stats::oneway.test(y ~ as.factor(round(intra)), var.equal=F) 
  
  Chi_krit <- a[[1]]
  Pval <- a$p.value
  
  
  # Return
  out <- list(stat=Chi_krit, Pval=Pval, test="Kruskall Wallis", model=model)
  class(out) <- "seastests"
  
  
  return(out)
}
