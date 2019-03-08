#' Check model used in OCSB test
#' 
#' Test the residuals of the model used for the OCSB test for serial correlation.
#' @param x results of ocsb test
#' @param plot boolean, should barplot be printed?
#' @details The residuals of the model used for the OCSB test should ideally be white noise. Here the Ljung-Box statistic is calculated and shown for all lags up to 2 times the frequency of the series. Be aware that the Ljung-Box statistic is a 'cumulative test'. For instance, the p-value of the Ljung-Box statistic for lag 3 is based on the null hypothesis, that the autocorrelations of the first three lags are jointly zero. 
#' @references Box, G. and G. Jenkins (1970). Time Series Analysis: Forecasting and Control. San Francisco: Holden-Day.
#' @references Osborn D.R., Chui A.P.L., Smith J., and Birchenhall C.R. (1988). Seasonality and the order of integration for consumption, Oxford Bulletin of Economics and Statistics 50(4):361-377.
#' @author Daniel Ollech
#' @examples teststat <- ocsb(ts(rnorm(100, 10,10), frequency=12), nrun=100)
#' check_residuals(teststat)
#' @export


check_residuals <- function(x, plot=F) {

lb <- sapply(1:(2*x$freq), function(y) stats::Box.test(stats::residuals(x$model),lag=y, type="L")$p.value)  
names(lb) <- 1:(2*x$freq)

if (plot) {
  graphics::barplot(lb, main="Ljung-Box test for residual correlation", xlab="lags", ylab="p-value", ylim=c(0,1))
  graphics::abline(h=0.05)
}

out = list(lb, freq=x$freq)
class(out) <- "residualcheck"

return(out)
} 


print.residualcheck <- function(output) {
  keep <- round(output[[1]][c(1,2,3,output[[2]],2*output[[2]])],3)
  
  use1 <- substr("   ",1, (3-nchar(output[[2]])+1))
  use2 <- substr("   ",1, (3-nchar(2*output[[2]])+1))
  
  cat("P-values of Ljung-Box statistic for selected lags: \n")
  cat(paste0("Lag 1:","   "), keep[1], "\n")
  cat(paste0("Lag 2:","   "), keep[2], "\n")
  cat(paste0("Lag 3:","   "), keep[3], "\n")
  cat(paste0("Lag ", output[[2]],":",use1), keep[4], "\n")
  cat(paste0("Lag ", 2*output[[2]],":",use2), keep[5], "\n")
  
  cat(ifelse(all(keep > 0.05), "\n The null of independence of the residual could not be rejected at 5-percent significant level. No indication of serial correlation at lags shown. \n", "\n The Ljung-Box test indicates serial correlation at some of the lags shown. \n"))
}


       