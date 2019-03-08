#' Obtain the frequency of an xts time series
#' 
#' Estimate the number of periods per year of an xts time series
#' @param series time series
#' @author Daniel Ollech
#' @examples x <- xts::xts(rnorm(100), seq.Date(from=as.Date("2010-01-01"), by="months", length.out=100))
#' frequency(x)
#' @details The function gives back the average number of observations per year calculated on the whole series except for the first and the last year.
#' @export



freq_xts <- function(series) {
  if (!any(class(series) == "xts")) {stop("Series needs to be of class xts")}
  ff <- as.numeric(format(zoo::index(xts::first(series)), "%Y"))+1
  ll <- as.numeric(format(zoo::index(xts::last(series)), "%Y"))-1
  if (ll < ff) {stop("Series needs to contain more than two full years")}
  tester <- series[paste(ff,ll, sep="/")]
  freq <- length(tester) / (ll-ff + 1) 
  return(freq)
}

