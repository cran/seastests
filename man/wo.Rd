% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wo.R
\name{wo}
\alias{wo}
\title{WO test}
\usage{
wo(y, freq = NA)
}
\arguments{
\item{y}{time series}

\item{freq}{Frequency of the time series}
}
\description{
Webel-Ollech overall seasonality test that combines results from different seasonality tests.
}
\details{
By default, the WO-test combines the results of the QS-test and the kwman-test, both calculated on the residuals of an automatic non-seasonal ARIMA model. If the p-value of the QS-test is below 0.01 or the p-value of the kwman-test is below 0.002, the WO-test will classify the corresponding time series as seasonal.

If residuals=FALSE the autoarima settings are ignored.

If residuals=TRUE, a non-seasonal ARIMA model is estimated for the time series. And the residuals of the fitted model are used as input to the test statistic. If an automatic order selection is used, the Hyndman-Khandakar algorithm is employed with max(p)=max(q) <= 3.
}
\examples{
wo(ts(rnorm(120, 10,10), frequency=12))
wo(ts(rnorm(120, 10,10), frequency=7))
}
\references{
Webel, K. and Ollech, D. (forthcoming). An overall seasonality test. Deutsche Bundesbank's Discussion Paper series.
}
\author{
Daniel Ollech
}
