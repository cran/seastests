#' OCSB test
#' 
#' Test for seasonal unit root roots in a time series.
#' @param x time series
#' @param freq frequency to be tested
#' @param method "OLS" or "ML"
#' @param augmentations non-seasonal and seasonal order of the augmentations
#' @param nrun number of runs in monte carlo simulation
#' @param seed seed for monte carlo simulated based generation of null distribution
#' @details The null hypothesis of the OCSB is that a series contains a seasonal unit root. This is tested by a Dickey-Fuller type regression. The test regression has often to be augmented by autocorrelational terms to ensure white noise of the error terms. 
#' @details If seasonal lags are included and method='OLS' the test regression is calculated by OLS, so only the seasonal lags are included. If instead of 'OLS' method='ML' a seasonal AR model is calculated, which implies that high-order non-seasonal lags will be indirectly included as well (see Box and Jenkins, 1970). For seasonal augmentations, ML is quite a bit slower than OLS. The run time can be speeded up by reducing the number of runs of the monte carlo simulation (e.g. nrun=100).
#' @details Under the null hypothesis the test statistic follows a non-standard distribution and thus needs to be simulated. The number of runs and the seed can be changed.
#' @references Box, G. and G. Jenkins (1970). Time Series Analysis: Forecasting and Control. San Francisco: Holden-Day.
#' @references Osborn D.R., Chui A.P.L., Smith J., and Birchenhall C.R. (1988). Seasonality and the order of integration for consumption, Oxford Bulletin of Economics and Statistics 50(4):361-377.
#' @author Daniel Ollech
#' @examples teststat <- ocsb(ts(rnorm(70, 10,10), frequency=7), nrun=200)
#' check_residuals(teststat)
#' @export


ocsb <- function(x, method="OLS", augmentations=c(3,0), freq=NA,nrun=1000, seed=123) {
  
  if(method != "OLS" & method != "ML") {stop("Please, choose either OLS or ML as the method")}  
  if(method == "ML" & nrun > 100 & augmentations[2]>0) {cat("This will take some time. Please consider using method='OLS' or reducing nrun")}
  
  
  if(is.na(freq)) {
    if (any(class(x)=="ts")) {freq <- stats::frequency(x)} else {
      if (any(class(x)=="xts")) {freq <- freq_xts(x)} else {
        stop("Do not know the frequency of the time series.")
      }}}
  if(freq < 2) {warning("Frequency is < 2, this series does not seem to be seasonal. Results will almost surely be misguiding.")}
  
  if(method=="ML" & augmentations[2] > 0 & freq>249) {stop("If method = 'ML', seasonal lags cannot be included for a series with this many observations per year. Set method='OLS'.")} 
  
  if (length(augmentations)==1) {augmentations = c(augmentations, 0)}
  
  len <- length(x)
  Start <- stats::start(x)
  
  if (method=="ML") {  
    .teststat <- function(x, Augmentations=augmentations, Freq=freq) {
      mod <- tryCatch(suppressWarnings(stats::arima(.Diff(.Diff(x,1),Freq), order=c(Augmentations[1],0,0), seasonal=list(order=c(Augmentations[2],0,0), period=Freq), xreg=cbind(regular_integration =.Diff(.Lag(x,1),Freq),seasonal_integration=.Diff(.Lag(x,Freq),1)))) , error=function(e) e)
      if(inherits(mod, "error")) {mod <- NA;t_krit1 <- NA; stop("Error in the ARIMA model estimation, please try other version of OCSB test")} else {
        t_krit <- suppressWarnings(utils::tail(mod$coef,1)/utils::tail(sqrt(diag(mod$var.coef)),1))}
      return(list(t_krit=t_krit, mod=mod))
    }
    
    an <- .teststat(x)
    t_krit <- an$t_krit
    mod <- an$mod
  } 
  
  
  if (method=="OLS") {  
    
    .teststat <- function(x, Augmentations=augmentations, Freq=freq) {
      y <- .Diff(.Diff(x,1),Freq)
      
      if (sum(Augmentations)==0) {augmen = NULL
      data <- data.frame(y, regular_integration = .Diff(.Lag(x,1),Freq), seasonal_integration=.Diff(.Lag(x,Freq),1))
      } else {
        augmen <- data.frame(lapply(1:max(1,Augmentations[1], Augmentations[2]*Freq), function(x) .Lag(y,k=x)))
        augmen <- augmen[, c(seq_len(Augmentations[1]), seq_len(Augmentations[2])*Freq)]
        nc_aug <- ifelse(sum(Augmentations)<=1, sum(Augmentations), ncol(augmen))
        if (nc_aug > 1) {colnames(augmen) <- paste0("augmentation", seq_len(nc_aug))}
        data <- data.frame(y, augmen, regular_integration = .Diff(.Lag(x,1),Freq), seasonal_integration=.Diff(.Lag(x,Freq),1))
      }
      
      
      mod <- tryCatch(summary(stats::lm(y ~ ., data=data)), error=function(e) e)
      if(inherits(mod, "error")) {mod <- NA;t_krit1 <- NA; stop("Error in the OLS model estimation, please try other version of OCSB test")} else {
        t_krit <-  mod$coefficients[nrow(mod$coefficients),3]
      }
      return(list(t_krit=t_krit, mod=mod))
    }
    
    an <- .teststat(x)
    t_krit <- an$t_krit
    mod <- an$mod
  } 
  
  .sim_int <- function(Length, Freq) {
    e <- stats::rnorm(Length*7, 0, 1) # Simulating error terms
    y1 <- stats::diffinv(e, xi=100)   # diffinverting error terms to get a regular integrated series
    y <- stats::ts(utils::tail(stats::diffinv(y1, lag=Freq, xi=stats::rnorm(Freq,0,10)), Length), frequency=Freq, start=c(2001,1)) # seasonal diffinverting y1 to get a regular+seasonally intgr. series
    return(y)
  }
  
  .getPval <- function(x, Freq=freq, Seed=seed, Augmentations=augmentations, Len=len, sstart=Start) {
    #simulation of series
    set.seed(Seed + x)
    
    sim_series <- NA
    while(any(is.na(sim_series))) {
      sim_series <-  .sim_int(Len, Freq)} 
    sim_series <- stats::ts(100 + scale(as.numeric(sim_series)), start=sstart, frequency=Freq)
    
    # get test statistic
    t_krit <- .teststat(sim_series, Augmentations=Augmentations)$t_krit
    return(t_krit)
  }
  
  null_distr <- sapply(seq_len(nrun), function(x) .getPval(x, Freq=freq, Seed=seed, Augmentations=augmentations, Len=len, sstart=Start)) 
  
  p_value <- sum(t_krit > null_distr)/nrun
  
  names(t_krit) <- ""
  
  out <- list(stat=t_krit, Pval=p_value, test=paste0("OCSB with augmentations (regular, seasonal): (", augmentations[1], ", ", augmentations[2], ")"), model=mod, freq=freq)
  class(out) <- c("seasinttests")
  return(out)
}
