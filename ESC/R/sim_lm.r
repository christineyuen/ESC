#' Simulate a time series from ARFIMA model
#' @export
#' @param n length of time series
#' @param mod ARFIMA params (ar, d, ma, mu, sigma) after change-point
#' @return a simulated time series from ARFIMA
arfima.sim <- function(mod, n) {
  sigma = ifelse(is.null(mod$sd), mod$sigma, mod$sd)
  return (fracdiff::fracdiff.sim(n = n, d = mod$d,
                                 ar = mod$ar, ma = mod$ma,
                                 innov = stats::rnorm(n+5, 0, sigma),
                                 mu = mod$mu)$series)
}

sim.lm.ts <- function(its, mod){
  n <- mod$n
  sims <- matrix(nrow = its, ncol = n)
  for(i in 1:its){
    sims[i,] <- arfima.sim(mod = mod, n = n)
  }
  return(sims)
}

