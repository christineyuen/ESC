#' Estimate and simulate "training" data from ARMA 1-change-point model and the ARFIMA model
#' @export
#' @importFrom fracdiff fracdiff.sim
#' @importFrom forecast auto.arima arfima
#' @param x observed time series
#' @param lm.max.p maximum value of p in ARFIMA(p,d,q)
#' @param lm.max.q maximum value of q in ARFIMA(p,d,q)
#' @param cp.max.p maximum value of p in ARMA(p,q)
#' @param cp.max.q maximum value of q in ARMA(p,q)
#' @param its number of simulated "training" data for each model
#' @return a list, with the fitted models (mod) and the simulated "training" data (sim.x) from the ARMA 1-change-point model and the ARFIMA model. The first column of sim.x stores the labels, with 0 for ARFIMA and 1 for ARMA 1-change-point.
#' @examples
#' \donttest{
#' x1 <- arma.cp.sim(mod.1 = list(ar = 0.8, ma = 0.4, mu = 0, sd = 1, n = 100),
#' mod.2 = list(ar = 0.8, ma = 0.4, mu = 1, sd = 1, n = 100))
#' x1.es <- es(x1)
#' ts.plot(x1)
#' lines(x1.es$sim.x[1,-1], col = "blue", lty = 2)
#' lines(x1.es$sim.x[2,-1], col = "red", lty = 3)
#'
#' x2 <- arfima.sim(n = 200, mod = list(d = 0.3, ar = 0.7, ma = -0.5, mu = 0, sigma = 1))
#' x2.es <- es(x2)
#' ts.plot(x2)
#' lines(x2.es$sim.x[1,-1], col = "blue", lty = 2)
#' lines(x2.es$sim.x[2,-1], col = "red", lty = 3)
#' }
es <- function(x,
               lm.max.p = 1, lm.max.q = 1,
               cp.max.p = 1, cp.max.q = 1,
               its = 1000){

  lm.mod <- fit.lm(x = x, max.p = lm.max.p, max.q = lm.max.q)
  cp.mod <- fit.cp(x = x, max.p = cp.max.p, max.q = cp.max.q)

  cp.sims <- sim.cp.ts(its = its, mod = cp.mod)
  lm.sims <- sim.lm.ts(its = its, mod = lm.mod)

  return (list(sim.x = rbind(cp.sims, lm.sims),
               sim.y = c(rep(1, its), rep(0, its)),
               mod = list(cp = cp.mod,
                          lm = lm.mod)))
}
