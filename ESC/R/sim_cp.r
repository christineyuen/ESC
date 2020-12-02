#' Simulate a time series from ARMA 1-change-point model
#' @export
#' @param mod.1 ARMA params (ar, ma, mu, sd, n) before change-point
#' @param mod.2 ARMA params (ar, ma, mu, sd, n) after change-point
#' @param burn.out.n (optional) number of samples to discard before generating the actual data
#' @return a simulated time series from ARMA 1-change-point model
arma.cp.sim <- function(mod.1, mod.2, burn.out.n = 100){
  n <- mod.1$n + mod.2$n
  arma.1 <- my.arma.sim(phi = mod.1$ar, theta = mod.1$ma, mu = 0, sigma = mod.1$sd,
                        n = mod.1$n, burn.out.n = burn.out.n)
  # note n should be used instead of mod.2$n!
  # and burn.out.n has to be 0!
  arma.2 <- my.arma.sim(phi = mod.2$ar, theta = mod.2$ma, mu = 0, sigma = mod.1$sd,
                        n = n, burn.out.n = 0,
                        x = arma.1["x",], e = arma.1["e",], last.pos = mod.1$n)
  mu <- c(rep(mod.1$mu, mod.1$n), rep(mod.2$mu, mod.2$n))
  x <- as.numeric(arma.2["x",] + mu)
  return (x)
}

sim.cp.ts <- function(its, mod){
  n = mod[[1]]$n + mod[[2]]$n
  sims <- matrix(nrow = its, ncol = n)

  for(i in 1:its) {
    sims[i,] = arma.cp.sim(mod.1 = mod[[1]],
                           mod.2 = mod[[2]])
  }
  return(sims)
}

