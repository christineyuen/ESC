fit.cp <- function(x, max.p = 1, max.q = 1){
  n <- length(x)
  min.len <- max.p + max.q + 5
  if (n - min.len < min.len) {
    stop("sum of max.p and max.q is too comparing to length of x")
  }

  loglik <- -Inf
  mods <- vector(mode = "list", length = 2)
  for (i in c(min.len:(n - min.len))) {
    mod.1 <- my.auto.arma(x = x[1:i], max.p = max.p, max.q = max.q)
    mod.2 <- my.auto.arma(x = x[(i+1):n], max.p = max.p, max.q = max.q)
    current.loglik <- mod.1$loglik + mod.2$loglik
    if (loglik < current.loglik) {
      loglik <- current.loglik
      mods[[1]] <- mod.1
      mods[[2]] <- mod.2
    }
  }

  for(i in 1:2){
    coefs <- mods[[i]]$coef
    mods[[i]] <- list(ar = get.coef(coefs = coefs, coef.name = "ar"),
                      ma = get.coef(coefs = coefs, coef.name = "ma"),
                      sd = sqrt(mods[[i]]$sigma2),
                      mu = get.coef(coefs = coefs, coef.name = "intercept"),
                      n = length(mods[[i]]$x),
                      residuals = mods[[i]]$residuals)
  }
  class(mods) <- "arma.cp"
  return (mods)
}

my.auto.arma <- function (x, max.p = 1, max.q = 1){
  model <- forecast::auto.arima(x = x, d = 0, D = 0,
                                max.p = max.p, max.q = max.q,
                                max.P = 0, max.Q = 0,
                                max.order = (max.p + max.q), max.d = 0, max.D = 0,
                                start.p = 1,start.q = 1,
                                start.P = 0,start.Q = 0, seasonal = FALSE,
                                stationary = TRUE,
                                ic = "bic", approximation = TRUE,
                                allowdrift = FALSE, allowmean = TRUE)
  return(model)
}

get.coef <- function(coefs, coef.name) {
  match.i <- grep(coef.name, names(coefs))
  if(length(match.i) == 0) {
    return (0)
  } else {
    return (coefs[match.i])
  }
}
