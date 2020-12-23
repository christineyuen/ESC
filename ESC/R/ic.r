#' Calculate the bic value
#' @export
#' @param mod fitted model (either short memory change-point or long memory)
#' @param n time series length
#' @param cp.penalty penalty (multiplier) on change-points
#' @return bic value
#' @examples
#' \donttest{
#' x1 <- arma.cp.sim(mod.1 = list(ar = 0.8, ma = 0.4, mu = 0, sd = 1, n = 100),
#' mod.2 = list(ar = 0.8, ma = 0.4, mu = 1, sd = 1, n = 100))
#' x1.es <- es(x1)
#' bic.cp = get.bic(x1.es$mod$cp, n = 200, cp.penalty = 1)
#' bic.lm = get.bic(x1.es$mod$lm, n = 200, cp.penalty = 1)
#' print(c(bic.cp, bic.lm))
#' }
get.bic <- function(mod, n, cp.penalty) {
  mse <- get.sq.error.sum(mod)/n
  p <- get.num.param(mod)
  m <- get.num.cp(mod)
  return (get.ic.from.mse(mse = mse, n = n, p = p, m = m,
                          cp.penalty = cp.penalty, ic = "bic"))
}

get.sq.error.sum <- function(mod) {
  if(class(mod)[1] == "arma.cp") {
    return (sum(sapply(mod, function(m) get.sq.error.sum(m))))
  }
  return (sum(mod$residuals^2))
}

get.ic.from.mse <- function(mse, n, p, m, cp.penalty, ic){
  # p is the number of parameters (not include change point)
  if(ic == "bic"){
    ic.factor <- log(n)
  } else if(ic == "aic") {
    ic.factor <- 2
  } else if(ic == "ssic") {
    ic.factor <- log(n)^1.01 # use the alpha value from WBS paper
  } else {
    stop("Wrong ic.factor.")
  }
  effective.param <-(p+m*cp.penalty)
  return (log(mse*n/(n-effective.param))*n+effective.param*ic.factor)
}

get.num.cp <- function(mod) {
  if(class(mod)[1] == "arma.cp") {
    return (length(mod)-1)
  } else {
    return (0)
  }
}

get.num.param <- function(mod) {
  if(class(mod)[1]== "arma.cp") {
    return (sum(sapply(mod, function(m) get.num.param(m))))
  }
  num.ar <- ifelse(is.null(mod$ar), 0, length(mod$ar))
  num.ma <- ifelse(is.null(mod$ma), 0, length(mod$ma))
  num.mu <- (!is.null(mod$mu)) && (mod$mu != 0)
  num.d <- (!is.null(mod$d)) && (mod$d != 0)
  return (num.ar + num.ma + num.mu + num.d)
}
