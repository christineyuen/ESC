fit.lm <- function(x, max.p = 1, max.q = 1){
  mod <- forecast::arfima(x, drange = c(0.0001, 0.5), D = 0,
                          max.p = max.p, max.q = max.q,
                          max.order = (max.p + max.q), max.D = 0,
                          start.p = 1, start.q = 1,
                          start.P = 0, start.Q = 0, seasonal = FALSE,
                          ic = "bic", approximation = TRUE,
                          allowdrift = FALSE, allowmean = TRUE)
  mod$mu <- mean(mod$x)
  return(mod)
}
