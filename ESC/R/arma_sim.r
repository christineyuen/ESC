# Note in the thesis the arma sim is using the sim from LSW paper
next.arma <- function(x, e, phi, theta, mu, sigma, last.i){
  ar.order <- length(phi)
  ma.order <- length(theta)
  new.e <- stats::rnorm(1, sd = sigma)
  new.x <- sum(mu, x[(last.i-ar.order+1):last.i]*phi,
              e[(last.i-ma.order+1):last.i]*theta, new.e)
  return (c(x = new.x, e = new.e))
}

my.arma.sim <- function(phi, theta, mu, sigma, n,
                        burn.out.n = 100, x = NULL, e = NULL, last.pos = 0){
  total.n <- n + burn.out.n
  if(length(x) < total.n){ x <- c(x, rep(0, total.n-length(x))) }
  if(length(e) < total.n){ e <- c(e, rep(0, total.n-length(e))) }

  min.start.n <- max(length(phi), length(theta))
  if(last.pos < min.start.n) {
    x[(last.pos+1):min.start.n] <- stats::rnorm(min.start.n-last.pos, sd = sigma)
    e[(last.pos+1):min.start.n] <- stats::rnorm(min.start.n-last.pos, sd = sigma)
    last.pos <- min.start.n
  }

  for(i in last.pos:(total.n-1)){
    new.val <- next.arma(x = x, e = e, phi = phi, theta = theta,
                        mu = mu, sigma = sigma, last.i = i)
    x[i+1] <- new.val["x"]
    e[i+1] <- new.val["e"]
  }
  return (rbind(x = x, e = e)[,(burn.out.n+1):total.n])
}
