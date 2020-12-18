#' Get simulation settings
#' @export
#' @param file.name location of the setting file
#' @param n simulated time series length
#' @return a list of settings with corresponding sim function
get.settings <- function(file.name, n){
  settings.txt = utils::read.csv(file.name, header = TRUE, na.strings = "NA",
                                 stringsAsFactors=FALSE, strip.white = TRUE)
  settings = list()
  for (i in 1:nrow(settings.txt)) {
    one.setting = unlist(settings.txt[i,])
    # changepoint setting
    if(is.na(one.setting["d"])){
      param = list()
      param[[1]] = list(ar = as.numeric(one.setting["phi_1"]),
                        ma = as.numeric(one.setting["theta_1"]),
                        mu = 0,
                        sd = 1,
                        n = round(one.setting["lambda"]*n))
      param[[2]] = list(ar = as.numeric(one.setting["phi_2"]),
                        ma = as.numeric(one.setting["theta_2"]),
                        mu = as.numeric(one.setting["mu"]),
                        sd = 1,
                        n = n)
      settings[[i]] = list(param = param,
                           sim.func = sim.cps.func)
      # long memory
    } else{
      settings[[i]] = list(param = list(ar = as.numeric(one.setting["phi_1"]),
                                        ma = as.numeric(one.setting["theta_1"]),
                                        mu = 0,
                                        sd = 1,
                                        d = as.numeric(one.setting["d"]),
                                        n = n),
                           sim.func = sim.lm.func)
    }
  }
  return (settings)
}

# ======= unify the sim function for cp and lm =========
sim.cps.func <- function(param){
  return (arma.cp.sim(mod.1 = param[[1]], mod.2 = param[[2]]))
}

sim.lm.func <- function(param){
  return (arfima.sim(mod = param, n = param$n))
}
