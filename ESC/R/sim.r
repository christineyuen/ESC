get.sim.data <- function(setting, setting.name, n, its, seed) {
  set.seed(seed)
  x <- setting$sim.func(setting$param)
  es.result <- es(x, lm.max.p = 1, lm.max.q = 1,
                  cp.max.p = 1, cp.max.q = 1,
                  its = its)
  es.result$x <- x
  es.result$setting <- setting
  return (es.result)
}

#' Save simulated data
#' @export
#' @importFrom utils write.table
#' @param setting simulation setting from get.settings()
#' @param n time series length
#' @param its number of simulated training data for each class
#' @param data_folder folder for the simulated data
#' @param seed for set.seed()
#' @return none
save.sim <- function(setting, n, its, data_folder, seed) {
  d = get.sim.data(setting = setting, n = n, its = its, seed = seed)
  write.table(matrix(d$x, nrow=1),
              file = file.path(data_folder, "x.csv"), sep = ",", row.names=FALSE, col.names = FALSE)
  write.table(ifelse(is.null(d$setting$param$d), 1, 0),
              file = file.path(data_folder,  "y.csv"), sep = ",", row.names=FALSE, col.names = FALSE)
  write.table(d$sim.x,
              file = file.path(data_folder, "x_sim.csv"), sep = ",", row.names=FALSE, col.names = FALSE)
  write.table(d$sim.y,
              file = file.path(data_folder, "y_sim.csv"), sep = ",", row.names=FALSE, col.names = FALSE)
}

#' Save simulated data
#' @export
#' @param setting.file file for get.settings()
#' @param n time series length
#' @param its number of simulated training data for each class
#' @param root.dir folder for the simulated data
#' @return none
#' @examples
#' \donttest{
#' n = 512
#' root.dir = "/Users/loktingyuen/Documents/sim_result/ts_classification_through_simulation/NN/512"
#' setting.file = "/Users/loktingyuen/Library/Mobile Documents/com~apple~CloudDocs/PhD/ESC/setting.csv"
#' #save.sim.for.resnet(setting.file = setting.file, n = n, its = 1000, root.dir = root.dir)
#' }
save.sim.for.resnet <- function(setting.file, n = 512, its = 1000, root.dir) {
  settings <- get.settings(setting.file, n)
  for(setting.name in 1:length(settings)) {
    print(setting.name)
    setting <- settings[[setting.name]]
    setting.dir = file.path(root.dir, setting.name)
    dir.create(setting.dir, showWarnings = FALSE)
    dir.create(file.path(setting.dir, "sim_data"), showWarnings = FALSE)
    save(setting, file = file.path(root.dir, setting.name, "setting.RData"))
    for(i in 1:100) {
      print(i)
      data.dir <- file.path(root.dir, setting.name, "sim_data", i)
      dir.create(data.dir, showWarnings = FALSE)
      save.sim(setting = setting, n = n, its = its, data_folder = data.dir, seed = i)
    }
  }
}
