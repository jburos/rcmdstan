# global reference to cmdstanpy (will be initialized in .onLoad)
cmdstanpy <- NULL

#' @importFrom reticulate import
.onLoad <- function(libname, pkgname) {
  # use superassignment to update global reference to scipy
  cmdstanpy <<- reticulate::import("cmdstanpy", delay_load = TRUE)
}
