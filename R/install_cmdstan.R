
#' Convenience function to install CmdStan to your system
#' Note: assumes you have build tools installed (c++ etc)
#' @export
install_cmdstan <- function(version = NULL, directory = NULL, test = F) {
  args <- c()
  if (!is.null(version))
    args <- c(args, c('-v', version))
  if (!is.null(directory))
    args <- c(args, c('-d', directory))

  if (!reticulate::py_available('cmdstanpy')) {
    stop('CmdStanPy installation required but not found - try running install_cmdstanpy().')
  } else {
    install_binary <- system('which install_cmdstan', intern = T)
    install_cmd <- paste(unlist(c(install_binary, args)), collapse = ' ')
    if (!test) {
      system2(install_binary, args = args)
    } else {
      cat("# Command (not run): \n")
      cat(install_cmd)
      invisible(install_cmd)
    }
  }
}
