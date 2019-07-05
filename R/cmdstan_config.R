#' CmdStan configuration information
#'
#' @return List with information on the current configuration of CmdStan.
#'   You can determine whether CmdStan+CmdStanPy was found using the `available`
#'   member (other members vary depending on whether `available` is `TRUE`
#'   or `FALSE`)
#'
#' @keywords internal
#' @importFrom reticulate py_module_available
#' @importFrom reticulate py_config
#' @export
cmdstan_config <- function() {

  # confirm that reticulate has been initialized
  have_python <- reticulate::py_available(initialize = T)
  # first check if we found cmdstanpy
  have_cmdstanpy <- reticulate::py_module_available("cmdstanpy")
  # then we will check if cmdstanpy can find cmdstan
  have_cmdstan <- NULL

  # get py config
  config <- reticulate::py_config()

  # found it!
  if (have_cmdstanpy) {

    # verify that we have cmdstan
    cmdstan_path <- try(get_cmdstan_path(), silent = T)
    if (inherits(cmdstan_path, 'try-error'))
      have_cmdstan <- F
    else
      have_cmdstan <- T

    if (have_cmdstan) {
      structure(class = "cmdstan_config", list(
        available = TRUE,
        version = '?', ## TBD
        path = cmdstan_path,
        cmdstanpy_available = TRUE,
        cmdstanpy_version = cmdstanpy$`__version__`,
        location = config$required_module_path,
        python = config$python,
        python_version = config$version
      ))
    } else {
      structure(class = "cmdstan_config", list(
        available = FALSE,
        version = '?', ## TBD
        path = NULL,
        cmdstanpy_available = TRUE,
        cmdstanpy_version = cmdstanpy$`__version__`,
        location = config$required_module_path,
        python = config$python,
        python_version = config$version,
        error_message = 'CmdStan not yet installed - try running install_cmdstan().'
      ))
    }


    # didn't find it
  } else {
    structure(class = "cmdstan_config", list(
      available = FALSE,
      python_verisons = config$python_versions,
      error_message = cmdstan_config_error_message()
    ))
  }
}

#' Get cmdstan installation info
get_cmdstan_path <- function() {
  # following will issue an error if not found or dir validated
  cmdstan_path <- cmdstanpy$cmdstan_path()
}

#' @rdname cmdstan_config
#' @keywords internal
#' @export
cmdstan_version <- function() {
  config <- cmdstan_config()
  if (config$available)
    config$version
  else
    NULL
}

#' @export
print.cmdstan_config <- function(x, ...) {
  if (x$available) {
    aliased <- function(path) sub(Sys.getenv("HOME"), "~", path)
    cat("CmdStan v", x$version, " (", aliased(x$path), ") via CmdStanPy v", x$cmdstanpy_version, "\n", sep = "")
    cat("Python v", x$python_version, " (", aliased(x$python), ")\n", sep = "")
  } else {
    cat(x$error_message, "\n")
  }
}


# Build error message for CmdStanPy configuration errors
cmdstan_config_error_message <- function() {
  message <- "Installation of CmdStanPy not found."
  config <- py_config()
  if (!is.null(config)) {
    if (length(config$python_versions) > 0) {
      message <- paste0(message,
                        "\n\nPython environments searched for 'cmdstanpy' package:\n")
      python_versions <- paste0(" ", normalizePath(config$python_versions, mustWork = FALSE),
                                collapse = "\n")
      message <- paste0(message, python_versions, sep = "\n")
    }
  }
  message <- paste0(message,
                    "\nYou can install CmdStanPy using the install_cmdstanpy() function.\n")
  message
}
