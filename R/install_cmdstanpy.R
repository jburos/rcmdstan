
# Note: this code mirrors that in the current version of
# Tensorflow, available here:
# https://github.com/rstudio/tensorflow/blob/37b15afb9d31c72bf57ba96c222da79bace48e99/R/install.R

# This sets up & configures a dedicated conda environment or virtualenv for cmdstanpy

#' Install CmdStanPy and its dependencies
#'
#' @inheritParams reticulate::conda_list
#'
#' @param method Installation method. By default, "auto" automatically finds a
#'   method that will work in the local environment. Change the default to force
#'   a specific installation method. Note that the "virtualenv" method is not
#'   available on Windows (as this isn't supported by TensorFlow). Note also
#'   that since this command runs without privillege the "system" method is
#'   available only on Windows.
#'
#' @param version CmdStanPy version to install. Specify "default" to install
#'   the latest version tested in this package. Specify "latest" to install
#'   the latest version (experimental) irrespective of testing.
#'
#'   You can also provide a full major.minor.patch specification (e.g. "1.1.0")
#'
#'   Alternatively, you can provide the full URL to an installer binary (e.g.
#'   for a nightly binary).
#'
#' @param envname Name of Python environment to install within
#'
#' @param extra_packages Additional Python packages to install along with
#'   TensorFlow.
#'
#' @param restart_session Restart R session after installing (note this will
#'   only occur within RStudio).
#'
#' @param conda_python_version the python version installed in the created conda
#'   environment. Python 3.6 is installed by default.
#'
#' @param ... other arguments passed to [\code{reticulate::conda_install()}] or
#'   [\code{reticulate::virtualenv_install()}].
#'
#' @importFrom jsonlite fromJSON
#'
#' @export
install_cmdstanpy <- function(method = c("auto", "virtualenv", "conda"),
                              conda = "auto",
                              version = "default",
                              envname = "r-cmdstanpy",
                              extra_packages = NULL,
                              restart_session = TRUE,
                              conda_python_version = "3.6",
                              ...) {

  method <- match.arg(method)

  # unroll version
  ver <- parse_cmdstanpy_version(version)

  version <- ver$version
  # gpu <- ver$gpu
  package <- ver$package

  # Packages in this list should always be installed.
  default_packages <- c()
  extra_packages <- unique(c(default_packages, extra_packages))

  # Main OS verification.
  if (is_osx() || is_linux()) {

    if (method == "conda") {
      install_conda(
        package = package,
        extra_packages = extra_packages,
        envname = envname,
        conda = conda,
        conda_python_version = conda_python_version,
        ...
      )
    } else if (method == "virtualenv" || method == "auto") {
      install_virtualenv(
        package = package,
        extra_packages = extra_packages,
        envname = envname,
        ...
      )
    }

  } else if (is_windows()) {

    if (method == "virtualenv") {
      ## TODO: test this!
      stop("Installing cmdstanpy into a virtualenv is not supported on Windows",
           call. = FALSE)
    } else if (method == "conda" || method == "auto") {

      install_conda(
        package = package,
        extra_packages = extra_packages,
        envname = envname,
        conda = conda,
        conda_python_version = conda_python_version,
        ...
      )

    }

  } else {
    stop("Unable to install cmdstanpy on this platform. ",
         "Binary installation is available for Windows, OS X, and Linux")
  }

  cat("\nInstallation complete.\n\n")

  if (restart_session && rstudioapi::hasFun("restartSession"))
    rstudioapi::restartSession()

  invisible(NULL)
}

install_conda <- function(package, extra_packages, envname, conda, conda_python_version, ...) {

  # find if environment exists
  envname_exists <- envname %in% reticulate::conda_list(conda = conda)$name

  # remove environment
  if (envname_exists) {
    cat("Removing ", envname, " conda environment... \n")
    reticulate::conda_remove(envname = envname, conda = conda)
  }


  cat("Creating ", envname, " conda environment... \n")
  reticulate::conda_create(
    envname = envname, conda = conda,
    packages = paste0("python=", conda_python_version)
  )

  cat("Installing python modules...\n")
  reticulate::conda_install(
    envname = envname,
    packages = c(package, extra_packages),
    conda = conda,
    pip = TRUE, # always use pip since it's the recommend way.
    ...
  )

}

install_virtualenv <- function(package, extra_packages, envname, ...) {

  # find if environment exists
  envname_exists <- envname %in% reticulate::virtualenv_list()

  # remove environment
  if (envname_exists) {
    cat("Removing ", envname, " virtualenv environment... \n")
    reticulate::virtualenv_remove(envname = envname, confirm = FALSE)
  }

  cat("Creating ", envname, " virtualenv environment... \n")
  reticulate::virtualenv_create(envname = envname)

  cat("Installing python modules...\n")
  reticulate::virtualenv_install(
    envname = envname,
    packages = c(package, extra_packages),
    ...
  )

}

parse_cmdstanpy_version <- function(version) {

  default_version <- "0.3.1"

  ver <- list(
    version = default_version, # version string
    package = NULL             # input to conda_install or whatever
  )

  # default version
  if (version == "default") {

    ver$package <- paste0("cmdstanpy==", ver$version)

  # user provided path to a package
  } else if (grepl("^.*\\.whl$", version)) {

    ver$version <- NA

    if (grepl("^http", version))
      ver$package <- version
    else
      ver$package <- normalizePath(version)

  # user specified a version
  } else {

    ver$version <- version

  }

  # if still not specified by the above ...
  if (is.null(ver$package)) {

    if (ver$version == "latest") {

        ver$package <- "cmdstanpy"

    } else {

      ver$package <- paste0("cmdstanpy==", ver$version)

    }

  }

  ver
}
