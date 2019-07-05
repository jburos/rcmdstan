
# Note: this code mirrors that in the current version of
# Tensorflow, available here:
# https://github.com/rstudio/tensorflow/blob/37b15afb9d31c72bf57ba96c222da79bace48e99/R/install.R

# This sets up & configures a dedicated conda environment or virtualenv for cmdstanpy

#' install cmdstanpy to a default environment
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
  gpu <- ver$gpu
  package <- ver$package

  # Packages in this list should always be installed.

  default_packages <- c("cmdstanpy-hub")

  # Resolve TF probability version.
  if (!is.na(version) && substr(version, 1, 4) %in% c("1.12", "1.13", "1.14")) {
    default_packages <- c(default_packages, "cmdstanpy-probability")
    # install tfp-nightly
  } else if (is.na(version) ||(substr(version, 1, 4) %in% c("2.0.") || version == "nightly")) {
    default_packages <- c(default_packages, "tfp-nightly")
  }

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

  default_version <- "1.14.0"

  ver <- list(
    version = default_version,
    gpu = FALSE,
    package = NULL
  )

  if (version == "default") {

    ver$package <- paste0("cmdstanpy==", ver$version)

    # default gpu version
  } else if (version == "gpu") {

    ver$gpu <- TRUE
    ver$package <- paste0("cmdstanpy-gpu==", ver$version)

    # gpu qualifier provided
  } else if (grepl("-gpu$", version)) {

    split <- strsplit(version, "-")[[1]]
    ver$version <- split[[1]]
    ver$gpu <- TRUE

    # full path to whl.
  } else if (grepl("^.*\\.whl$", version)) {

    ver$gpu <- NA
    ver$version <- NA

    if (grepl("^http", version))
      ver$package <- version
    else
      ver$package <- normalizePath(version)

    # another version
  } else {

    ver$version <- version

  }

  # find the right package for nightly and other versions
  if (is.null(ver$package)) {

    if (ver$version == "nightly") {

      if (ver$gpu) {
        ver$package <- "tf-nightly-gpu"
      } else {
        ver$package <- "tf-nightly"
      }

    } else {

      if (ver$gpu) {
        ver$package <- paste0("cmdstanpy-gpu==", ver$version)
      } else {
        ver$package <- paste0("cmdstanpy==", ver$version)
      }

    }

  }

  ver
}