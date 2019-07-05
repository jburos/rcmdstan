
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
#' @param version CmdStanPy version to install. Because CmdStanPy
#'   is not yet available on PyPI, this installs from github
#'   using git+git://github.com/stan-dev/cmdstanpy.
#'
#'   Specify "default" to install from the "master" branch. Otherwise, provide a tag to the
#'   git commit/branch you wish to install.
#'
#'   Alternatively, you can provide the full URL to an installer binary (e.g.
#'   for a nightly binary) or to an alternate git repo (e.g. your local fork).
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
  default_packages <- c('pandas')
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

  # check current environment
  if (envname_exists) {
    cat("Conda environment", envname, "already exists. Checking if CmdStanPy already installed...\n")
    reticulate::use_condaenv(envname)
    if (reticulate::py_module_available('cmdstanpy')) {
      cat("CmdStanPy found.")
      return(TRUE)
    } else {
      cat("CmdStanPy not found. Proceeding with the installation.")
      #  reticulate::conda_remove(envname = envname, conda = conda)
    }
  } else {
    cat("Creating", envname, "conda environment... \n")
    reticulate::conda_create(
      envname = envname, conda = conda,
      packages = paste0("python=", conda_python_version)
    )
  }

  cat("Installing extra python modules & dependencies...\n")
  reticulate::conda_install(
    envname = envname,
    packages = c(extra_packages),
    conda = conda,
    pip = TRUE, # always use pip since it's the recommend way.
    ...
  )
  cat("Installing cmdstanpy...\n")
  reticulate::conda_install(
    envname = envname,
    packages = package,
    conda = conda,
    pip = TRUE,
    ...
  )

}

install_virtualenv <- function(package, extra_packages, envname, ...) {

  # find if environment exists
  envname_exists <- envname %in% reticulate::virtualenv_list()

  # remove environment
  if (envname_exists) {
    cat("Virtual environment", envname, "already exists. Checking if CmdStanPy already installed...\n")
    reticulate::use_virtualenv(envname)
    if (reticulate::py_module_available('cmdstanpy')) {
      cat("CmdStanPy found.")
      return(TRUE)
    } else {
      cat("CmdStanPy not found. Proceeding with the installation.")
      #  reticulate::conda_remove(envname = envname, conda = conda)
    }
  } else {
    cat("Creating", envname, "virtualenv environment... \n")
    reticulate::virtualenv_create(envname = envname)
  }

  cat("Installing extra python modules...\n")
  reticulate::virtualenv_install(
    envname = envname,
    packages = extra_packages,
    ...
  )
  cat("Installing cmdstanpy...\n")
  reticulate::virtualenv_install(
    envname = envname,
    packages = package,
    ...
  )

}

parse_cmdstanpy_version <- function(version) {

  default_version <- 'master'
  default_repo <- 'git+git://github.com/stan-dev/cmdstanpy'

  ver <- list(
    version = default_version,  # used for tagging results
    package = NULL         # input to conda_install or whatever
  )

  if (version == "default") {
    # user requested default version
    ver$version <- default_version
    ver$package <- paste0(default_repo, "@", default_version)

  } else if (grepl("^.*\\.whl$", version)) {
    # user provided path or url to a package

    ver$version <- NA

    if (grepl("^http", version))
      ver$package <- version
    else
      ver$package <- normalizePath(version)

  } else if (grepl("^git", version)) {
    # user provided full path to git repo
    ver$version <- NA

    if (grepl('^git\\+git\\:\\/\\/', version))
      ver$package <- version
    else if (grepl('^git\\:\\/\\/', version))
      ver$package <- paste0('git+', version)
    else
      stop('Unable to parse git repo path')

  } else {
    # user specified a version (tag or branch of repo, since not on pypi yet)

    ver$version <- version
    ver$package <- paste0(default_repo, '@', version)

  }

  # if still not specified by the above ...
  if (is.null(ver$package))
    stop(str('Unable to parse version string:', version))

  ver
}
