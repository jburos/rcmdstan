skip_if_no_cmdstanpy <- function() {
  if (!reticulate::py_module_available("cmdstanpy"))
    skip("CmdStanPy not available for testing")
}
