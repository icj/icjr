#' Print an elastic-net fit
#'
#' @param x An `icjr_elasticnet_fit` object.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#' @export
print.icjr_elasticnet_fit <- function(x, ...) {
  sample_summary <- x$sample_summary

  cat("Repeated elastic-net model\n")
  cat("  Family: ", x$specification$family, "\n", sep = "")
  cat(
    "  Samples modeled: ",
    sample_summary$n_samples_modeled,
    "\n",
    sep = ""
  )
  cat(
    "  Features modeled: ",
    sample_summary$n_features_modeled,
    "\n",
    sep = ""
  )
  cat(
    "  Successful repetitions: ",
    sample_summary$n_reps_successful,
    " / ",
    sample_summary$n_reps_requested,
    "\n",
    sep = ""
  )
  cat(
    "  Selected features: ",
    nrow(x$selected_features),
    "\n",
    sep = ""
  )

  invisible(x)
}
