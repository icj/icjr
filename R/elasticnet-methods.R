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

#' Summarize an elastic-net fit
#'
#' Provides a concise model summary including the model specification, sample
#' accounting, repeated-fit success, and the selected features with the highest
#' selection frequencies.
#'
#' @param object An `icjr_elasticnet_fit` object.
#' @param n_features Number of top selected features to retain in the returned
#'   summary. Set to `Inf` to retain all selected features.
#' @param ... Unused.
#'
#' @return An object of class `summary.icjr_elasticnet_fit`, returned
#'   invisibly. It contains `specification`, `sample_summary`,
#'   `n_selected_features`, and `top_features`.
#' @export
summary.icjr_elasticnet_fit <- function(
  object,
  n_features = 10,
  ...
) {
  if (
    length(n_features) != 1L ||
      !is.numeric(n_features) ||
      is.na(n_features) ||
      n_features < 1
  ) {
    rlang::abort("`n_features` must be one number greater than or equal to 1.")
  }

  features <- selected_features(object)

  top_features <- if (nrow(features) == 0L) {
    features
  } else {
    utils::head(features, n = n_features)
  }

  structure(
    list(
      specification = object$specification,
      sample_summary = model_summary(object),
      n_selected_features = nrow(features),
      top_features = top_features
    ),
    class = "summary.icjr_elasticnet_fit"
  )
}

#' Print an elastic-net fit summary
#'
#' @param x An object returned by [summary.icjr_elasticnet_fit()].
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#' @export
print.summary.icjr_elasticnet_fit <- function(x, ...) {
  specification <- x$specification
  sample_summary <- x$sample_summary

  cat("Elastic-net model summary\n")
  cat("  Family: ", specification$family, "\n", sep = "")

  if (identical(specification$family, "gaussian")) {
    cat("  Outcome: ", specification$outcome, "\n", sep = "")
  }

  if (identical(specification$family, "binomial")) {
    cat("  Outcome: ", specification$outcome, "\n", sep = "")
    cat("  Event level: ", specification$event_level, "\n", sep = "")
    cat(
      "  Cases / controls: ",
      sample_summary$n_cases,
      " / ",
      sample_summary$n_controls,
      "\n",
      sep = ""
    )
  }

  if (identical(specification$family, "cox")) {
    cat("  Time: ", specification$time, "\n", sep = "")
    cat("  Status: ", specification$status, "\n", sep = "")
    cat("  Events: ", sample_summary$n_events, "\n", sep = "")
  }

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
    "  Alpha: ",
    specification$alpha,
    "; lambda: ",
    specification$lambda,
    "\n",
    sep = ""
  )
  cat(
    "  CV folds: ",
    specification$nfolds_used,
    " / requested ",
    specification$nfolds_requested,
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
    x$n_selected_features,
    "\n",
    sep = ""
  )

  if (nrow(x$top_features) == 0L) {
    cat("\nNo penalized features were selected.\n")
    return(invisible(x))
  }

  display_columns <- c(
    "feature",
    "feature_label",
    "percent",
    "median_coefficient",
    "median_effect",
    "median_effect_label",
    "sign_consistency"
  )
  display_columns <- intersect(display_columns, names(x$top_features))

  cat("\nTop selected features:\n")

  print(
    x$top_features[, display_columns, drop = FALSE],
    row.names = FALSE
  )

  invisible(x)
}
