#' Extract selected features from an elastic-net fit
#'
#' @param x An `icjr_elasticnet_fit` object.
#' @param ... Unused.
#'
#' @return A tibble with one row per selected feature. The table includes
#'   selection frequency and coefficient summaries across successful repeated
#'   cross-validation fits.
#' @export
selected_features <- function(x, ...) {
  UseMethod("selected_features")
}

#' @export
selected_features.icjr_elasticnet_fit <- function(x, ...) {
  x$selected_features
}

#' Extract covariate coefficients from an elastic-net fit
#'
#' @param x An `icjr_elasticnet_fit` object.
#' @param ... Unused.
#'
#' @return A tibble with coefficient summaries for unpenalized covariates across
#'   successful repeated cross-validation fits.
#' @export
covariate_coefficients <- function(x, ...) {
  UseMethod("covariate_coefficients")
}

#' @export
covariate_coefficients.icjr_elasticnet_fit <- function(x, ...) {
  x$covariate_coefficients
}

#' Extract model-level sample and fit summary information
#'
#' @param x An `icjr_elasticnet_fit` object.
#' @param ... Unused.
#'
#' @return A one-row tibble containing sample accounting, feature counts,
#'   cross-validation-fold counts, and repeated-fit success counts.
#' @export
model_summary <- function(x, ...) {
  UseMethod("model_summary")
}

#' @export
model_summary.icjr_elasticnet_fit <- function(x, ...) {
  x$sample_summary
}

#' Extract feature IDs modeled by an elastic-net fit
#'
#' @param x An `icjr_elasticnet_fit` object.
#' @param ... Unused.
#'
#' @return A character vector of feature IDs retained after filtering and used
#'   in model fitting.
#' @export
modeled_features <- function(x, ...) {
  UseMethod("modeled_features")
}

#' @export
modeled_features.icjr_elasticnet_fit <- function(x, ...) {
  x$feature_names
}

#' Extract repeated-fit status information
#'
#' @param x An `icjr_elasticnet_fit` object.
#' @param ... Unused.
#'
#' @return A tibble with one row per requested repetition and columns describing
#'   whether fitting succeeded and any error message.
#' @export
repetition_summary <- function(x, ...) {
  UseMethod("repetition_summary")
}

#' @export
repetition_summary.icjr_elasticnet_fit <- function(x, ...) {
  x$repetition_summary
}

#' Extract feature standard deviations used for SD-scale effect reporting
#'
#' @param x An `icjr_elasticnet_fit` object.
#' @param ... Unused.
#'
#' @return A named numeric vector of standard deviations for modeled penalized
#'   features. These values are used to convert original-scale coefficients to
#'   reported per-1-SD effects.
#' @export
feature_standard_deviations <- function(x, ...) {
  UseMethod("feature_standard_deviations")
}

#' @export
feature_standard_deviations.icjr_elasticnet_fit <- function(x, ...) {
  x$feature_sd
}
