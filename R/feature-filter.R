#' Create a feature-filter specification
#'
#' Feature-filter specifications determine which candidate columns are retained
#' after non-finite and constant features are removed. They are intended for use
#' with `icjr` model-fitting functions.
#'
#' @param method Feature-filtering method.
#' @param quantile Variability quantile in `[0, 1)`.
#' @param threshold Minimum median absolute deviation required for retention.
#'
#' @return An object of class `icjr_feature_filter`.
#' @keywords internal
#' @noRd
new_feature_filter <- function(
  method,
  quantile = NULL,
  threshold = NULL
) {
  structure(
    list(
      method = method,
      quantile = quantile,
      threshold = threshold
    ),
    class = "icjr_feature_filter"
  )
}

#' Retain all finite, non-constant features
#'
#' Creates a feature-filter specification that retains every feature that is
#' finite and non-constant in the analysis sample.
#'
#' @return An `icjr_feature_filter` object.
#' @export
feature_filter_none <- function() {
  new_feature_filter(method = "none")
}

#' Retain features above a MAD quantile
#'
#' Creates a feature-filter specification that retains finite, non-constant
#' features whose median absolute deviation is strictly above the specified
#' quantile of MAD values computed in the analysis sample.
#'
#' @param quantile Variability quantile in `[0, 1)`. The default, `0.5`,
#' retains features with MAD strictly above the median MAD.
#'
#' @return An `icjr_feature_filter` object.
#' @export
feature_filter_mad <- function(quantile = 0.5) {
  validate_filter_quantile(quantile)
  new_feature_filter(
    method = "mad_quantile",
    quantile = quantile
  )
}

#' Retain features above a variance quantile
#'
#' Creates a feature-filter specification that retains finite, non-constant
#' features whose variance is strictly above the specified quantile of variances
#' computed in the analysis sample.
#'
#' @param quantile Variability quantile in `[0, 1)`. The default, `0.5`,
#' retains features with variance strictly above the median variance.
#'
#' @return An `icjr_feature_filter` object.
#' @export
feature_filter_variance <- function(quantile = 0.5) {
  validate_filter_quantile(quantile)
  new_feature_filter(
    method = "variance_quantile",
    quantile = quantile
  )
}

#' Retain features above a MAD threshold
#'
#' Creates a feature-filter specification that retains finite, non-constant
#' features with median absolute deviation strictly greater than `threshold`.
#'
#' @param threshold A finite numeric threshold.
#'
#' @return An `icjr_feature_filter` object.
#' @export
feature_filter_mad_threshold <- function(threshold) {
  if (
    length(threshold) != 1L ||
      is.na(threshold) ||
      !is.numeric(threshold) ||
      !is.finite(threshold)
  ) {
    rlang::abort("`threshold` must be one finite numeric value.")
  }

  new_feature_filter(
    method = "mad_threshold",
    threshold = threshold
  )
}

#' @keywords internal
#' @noRd
validate_filter_quantile <- function(quantile) {
  if (
    length(quantile) != 1L ||
      is.na(quantile) ||
      !is.numeric(quantile) ||
      !is.finite(quantile) ||
      quantile < 0 ||
      quantile >= 1
  ) {
    rlang::abort("`quantile` must be one finite numeric value in [0, 1).")
  }

  invisible(quantile)
}

#' @keywords internal
#' @noRd
validate_feature_filter <- function(feature_filter) {
  if (!inherits(feature_filter, "icjr_feature_filter")) {
    rlang::abort(
      "`feature_filter` must be created with a `feature_filter_*()` function."
    )
  }

  valid_methods <- c(
    "none",
    "mad_quantile",
    "variance_quantile",
    "mad_threshold"
  )

  if (!feature_filter$method %in% valid_methods) {
    rlang::abort("`feature_filter` has an unrecognized method.")
  }

  if (feature_filter$method %in% c("mad_quantile", "variance_quantile")) {
    validate_filter_quantile(feature_filter$quantile)
  }

  if (identical(feature_filter$method, "mad_threshold")) {
    feature_filter_mad_threshold(feature_filter$threshold)
  }

  invisible(feature_filter)
}

#' @keywords internal
#' @noRd
validate_feature_matrix <- function(x) {
  if (!is.matrix(x) || !is.numeric(x)) {
    rlang::abort(
      "`x` must be a numeric matrix with samples in rows and features in columns."
    )
  }

  if (nrow(x) < 1L || ncol(x) < 1L) {
    rlang::abort("`x` must contain at least one sample and one feature.")
  }

  if (is.null(rownames(x)) || anyNA(rownames(x)) || any(rownames(x) == "")) {
    rlang::abort(
      "`x` must have non-missing, non-empty sample IDs in `rownames(x)`."
    )
  }

  if (anyDuplicated(rownames(x))) {
    rlang::abort("`rownames(x)` must be unique.")
  }

  if (is.null(colnames(x)) || anyNA(colnames(x)) || any(colnames(x) == "")) {
    rlang::abort(
      "`x` must have non-missing, non-empty feature IDs in `colnames(x)`."
    )
  }

  if (anyDuplicated(colnames(x))) {
    rlang::abort("`colnames(x)` must be unique.")
  }

  invisible(x)
}

#' @keywords internal
#' @noRd
select_model_features <- function(x, feature_filter) {
  validate_feature_matrix(x)
  validate_feature_filter(feature_filter)

  finite <- apply(x, 2, function(feature) all(is.finite(feature)))
  nonconstant <- apply(x, 2, function(feature) length(unique(feature)) > 1L)
  eligible <- colnames(x)[finite & nonconstant]

  if (length(eligible) == 0L) {
    rlang::abort("No finite, non-constant features are available for modeling.")
  }

  if (identical(feature_filter$method, "none")) {
    return(eligible)
  }

  x_eligible <- x[, eligible, drop = FALSE]

  variability <- switch(
    feature_filter$method,
    mad_quantile = apply(
      x_eligible,
      2,
      stats::mad,
      constant = 1
    ),
    variance_quantile = apply(x_eligible, 2, stats::var),
    mad_threshold = apply(
      x_eligible,
      2,
      stats::mad,
      constant = 1
    )
  )

  threshold <- if (
    feature_filter$method %in% c("mad_quantile", "variance_quantile")
  ) {
    stats::quantile(
      variability,
      probs = feature_filter$quantile,
      names = FALSE
    )
  } else {
    feature_filter$threshold
  }

  selected <- names(variability)[
    is.finite(variability) & variability > threshold
  ]

  if (length(selected) == 0L) {
    rlang::abort(
      paste0(
        "No features remain after applying `",
        feature_filter$method,
        "`. Use a less restrictive filter."
      )
    )
  }

  selected
}
