#' Extract notable elastic-net features
#'
#' Returns a compact reporting table of penalized features that meet minimum
#' selection-frequency and absolute median-coefficient thresholds.
#'
#' @param x An `icjr_elasticnet_fit` object.
#' @param min_percent Minimum percentage of successful repeated fits that must
#'   select a feature.
#' @param min_abs_coefficient Minimum absolute median penalized coefficient.
#' @param n_features Maximum number of features to return. Set to `Inf` to
#'   return all eligible features.
#' @param frequency_cutoffs Named numeric vector of inclusive lower bounds for
#'   selection-frequency categories passed to [classify_elasticnet_features()].
#' @param effect_cutoffs Named numeric vector of inclusive lower bounds for
#'   absolute median-coefficient categories passed to
#'   [classify_elasticnet_features()].
#'
#' @return A data frame containing notable features, ordered by decreasing
#'   selection frequency and absolute median coefficient magnitude. The output
#'   includes the median, minimum, and maximum coefficients across repeated
#'   fits that selected each feature.
#' @export
notable_features <- function(
  x,
  min_percent = 50,
  min_abs_coefficient = 0.10,
  n_features = Inf,
  frequency_cutoffs = c(
    Rare = 0,
    Low = 20,
    Medium = 50,
    High = 80
  ),
  effect_cutoffs = c(
    Negligible = 0,
    Weak = 0.05,
    Moderate = 0.10,
    Strong = 0.30
  )
) {
  if (!inherits(x, "icjr_elasticnet_fit")) {
    rlang::abort("`x` must be an `icjr_elasticnet_fit` object.")
  }

  if (
    length(min_percent) != 1L ||
      !is.numeric(min_percent) ||
      is.na(min_percent) ||
      min_percent < 0 ||
      min_percent > 100
  ) {
    rlang::abort("`min_percent` must be one number from 0 to 100.")
  }

  if (
    length(min_abs_coefficient) != 1L ||
      !is.numeric(min_abs_coefficient) ||
      is.na(min_abs_coefficient) ||
      min_abs_coefficient < 0
  ) {
    rlang::abort(
      "`min_abs_coefficient` must be one number greater than or equal to 0."
    )
  }

  if (
    length(n_features) != 1L ||
      !is.numeric(n_features) ||
      is.na(n_features) ||
      n_features < 1
  ) {
    rlang::abort("`n_features` must be one number greater than or equal to 1.")
  }

  features <- selected_features(x)

  features <- classify_elasticnet_features(
    features = features,
    frequency_cutoffs = frequency_cutoffs,
    effect_cutoffs = effect_cutoffs
  )

  features <- features[
    features$percent >= min_percent &
      abs(features$median_coefficient) >= min_abs_coefficient,
    ,
    drop = FALSE
  ]

  features <- features[
    order(
      -features$percent,
      -abs(features$median_coefficient),
      features$feature
    ),
    ,
    drop = FALSE
  ]

  features <- utils::head(features, n = n_features)

  report_columns <- c(
    "feature",
    "feature_label",
    "percent",
    "median_coefficient",
    "min_coefficient",
    "max_coefficient",
    "median_effect",
    "median_effect_label",
    "sign_consistency",
    "frequency_class",
    "effect_class",
    "signal_class"
  )

  report_columns <- intersect(report_columns, names(features))

  features[, report_columns, drop = FALSE]
}
