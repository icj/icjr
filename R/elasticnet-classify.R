#' Classify elastic-net features by stability and effect magnitude
#'
#' Assigns ordered stability categories from selection frequency and ordered
#' effect-magnitude categories from absolute median standardized elastic-net coefficients.
#' Each cutoff is the inclusive lower bound for its named category.
#'
#' @param features A data frame containing numeric `percent` and
#'   `median_standardized_coefficient` columns, such as the output of
#'   [selected_features()].
#' @param frequency_cutoffs Named numeric vector of inclusive lower bounds for
#'   selection-frequency categories. Values must be strictly increasing and
#'   begin at 0.
#' @param effect_cutoffs Named numeric vector of inclusive lower bounds for
#'   absolute median standardized-coefficient categories. Values must be strictly increasing
#'   and begin at 0.
#'
#' @return `features` with ordered-factor columns `frequency_class`,
#'   `effect_class`, and `signal_class`.
#' @export
classify_elasticnet_features <- function(
  features,
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
  validate_cutoffs <- function(cutoffs, argument) {
    if (
      !is.numeric(cutoffs) ||
        is.null(names(cutoffs)) ||
        any(names(cutoffs) == "") ||
        anyNA(cutoffs) ||
        any(!is.finite(cutoffs))
    ) {
      rlang::abort(
        paste0(
          "`",
          argument,
          "` must be a named numeric vector with finite values."
        )
      )
    }

    if (cutoffs[[1L]] != 0) {
      rlang::abort(
        paste0("`", argument, "` must begin at 0.")
      )
    }

    if (any(diff(cutoffs) <= 0)) {
      rlang::abort(
        paste0(
          "`",
          argument,
          "` values must be strictly increasing, in category order."
        )
      )
    }

    invisible(NULL)
  }

  if (!is.data.frame(features)) {
    rlang::abort("`features` must be a data frame.")
  }

  required_columns <- c("percent", "median_standardized_coefficient")
  missing_columns <- setdiff(required_columns, names(features))

  if (length(missing_columns) > 0L) {
    rlang::abort(
      paste0(
        "`features` must contain these columns: ",
        paste(required_columns, collapse = ", "),
        "."
      )
    )
  }

  if (
    !is.numeric(features$percent) ||
      !is.numeric(features$median_standardized_coefficient)
  ) {
    rlang::abort(
      "`percent` and `median_standardized_coefficient` must both be numeric."
    )
  }

  if (
    anyNA(features$percent) ||
      anyNA(features$median_standardized_coefficient) ||
      any(!is.finite(features$percent)) ||
      any(!is.finite(features$median_standardized_coefficient))
  ) {
    rlang::abort(
      paste0(
        "`percent` and `median_standardized_coefficient` must contain finite, ",
        "non-missing values."
      )
    )
  }

  if (any(features$percent < 0 | features$percent > 100)) {
    rlang::abort("`percent` must contain values from 0 to 100.")
  }

  validate_cutoffs(frequency_cutoffs, "frequency_cutoffs")
  validate_cutoffs(effect_cutoffs, "effect_cutoffs")

  frequency_levels <- names(frequency_cutoffs)
  effect_levels <- names(effect_cutoffs)

  frequency_index <- findInterval(
    x = features$percent,
    vec = unname(frequency_cutoffs)
  )

  effect_index <- findInterval(
    x = abs(features$median_standardized_coefficient),
    vec = unname(effect_cutoffs)
  )

  features$frequency_class <- factor(
    frequency_levels[frequency_index],
    levels = frequency_levels,
    ordered = TRUE
  )

  features$effect_class <- factor(
    effect_levels[effect_index],
    levels = effect_levels,
    ordered = TRUE
  )

  signal_levels <- as.vector(
    outer(
      frequency_levels,
      effect_levels,
      paste,
      sep = ":"
    )
  )

  features$signal_class <- factor(
    paste(
      features$frequency_class,
      features$effect_class,
      sep = ":"
    ),
    levels = signal_levels,
    ordered = TRUE
  )

  features
}
