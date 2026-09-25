#' @importFrom rlang .data
NULL

#' @noRd
validate_elasticnet_settings <- function(
  alpha,
  nfolds,
  n_reps,
  seed,
  lambda
) {
  if (
    length(alpha) != 1L ||
      !is.numeric(alpha) ||
      is.na(alpha) ||
      !is.finite(alpha) ||
      alpha < 0 ||
      alpha > 1
  ) {
    rlang::abort("`alpha` must be one finite number in [0, 1].")
  }

  if (
    length(nfolds) != 1L ||
      !is.numeric(nfolds) ||
      is.na(nfolds) ||
      nfolds < 2L ||
      nfolds != as.integer(nfolds)
  ) {
    rlang::abort("`nfolds` must be one integer greater than or equal to 2.")
  }

  if (
    length(n_reps) != 1L ||
      !is.numeric(n_reps) ||
      is.na(n_reps) ||
      n_reps < 1L ||
      n_reps != as.integer(n_reps)
  ) {
    rlang::abort("`n_reps` must be one positive integer.")
  }

  if (
    !is.null(seed) &&
      (length(seed) != 1L ||
        !is.numeric(seed) ||
        is.na(seed) ||
        seed != as.integer(seed))
  ) {
    rlang::abort("`seed` must be NULL or one integer.")
  }

  if (
    length(lambda) != 1L ||
      !lambda %in% c("lambda.1se", "lambda.min")
  ) {
    rlang::abort(
      "`lambda` must be either `\"lambda.1se\"` or `\"lambda.min\"`."
    )
  }

  invisible(TRUE)
}

#' @noRd
build_elasticnet_design <- function(x, covariate_matrix, feature_filter) {
  feature_names <- select_model_features(
    x = x,
    feature_filter = feature_filter
  )

  feature_matrix <- scale(x[, feature_names, drop = FALSE])

  design_matrix <- if (is.null(covariate_matrix)) {
    feature_matrix
  } else {
    cbind(covariate_matrix, feature_matrix)
  }

  storage.mode(design_matrix) <- "double"

  penalty_factor <- c(
    rep(
      0,
      if (is.null(covariate_matrix)) 0L else ncol(covariate_matrix)
    ),
    rep(1, ncol(feature_matrix))
  )

  list(
    x = design_matrix,
    feature_names = feature_names,
    covariate_names = if (is.null(covariate_matrix)) {
      character()
    } else {
      colnames(covariate_matrix)
    },
    penalty_factor = penalty_factor
  )
}

#' @noRd
fit_repeated_cv_glmnet <- function(
  x,
  y,
  family,
  alpha,
  nfolds,
  n_reps,
  penalty_factor,
  type_measure,
  seed
) {
  nfolds_used <- min(as.integer(nfolds), nrow(x))

  if (nfolds_used < 2L) {
    rlang::abort("At least 2 samples are required for cross-validation.")
  }

  fit_one <- function(repetition) {
    if (!is.null(seed)) {
      set.seed(seed + repetition - 1L)
    }

    tryCatch(
      glmnet::cv.glmnet(
        x = x,
        y = y,
        family = family,
        alpha = alpha,
        nfolds = nfolds_used,
        type.measure = type_measure,
        standardize = FALSE,
        penalty.factor = penalty_factor
      ),
      error = function(error) {
        structure(
          list(message = conditionMessage(error)),
          class = "icjr_elasticnet_fit_error"
        )
      }
    )
  }

  models <- lapply(seq_len(n_reps), fit_one)

  successful <- !vapply(
    models,
    inherits,
    logical(1),
    what = "icjr_elasticnet_fit_error"
  )

  errors <- rep(NA_character_, n_reps)

  if (any(!successful)) {
    errors[!successful] <- vapply(
      models[!successful],
      function(model) model$message,
      character(1)
    )
  }

  list(
    models = models[successful],
    repetition_summary = tibble::tibble(
      repetition = seq_len(n_reps),
      successful = successful,
      error = errors
    ),
    nfolds_used = nfolds_used
  )
}

#' @noRd
extract_nonzero_coefficients <- function(model, lambda, terms) {
  empty_result <- tibble::tibble(
    feature = character(),
    coefficient = numeric()
  )

  coefficient_matrix <- tryCatch(
    stats::coef(model, s = lambda),
    error = function(error) NULL
  )

  if (is.null(coefficient_matrix)) {
    return(empty_result)
  }

  coefficient_matrix <- as.matrix(coefficient_matrix)

  tibble::tibble(
    feature = rownames(coefficient_matrix),
    coefficient = as.numeric(coefficient_matrix[, 1])
  ) |>
    dplyr::filter(
      .data$feature %in% terms,
      .data$coefficient != 0
    )
}

#' @noRd
empty_selection_summary <- function(effect_label) {
  tibble::tibble(
    feature = character(),
    n_selected = integer(),
    n_models_successful = integer(),
    selection_rate = numeric(),
    percent = numeric(),
    coefficient_values = list(),
    median_coefficient = numeric(),
    mean_coefficient = numeric(),
    min_coefficient = numeric(),
    max_coefficient = numeric(),
    sign_consistency = numeric(),
    median_effect = numeric(),
    median_effect_label = character()
  ) |>
    dplyr::mutate(median_effect_label = effect_label)
}

#' @noRd
summarize_selected_features <- function(
  coefficient_list,
  n_models_successful,
  annotation,
  effect_transform,
  effect_label
) {
  if (n_models_successful == 0L) {
    return(empty_selection_summary(effect_label))
  }

  selected <- dplyr::bind_rows(coefficient_list)

  if (nrow(selected) == 0L) {
    return(empty_selection_summary(effect_label))
  }

  summary <- selected |>
    dplyr::group_by(.data$feature) |>
    dplyr::summarise(
      n_selected = dplyr::n(),
      n_models_successful = n_models_successful,
      selection_rate = .data$n_selected / n_models_successful,
      percent = 100 * .data$selection_rate,
      coefficient_values = list(.data$coefficient),
      median_coefficient = stats::median(.data$coefficient),
      mean_coefficient = base::mean(.data$coefficient),
      min_coefficient = base::min(.data$coefficient),
      max_coefficient = base::max(.data$coefficient),
      sign_consistency = max(
        mean(.data$coefficient > 0),
        mean(.data$coefficient < 0)
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      median_effect = effect_transform(.data$median_coefficient),
      median_effect_label = effect_label
    )

  if (!is.null(annotation)) {
    if (
      !is.data.frame(annotation) ||
        !all(c("feature", "feature_label") %in% names(annotation))
    ) {
      rlang::abort(
        "`annotation` must be a data frame with `feature` and `feature_label` columns."
      )
    }

    annotation <- annotation |>
      dplyr::distinct(.data$feature, .data$feature_label)

    summary <- summary |>
      dplyr::left_join(annotation, by = "feature") |>
      dplyr::relocate(.data$feature_label, .after = .data$feature)
  }

  summary |>
    dplyr::arrange(
      dplyr::desc(.data$selection_rate),
      dplyr::desc(abs(.data$median_coefficient)),
      .data$feature
    )
}
