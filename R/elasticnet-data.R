#' @noRd
resolve_column_name <- function(data, column, argument) {
  column_expression <- rlang::enexpr(column)

  column_name <- if (is.character(column_expression)) {
    rlang::as_string(column_expression)
  } else {
    rlang::as_name(column_expression)
  }

  if (length(column_name) != 1L || !column_name %in% names(data)) {
    rlang::abort(
      paste0("`", argument, "` must name one column in `sample_data`.")
    )
  }

  column_name
}

#' @noRd
prepare_elasticnet_data <- function(
  x,
  sample_data,
  sample_id,
  outcome,
  covariates = NULL,
  subset_expression = NULL
) {
  validate_feature_matrix(x)

  if (!is.data.frame(sample_data)) {
    rlang::abort("`sample_data` must be a data frame or tibble.")
  }

  if (!is.null(covariates) && !inherits(covariates, "formula")) {
    rlang::abort(
      "`covariates` must be NULL or a one-sided formula, such as `~ age + sex`."
    )
  }

  sample_data <- tibble::as_tibble(sample_data)

  sample_id_name <- resolve_column_name(
    data = sample_data,
    column = {{ sample_id }},
    argument = "sample_id"
  )

  outcome_name <- resolve_column_name(
    data = sample_data,
    column = {{ outcome }},
    argument = "outcome"
  )

  n_metadata_input <- nrow(sample_data)

  if (!rlang::quo_is_null(subset_expression)) {
    sample_data <- dplyr::filter(sample_data, !!subset_expression)
  }

  n_after_subset <- nrow(sample_data)

  if (n_after_subset == 0L) {
    rlang::abort("No metadata rows remain after applying `subset`.")
  }

  sample_ids <- sample_data[[sample_id_name]]

  if (anyNA(sample_ids) || any(sample_ids == "")) {
    rlang::abort(
      "`sample_id` contains missing or empty values after applying `subset`."
    )
  }

  sample_data[[sample_id_name]] <- as.character(sample_ids)

  if (anyDuplicated(sample_data[[sample_id_name]])) {
    rlang::abort(
      "`sample_id` must uniquely identify rows in `sample_data` after subsetting."
    )
  }

  present_in_matrix <- sample_data[[sample_id_name]] %in% rownames(x)

  n_missing_from_matrix <- sum(!present_in_matrix)

  sample_data <- sample_data[present_in_matrix, , drop = FALSE]

  if (nrow(sample_data) == 0L) {
    rlang::abort(
      "No metadata rows remain after matching `sample_id` to `rownames(x)`."
    )
  }

  outcome_values <- sample_data[[outcome_name]]

  if (!is.numeric(outcome_values)) {
    rlang::abort("`outcome` must be numeric for Gaussian elastic-net models.")
  }

  covariate_matrix <- if (is.null(covariates)) {
    NULL
  } else {
    tryCatch(
      stats::model.matrix(covariates, data = sample_data)[, -1, drop = FALSE],
      error = function(error) {
        rlang::abort(
          paste0(
            "Could not construct the covariate model matrix: ",
            conditionMessage(error)
          )
        )
      }
    )
  }

  complete_outcome <- is.finite(outcome_values)

  complete_covariates <- if (is.null(covariate_matrix)) {
    rep(TRUE, nrow(sample_data))
  } else {
    stats::complete.cases(covariate_matrix)
  }

  complete_rows <- complete_outcome & complete_covariates

  n_incomplete_outcome_or_covariates <- sum(!complete_rows)

  sample_data <- sample_data[complete_rows, , drop = FALSE]

  if (!is.null(covariate_matrix)) {
    covariate_matrix <- covariate_matrix[complete_rows, , drop = FALSE]
  }

  if (nrow(sample_data) < 3L) {
    rlang::abort("Fewer than 3 complete samples remain for modeling.")
  }

  x_model <- x[sample_data[[sample_id_name]], , drop = FALSE]

  if (any(!is.finite(x_model))) {
    rlang::abort(
      "`x` contains missing or non-finite values among modeled samples."
    )
  }

  list(
    x = x_model,
    y = sample_data[[outcome_name]],
    covariate_matrix = covariate_matrix,
    sample_summary = tibble::tibble(
      n_metadata_input = n_metadata_input,
      n_after_subset = n_after_subset,
      n_missing_from_matrix = n_missing_from_matrix,
      n_incomplete_outcome_or_covariates = n_incomplete_outcome_or_covariates,
      n_samples_modeled = nrow(sample_data),
      n_features_input = ncol(x)
    ),
    sample_id = sample_id_name,
    outcome = outcome_name
  )
}
