#' @noRd
resolve_column_name <- function(data, column, argument) {
  column_expression <- rlang::get_expr(column)
  column_environment <- rlang::get_env(column)

  if (is.character(column_expression)) {
    column_name <- rlang::as_string(column_expression)
  } else if (rlang::is_symbol(column_expression)) {
    symbol_name <- rlang::as_name(column_expression)

    column_name <- if (symbol_name %in% names(data)) {
      symbol_name
    } else {
      value <- tryCatch(
        rlang::eval_tidy(
          column_expression,
          env = column_environment
        ),
        error = function(condition) NULL
      )

      if (
        is.character(value) &&
          length(value) == 1L &&
          !is.na(value)
      ) {
        value
      } else {
        symbol_name
      }
    }
  } else {
    column_name <- NA_character_
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
  subset_expression = NULL,
  required_columns = NULL
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
    column = rlang::enquo(sample_id),
    argument = "sample_id"
  )

  outcome_name <- resolve_column_name(
    data = sample_data,
    column = rlang::enquo(outcome),
    argument = "outcome"
  )

  if (is.null(required_columns)) {
    required_columns <- outcome_name
  }

  if (
    !is.character(required_columns) ||
      length(required_columns) < 1L ||
      anyNA(required_columns) ||
      any(required_columns == "")
  ) {
    rlang::abort(
      "`required_columns` must be a non-empty character vector of column names."
    )
  }

  required_columns <- unique(required_columns)

  missing_required_columns <- setdiff(required_columns, names(sample_data))

  if (length(missing_required_columns) > 0L) {
    rlang::abort(
      paste0(
        "`required_columns` refers to column(s) not found in `sample_data`: ",
        paste(missing_required_columns, collapse = ", "),
        "."
      )
    )
  }

  n_metadata_input <- nrow(sample_data)

  if (
    !is.null(subset_expression) &&
      !rlang::quo_is_null(subset_expression)
  ) {
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

  covariate_variables <- if (is.null(covariates)) {
    character()
  } else {
    all.vars(covariates)
  }

  missing_covariate_variables <- setdiff(
    covariate_variables,
    names(sample_data)
  )

  if (length(missing_covariate_variables) > 0L) {
    rlang::abort(
      paste0(
        "`covariates` refers to column(s) not found in `sample_data`: ",
        paste(missing_covariate_variables, collapse = ", "),
        "."
      )
    )
  }

  complete_required <- stats::complete.cases(
    sample_data[, required_columns, drop = FALSE]
  )

  complete_covariates <- if (length(covariate_variables) == 0L) {
    rep(TRUE, nrow(sample_data))
  } else {
    stats::complete.cases(
      sample_data[, covariate_variables, drop = FALSE]
    )
  }

  complete_rows <- complete_required & complete_covariates

  n_incomplete_outcome_or_covariates <- sum(!complete_rows)

  sample_data <- sample_data[complete_rows, , drop = FALSE]

  if (nrow(sample_data) < 3L) {
    rlang::abort("Fewer than 3 complete samples remain for modeling.")
  }

  covariate_matrix <- if (is.null(covariates)) {
    NULL
  } else {
    tryCatch(
      stats::model.matrix(
        covariates,
        data = sample_data
      )[, -1, drop = FALSE],
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

  if (!is.null(covariate_matrix)) {
    rownames(covariate_matrix) <- sample_data[[sample_id_name]]
  }

  modeled_ids <- sample_data[[sample_id_name]]

  matrix_order_ids <- rownames(x)[rownames(x) %in% modeled_ids]

  metadata_order <- match(
    matrix_order_ids,
    sample_data[[sample_id_name]]
  )

  if (anyNA(metadata_order)) {
    rlang::abort(
      "Internal error: unable to align metadata to feature-matrix sample IDs."
    )
  }

  sample_data <- sample_data[metadata_order, , drop = FALSE]

  if (!is.null(covariate_matrix)) {
    covariate_matrix <- covariate_matrix[metadata_order, , drop = FALSE]
  }

  x_model <- x[matrix_order_ids, , drop = FALSE]

  if (!identical(rownames(x_model), sample_data[[sample_id_name]])) {
    rlang::abort(
      "Internal error: feature-matrix and metadata sample IDs are not aligned."
    )
  }

  if (
    !is.null(covariate_matrix) &&
      !identical(rownames(covariate_matrix), rownames(x_model))
  ) {
    rlang::abort(
      "Internal error: covariates and feature matrix are not aligned."
    )
  }

  if (any(!is.finite(x_model))) {
    rlang::abort(
      "`x` contains missing or non-finite values among modeled samples."
    )
  }

  list(
    x = x_model,
    y = sample_data[[outcome_name]],
    required_data = sample_data[,
      unique(c(sample_id_name, required_columns)),
      drop = FALSE
    ],
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
