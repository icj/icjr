#' @noRd
prepare_cox_status <- function(status) {
  if (is.logical(status)) {
    return(as.integer(status))
  }

  if (
    !is.numeric(status) ||
      any(!is.finite(status)) ||
      any(!status %in% c(0, 1))
  ) {
    rlang::abort(
      "`status` must be logical or numeric with finite values 0 and 1."
    )
  }

  as.integer(status)
}

#' Fit repeated elastic-net Cox survival models
#'
#' Fits repeated cross-validated elastic-net Cox proportional-hazards models to
#' identify molecular or other high-dimensional features associated with
#' time-to-event outcomes. Feature-matrix columns are penalized and eligible for
#' selection. Covariates supplied through `covariates` are included without
#' penalization.
#'
#' Feature selection is summarized across repeated cross-validation fits. The
#' returned selection frequency is descriptive stability information and should
#' not be interpreted as a p-value.
#'
#' @param x Numeric matrix with samples in rows and candidate features in
#'   columns. Row names must be unique sample IDs and column names must be
#'   unique feature IDs.
#' @param sample_data Data frame or tibble containing sample IDs, follow-up
#'   time, event status, and optional adjustment covariates.
#' @param sample_id Unquoted sample-ID column or one character string matching
#'   `rownames(x)`.
#' @param time Unquoted numeric follow-up-time column or one character string.
#' @param status Unquoted logical or numeric 0/1 event-status column or one
#'   character string.
#' @param covariates Optional one-sided formula specifying unpenalized
#'   adjustment covariates, such as `~ age + sex + batch`.
#' @param subset Optional filtering expression evaluated in `sample_data`.
#' @param alpha Elastic-net mixing parameter. `0` is ridge, `1` is lasso, and
#'   values between them are elastic-net models.
#' @param nfolds Requested number of cross-validation folds. The number is
#'   reduced automatically if there are fewer samples or observed events than
#'   requested folds.
#' @param n_reps Number of repeated independent cross-validated model fits.
#' @param lambda Coefficient-extraction rule: `"lambda.1se"` or `"lambda.min"`.
#' @param feature_filter An `icjr_feature_filter` object created by a
#'   `feature_filter_*()` function.
#' @param seed Optional integer seed. Repetition `i` uses `seed + i - 1`.
#' @param workers Number of worker processes used for repeated model fitting.
#'   `1` runs sequentially. Values greater than `1` will enable parallel
#'   execution in a future update.
#' @param annotation Optional data frame with `feature` and `feature_label`
#'   columns.
#' @param keep_models Logical; retain successful `cv.glmnet` objects in the
#'   returned fit object.
#'
#' @return An object of class `icjr_elasticnet_fit` containing the modeling
#'   specification, sample accounting, selected-feature summary, covariate
#'   coefficient summary, repetition status, and optionally fitted models.
#' @export
fit_elasticnet_cox <- function(
  x,
  sample_data,
  sample_id,
  time,
  status,
  covariates = NULL,
  subset = NULL,
  alpha = 0.5,
  nfolds = 5,
  n_reps = 100,
  lambda = c("lambda.1se", "lambda.min"),
  feature_filter = feature_filter_mad(0.5),
  seed = NULL,
  workers = 1L,
  annotation = NULL,
  keep_models = FALSE
) {
  lambda <- match.arg(lambda)

  validate_elasticnet_settings(
    alpha = alpha,
    nfolds = nfolds,
    n_reps = n_reps,
    seed = seed,
    lambda = lambda
  )

  if (!is.data.frame(sample_data)) {
    rlang::abort("`sample_data` must be a data frame or tibble.")
  }

  sample_data <- tibble::as_tibble(sample_data)

  sample_id_name <- resolve_column_name(
    data = sample_data,
    column = {{ sample_id }},
    argument = "sample_id"
  )

  time_name <- resolve_column_name(
    data = sample_data,
    column = {{ time }},
    argument = "time"
  )

  status_name <- resolve_column_name(
    data = sample_data,
    column = {{ status }},
    argument = "status"
  )

  subset_expression <- rlang::enquo(subset)

  model_data <- prepare_elasticnet_data(
    x = x,
    sample_data = sample_data,
    sample_id = {{ sample_id }},
    outcome = {{ time }},
    covariates = covariates,
    subset_expression = subset_expression,
    required_columns = c(time_name, status_name)
  )

  time_values <- model_data$required_data[[time_name]]
  status_values <- prepare_cox_status(
    model_data$required_data[[status_name]]
  )

  if (
    !is.numeric(time_values) ||
      any(!is.finite(time_values)) ||
      any(time_values < 0)
  ) {
    rlang::abort(
      "`time` must contain only finite numeric values greater than or equal to 0."
    )
  }

  if (!identical(time_values, model_data$y)) {
    rlang::abort(
      "Internal error: survival time is not aligned with the model outcome."
    )
  }

  n_events <- sum(status_values == 1L)

  if (n_events < 2L) {
    rlang::abort(
      "At least 2 observed events are required for a Cox elastic-net model."
    )
  }

  design <- build_elasticnet_design(
    x = model_data$x,
    covariate_matrix = model_data$covariate_matrix,
    feature_filter = feature_filter
  )

  y <- survival::Surv(
    time = time_values,
    event = status_values
  )

  fitted_models <- fit_repeated_cv_glmnet(
    x = design$x,
    y = y,
    family = "cox",
    alpha = alpha,
    nfolds = min(nfolds, n_events),
    n_reps = n_reps,
    penalty_factor = design$penalty_factor,
    type_measure = "C",
    seed = seed,
    workers = workers
  )

  n_models_successful <- nrow(
    dplyr::filter(
      fitted_models$repetition_summary,
      .data$successful
    )
  )

  feature_coefficients <- lapply(
    fitted_models$models,
    extract_nonzero_coefficients,
    lambda = lambda,
    terms = design$feature_names
  )

  covariate_coefficients <- lapply(
    fitted_models$models,
    extract_nonzero_coefficients,
    lambda = lambda,
    terms = design$covariate_names
  )

  selected_features <- summarize_selected_features(
    coefficient_list = feature_coefficients,
    n_models_successful = n_models_successful,
    annotation = annotation,
    effect_transform = exp,
    effect_label = "Hazard ratio per 1-SD feature increase"
  )

  covariate_summary <- summarize_selected_features(
    coefficient_list = covariate_coefficients,
    n_models_successful = n_models_successful,
    annotation = NULL,
    effect_transform = exp,
    effect_label = "Hazard ratio per 1-SD covariate increase"
  )

  sample_summary <- model_data$sample_summary |>
    dplyr::mutate(
      n_events = n_events,
      n_features_modeled = length(design$feature_names),
      n_reps_requested = n_reps,
      n_reps_successful = n_models_successful,
      nfolds_used = fitted_models$nfolds_used
    )

  structure(
    list(
      call = match.call(),
      specification = list(
        family = "cox",
        sample_id = sample_id_name,
        time = time_name,
        status = status_name,
        covariates = covariates,
        alpha = alpha,
        nfolds_requested = nfolds,
        nfolds_used = fitted_models$nfolds_used,
        n_reps_requested = n_reps,
        lambda = lambda,
        type_measure = "C",
        cox_ties = "breslow",
        feature_filter = feature_filter,
        seed = seed
      ),
      sample_summary = sample_summary,
      feature_names = design$feature_names,
      selected_features = selected_features,
      covariate_coefficients = covariate_summary,
      repetition_summary = fitted_models$repetition_summary,
      models = if (isTRUE(keep_models)) fitted_models$models else NULL
    ),
    class = "icjr_elasticnet_fit"
  )
}
