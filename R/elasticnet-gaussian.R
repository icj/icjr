#' Fit repeated elastic-net models for a continuous outcome
#'
#' Fits repeated cross-validated Gaussian elastic-net models to identify
#' molecular or other high-dimensional features associated with a continuous
#' outcome. Feature-matrix columns are penalized and eligible for selection.
#' Covariates supplied through `covariates` are included without penalization.
#'
#' Feature selection is summarized across repeated cross-validation fits. The
#' returned selection frequency is descriptive stability information and should
#' not be interpreted as a p-value.
#'
#' @param x Numeric matrix with samples in rows and candidate features in
#'   columns. Row names must be unique sample IDs and column names must be
#'   unique feature IDs.
#' @param sample_data Data frame or tibble containing sample IDs, outcome, and
#'   optional adjustment covariates.
#' @param sample_id Unquoted sample-ID column or one character string matching
#'   `rownames(x)`.
#' @param outcome Unquoted numeric outcome column or one character string.
#' @param covariates Optional one-sided formula specifying unpenalized
#'   adjustment covariates, such as `~ age + sex + batch`.
#' @param subset Optional filtering expression evaluated in `sample_data`.
#' @param alpha Elastic-net mixing parameter. `0` is ridge, `1` is lasso, and
#'   values between them are elastic-net models.
#' @param nfolds Requested number of cross-validation folds. The number is
#'   reduced automatically if there are fewer samples than requested folds.
#' @param n_reps Number of repeated independent cross-validated model fits.
#' @param lambda Coefficient-extraction rule: `"lambda.1se"` or `"lambda.min"`.
#' @param feature_filter An `icjr_feature_filter` object created by a
#'   `feature_filter_*()` function.
#' @param seed Optional integer seed. Repetition `i` uses `seed + i - 1`.
#' @param workers Number of worker processes used for repeated model fitting.
#'   `1` runs sequentially; values greater than `1` run repetitions in
#'   parallel using a multisession future plan. With the same inputs and
#'   `seed`, analytical results are invariant to `workers`.
#' @param annotation Optional data frame with `feature` and `feature_label`
#'   columns.
#' @param keep_models Logical; retain successful `cv.glmnet` objects in the
#'   returned fit object.
#'
#' @return An object of class `icjr_elasticnet_fit` containing the modeling
#'   specification, sample accounting, selected-feature summary, covariate
#'   coefficient summary, repetition status, and optionally fitted models.
#' @export
fit_elasticnet_gaussian <- function(
  x,
  sample_data,
  sample_id,
  outcome,
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

  subset_expression <- rlang::enquo(subset)
  model_data <- prepare_elasticnet_data(
    x = x,
    sample_data = sample_data,
    sample_id = {{ sample_id }},
    outcome = {{ outcome }},
    covariates = covariates,
    subset_expression = subset_expression
  )

  if (!is.numeric(model_data$y) || any(!is.finite(model_data$y))) {
    rlang::abort(
      "`outcome` must contain only finite numeric values for Gaussian elastic-net models."
    )
  }

  design <- build_elasticnet_design(
    x = model_data$x,
    covariate_matrix = model_data$covariate_matrix,
    feature_filter = feature_filter
  )

  fitted_models <- fit_repeated_cv_glmnet(
    x = design$x,
    y = model_data$y,
    family = "gaussian",
    alpha = alpha,
    nfolds = nfolds,
    n_reps = n_reps,
    penalty_factor = design$penalty_factor,
    type_measure = "mse",
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
    effect_transform = identity,
    effect_label = "Outcome-unit difference per 1-SD feature increase"
  )

  covariate_summary <- summarize_selected_features(
    coefficient_list = covariate_coefficients,
    n_models_successful = n_models_successful,
    annotation = NULL,
    effect_transform = identity,
    effect_label = "Outcome-unit difference per 1-SD covariate increase"
  )

  sample_summary <- model_data$sample_summary |>
    dplyr::mutate(
      n_features_modeled = length(design$feature_names),
      n_reps_requested = n_reps,
      n_reps_successful = n_models_successful,
      nfolds_used = fitted_models$nfolds_used
    )

  structure(
    list(
      call = match.call(),
      specification = list(
        family = "gaussian",
        sample_id = model_data$sample_id,
        outcome = model_data$outcome,
        covariates = covariates,
        alpha = alpha,
        nfolds_requested = nfolds,
        nfolds_used = fitted_models$nfolds_used,
        n_reps_requested = n_reps,
        lambda = lambda,
        feature_filter = feature_filter,
        seed = seed
      ),
      sample_summary = sample_summary,
      selected_features = selected_features,
      feature_names = design$feature_names,
      covariate_coefficients = covariate_summary,
      repetition_summary = fitted_models$repetition_summary,
      models = if (isTRUE(keep_models)) fitted_models$models else NULL
    ),
    class = "icjr_elasticnet_fit"
  )
}
