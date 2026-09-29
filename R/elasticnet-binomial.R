#' @noRd
prepare_binomial_outcome <- function(y, event_level = NULL) {
  if (is.logical(y)) {
    if (!is.null(event_level)) {
      rlang::abort(
        "`event_level` must be NULL when `outcome` is logical."
      )
    }

    return(as.integer(y))
  }

  if (is.numeric(y)) {
    if (!is.null(event_level)) {
      rlang::abort(
        "`event_level` must be NULL when `outcome` is numeric 0/1."
      )
    }

    if (any(!is.finite(y)) || any(!y %in% c(0, 1))) {
      rlang::abort(
        "Numeric `outcome` must contain only finite values 0 and 1."
      )
    }

    return(as.integer(y))
  }

  if (!is.factor(y) && !is.character(y)) {
    rlang::abort(
      paste0(
        "`outcome` must be logical, numeric 0/1, or a two-level ",
        "factor/character vector for binomial elastic-net models."
      )
    )
  }

  if (anyNA(y)) {
    rlang::abort(
      "`outcome` contains missing values after data preparation."
    )
  }

  outcome_levels <- unique(as.character(y))

  if (length(outcome_levels) != 2L) {
    rlang::abort(
      paste0(
        "`outcome` must have exactly two observed levels after filtering ",
        "for binomial elastic-net models."
      )
    )
  }

  if (is.null(event_level) || length(event_level) != 1L || is.na(event_level)) {
    rlang::abort(
      "`event_level` must specify which outcome level is coded as 1."
    )
  }

  event_level <- as.character(event_level)

  if (!event_level %in% outcome_levels) {
    rlang::abort(
      paste0(
        "`event_level` must be one observed outcome level: ",
        paste(outcome_levels, collapse = ", "),
        "."
      )
    )
  }

  as.integer(as.character(y) == event_level)
}

#' Fit repeated elastic-net models for a binary outcome
#'
#' Fits repeated cross-validated binomial elastic-net models to identify
#' molecular or other high-dimensional features associated with a binary
#' outcome. Feature-matrix columns are penalized and eligible for selection.
#' Covariates supplied through `covariates` are included without penalization.
#'
#' For a factor or character outcome, `event_level` explicitly identifies the
#' level coded as 1. Positive coefficients and odds ratios above 1 therefore
#' indicate association with higher odds of that specified event level.
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
#' @param outcome Unquoted binary outcome column or one character string.
#' @param event_level For a factor or character outcome, the single outcome
#'   level to code as 1. Must be NULL for logical or numeric 0/1 outcomes.
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
fit_elasticnet_binomial <- function(
  x,
  sample_data,
  sample_id,
  outcome,
  event_level = NULL,
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

  model_data$y <- prepare_binomial_outcome(
    y = model_data$y,
    event_level = event_level
  )

  if (length(unique(model_data$y)) != 2L) {
    rlang::abort(
      "Both outcome classes must be present after filtering and data preparation."
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
    family = "binomial",
    alpha = alpha,
    nfolds = nfolds,
    n_reps = n_reps,
    penalty_factor = design$penalty_factor,
    type_measure = "deviance",
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
    effect_label = "Odds ratio per 1-SD feature increase"
  )

  covariate_summary <- summarize_selected_features(
    coefficient_list = covariate_coefficients,
    n_models_successful = n_models_successful,
    annotation = NULL,
    effect_transform = exp,
    effect_label = "Odds ratio per 1-SD covariate increase"
  )

  sample_summary <- model_data$sample_summary |>
    dplyr::mutate(
      n_cases = sum(model_data$y == 1L),
      n_controls = sum(model_data$y == 0L),
      n_features_modeled = length(design$feature_names),
      n_reps_requested = n_reps,
      n_reps_successful = n_models_successful,
      nfolds_used = fitted_models$nfolds_used
    )

  structure(
    list(
      call = match.call(),
      specification = list(
        family = "binomial",
        sample_id = model_data$sample_id,
        outcome = model_data$outcome,
        event_level = event_level,
        covariates = covariates,
        alpha = alpha,
        nfolds_requested = nfolds,
        nfolds_used = fitted_models$nfolds_used,
        n_reps_requested = n_reps,
        lambda = lambda,
        type_measure = "deviance",
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
