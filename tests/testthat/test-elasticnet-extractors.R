test_that("elastic-net extractors return fit components", {
  data <- simulate_gaussian_data()

  fit <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    covariates = ~ age + sex,
    nfolds = 3,
    n_reps = 2,
    feature_filter = feature_filter_none(),
    seed = 401
  )

  expect_identical(
    selected_features(fit),
    fit$selected_features
  )

  expect_identical(
    covariate_coefficients(fit),
    fit$covariate_coefficients
  )

  expect_identical(
    model_summary(fit),
    fit$sample_summary
  )

  expect_identical(
    modeled_features(fit),
    fit$feature_names
  )

  expect_identical(
    repetition_summary(fit),
    fit$repetition_summary
  )

  expect_identical(
    feature_standard_deviations(fit),
    fit$feature_sd
  )

  expect_type(feature_standard_deviations(fit), "double")
  expect_named(feature_standard_deviations(fit), modeled_features(fit))
})

test_that("feature standard deviations align with selected features", {
  data <- simulate_gaussian_data()

  fit <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    covariates = ~ age + sex,
    nfolds = 3,
    n_reps = 2,
    feature_filter = feature_filter_none(),
    seed = 405
  )

  feature_sd <- feature_standard_deviations(fit)
  expect_true(length(feature_sd) > 0)
  expect_true(all(is.finite(feature_sd)))
  expect_true(all(feature_sd > 0))
  expect_setequal(names(feature_sd), modeled_features(fit))

  selected <- selected_features(fit)

  if (nrow(selected) > 0) {
    joined <- selected |>
      dplyr::mutate(feature_sd = unname(feature_sd[feature]))

    expect_true(all(is.finite(joined$feature_sd)))
    expect_true(all(joined$feature_sd > 0))
  }
})

test_that("elastic-net extractors dispatch across outcome families", {
  gaussian_data <- simulate_gaussian_data()

  gaussian_fit <- fit_elasticnet_gaussian(
    x = gaussian_data$x,
    sample_data = gaussian_data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    nfolds = 3,
    n_reps = 2,
    feature_filter = feature_filter_none(),
    seed = 402
  )

  binomial_data <- simulate_binomial_data()

  binomial_fit <- fit_elasticnet_binomial(
    x = binomial_data$x,
    sample_data = binomial_data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    event_level = "case",
    nfolds = 3,
    n_reps = 2,
    feature_filter = feature_filter_none(),
    seed = 403
  )

  cox_data <- simulate_cox_data()

  cox_fit <- fit_elasticnet_cox(
    x = cox_data$x,
    sample_data = cox_data$sample_data,
    sample_id = sample_id,
    time = followup_time,
    status = event_status,
    nfolds = 3,
    n_reps = 2,
    feature_filter = feature_filter_none(),
    seed = 404
  )

  fits <- list(gaussian_fit, binomial_fit, cox_fit)

  for (fit in fits) {
    expect_s3_class(selected_features(fit), "tbl_df")
    expect_s3_class(covariate_coefficients(fit), "tbl_df")
    expect_s3_class(model_summary(fit), "tbl_df")
    expect_type(modeled_features(fit), "character")
    expect_s3_class(repetition_summary(fit), "tbl_df")
    expect_type(feature_standard_deviations(fit), "double")
    expect_named(feature_standard_deviations(fit), modeled_features(fit))
  }
})
