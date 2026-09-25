test_that("elastic-net summary returns the expected structure", {
  data <- simulate_gaussian_data()

  fit <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    covariates = ~ age + sex,
    nfolds = 3,
    n_reps = 3,
    feature_filter = feature_filter_none(),
    seed = 501
  )

  fit_summary <- summary(fit)

  expect_s3_class(fit_summary, "summary.icjr_elasticnet_fit")
  expect_identical(
    fit_summary$specification,
    fit$specification
  )
  expect_identical(
    fit_summary$sample_summary,
    model_summary(fit)
  )
  expect_equal(
    fit_summary$n_selected_features,
    nrow(selected_features(fit))
  )
  expect_lte(
    nrow(fit_summary$top_features),
    10
  )
})

test_that("elastic-net summary respects n_features", {
  data <- simulate_gaussian_data()

  fit <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    nfolds = 3,
    n_reps = 3,
    feature_filter = feature_filter_none(),
    seed = 502
  )

  fit_summary <- summary(fit, n_features = 1)

  expect_lte(nrow(fit_summary$top_features), 1)

  expect_error(
    summary(fit, n_features = 0),
    "greater than or equal to 1"
  )
})

test_that("elastic-net summary dispatches across outcome families", {
  gaussian_data <- simulate_gaussian_data()

  gaussian_fit <- fit_elasticnet_gaussian(
    x = gaussian_data$x,
    sample_data = gaussian_data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    nfolds = 3,
    n_reps = 2,
    feature_filter = feature_filter_none(),
    seed = 503
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
    seed = 504
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
    seed = 505
  )

  summaries <- lapply(
    list(gaussian_fit, binomial_fit, cox_fit),
    summary
  )

  for (fit_summary in summaries) {
    expect_s3_class(fit_summary, "summary.icjr_elasticnet_fit")
    expect_true(is.data.frame(fit_summary$top_features))
  }
})

test_that("elastic-net summary print method is informative", {
  data <- simulate_binomial_data()

  fit <- fit_elasticnet_binomial(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    event_level = "case",
    nfolds = 3,
    n_reps = 2,
    feature_filter = feature_filter_none(),
    seed = 506
  )

  expect_output(
    print(summary(fit)),
    "Elastic-net model summary"
  )

  expect_output(
    print(summary(fit)),
    "Cases / controls"
  )
})
