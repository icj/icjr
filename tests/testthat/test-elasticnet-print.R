test_that("print.icjr_elasticnet_fit prints a model overview", {
  data <- simulate_gaussian_data()

  fit <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    nfolds = 3,
    n_reps = 3,
    feature_filter = feature_filter_none(),
    seed = 601
  )

  expect_output(print(fit), "Repeated elastic-net model")
  expect_output(print(fit), "Family: gaussian")
  expect_output(print(fit), "Samples modeled: ")
  expect_output(print(fit), "Features modeled: ")
  expect_output(print(fit), "Successful repetitions: ")
  expect_output(print(fit), "Selected features: ")

  printed <- withVisible(capture.output(result <- print(fit)))
  expect_identical(result, fit)
})

test_that("print.summary.icjr_elasticnet_fit prints without error", {
  data <- simulate_gaussian_data()

  fit <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    nfolds = 3,
    n_reps = 3,
    feature_filter = feature_filter_none(),
    seed = 602
  )

  fit_summary <- summary(fit)

  expect_output(print(fit_summary))
  expect_no_error(capture.output(print(summary(fit, n_features = Inf))))
  expect_no_error(capture.output(print(summary(fit, n_features = 1))))
})
