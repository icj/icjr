test_that("Gaussian elastic-net fit returns the expected object", {
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
    seed = 101
  )

  expect_s3_class(fit, "icjr_elasticnet_fit")
  expect_identical(fit$specification$family, "gaussian")
  expect_equal(fit$sample_summary$n_samples_modeled, nrow(data$x))
  expect_equal(fit$sample_summary$n_reps_requested, 3)
  expect_equal(nrow(fit$repetition_summary), 3)
  expect_true(is.data.frame(fit$selected_features))
  expect_true(is.data.frame(fit$covariate_coefficients))
})

test_that("Gaussian elastic-net fit aligns metadata to matrix row names", {
  data <- simulate_gaussian_data()

  shuffled_data <- data$sample_data[
    sample(seq_len(nrow(data$sample_data))),
    ,
    drop = FALSE
  ]

  fit <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = shuffled_data,
    sample_id = sample_id,
    outcome = outcome,
    nfolds = 3,
    n_reps = 2,
    feature_filter = feature_filter_none(),
    seed = 102
  )

  expect_equal(fit$sample_summary$n_samples_modeled, nrow(data$x))
  expect_equal(fit$sample_summary$n_missing_from_matrix, 0)
})

test_that("Gaussian elastic-net fit records samples missing from the matrix", {
  data <- simulate_gaussian_data()

  sample_data <- dplyr::bind_rows(
    data$sample_data,
    tibble::tibble(
      sample_id = "absent_sample",
      age = 55,
      sex = "female",
      outcome = 0
    )
  )

  fit <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = sample_data,
    sample_id = sample_id,
    outcome = outcome,
    nfolds = 3,
    n_reps = 2,
    feature_filter = feature_filter_none(),
    seed = 103
  )

  expect_equal(fit$sample_summary$n_missing_from_matrix, 1)
  expect_equal(fit$sample_summary$n_samples_modeled, nrow(data$x))
})

test_that("Gaussian elastic-net fit supports metadata subsets", {
  data <- simulate_gaussian_data()

  fit <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    subset = sex == "female",
    nfolds = 3,
    n_reps = 2,
    feature_filter = feature_filter_none(),
    seed = 104
  )

  expected_n <- sum(data$sample_data$sex == "female")

  expect_equal(fit$sample_summary$n_after_subset, expected_n)
  expect_equal(fit$sample_summary$n_samples_modeled, expected_n)
})

test_that("Gaussian elastic-net fit validates a numeric outcome", {
  data <- simulate_gaussian_data()

  invalid_data <- data$sample_data |>
    dplyr::mutate(outcome = as.character(outcome))

  expect_error(
    fit_elasticnet_gaussian(
      x = data$x,
      sample_data = invalid_data,
      sample_id = sample_id,
      outcome = outcome,
      nfolds = 3,
      n_reps = 2
    ),
    "must be numeric"
  )
})

test_that("Gaussian elastic-net fit is reproducible with a fixed seed", {
  data <- simulate_gaussian_data()

  fit_one <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    nfolds = 3,
    n_reps = 3,
    feature_filter = feature_filter_none(),
    seed = 105
  )

  fit_two <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    nfolds = 3,
    n_reps = 3,
    feature_filter = feature_filter_none(),
    seed = 105
  )

  expect_identical(
    fit_one$selected_features,
    fit_two$selected_features
  )
})
