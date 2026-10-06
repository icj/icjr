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
    "finite numeric values"
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

test_that("Gaussian elastic-net fit excludes and records incomplete rows", {
  data <- simulate_gaussian_data()

  sample_data_missing <- data$sample_data

  sample_data_missing$outcome[1:2] <- NA_real_
  sample_data_missing$age[3] <- NA_real_

  fit <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = sample_data_missing,
    sample_id = sample_id,
    outcome = outcome,
    covariates = ~ age + sex,
    nfolds = 3,
    n_reps = 2,
    feature_filter = feature_filter_none(),
    seed = 106
  )

  expect_equal(
    fit$sample_summary$n_incomplete_outcome_or_covariates,
    3
  )
  expect_equal(
    fit$sample_summary$n_samples_modeled,
    nrow(data$x) - 3
  )
  expect_true(is.character(fit$feature_names))
  expect_equal(
    length(fit$feature_names),
    fit$sample_summary$n_features_modeled
  )
})

test_that("Gaussian elastic-net fit is invariant to metadata row order", {
  data <- simulate_gaussian_data()

  fit_original <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    covariates = ~ age + sex,
    nfolds = 3,
    n_reps = 3,
    feature_filter = feature_filter_none(),
    seed = 107
  )

  shuffled_data <- data$sample_data[
    sample(seq_len(nrow(data$sample_data))),
    ,
    drop = FALSE
  ]

  fit_shuffled <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = shuffled_data,
    sample_id = sample_id,
    outcome = outcome,
    covariates = ~ age + sex,
    nfolds = 3,
    n_reps = 3,
    feature_filter = feature_filter_none(),
    seed = 107
  )

  expect_identical(
    fit_original$selected_features,
    fit_shuffled$selected_features
  )

  expect_identical(
    fit_original$covariate_coefficients,
    fit_shuffled$covariate_coefficients
  )
})

test_that("shared preparation retains required data in matrix order", {
  data <- simulate_gaussian_data()

  sample_data <- data$sample_data |>
    dplyr::mutate(
      auxiliary = seq_len(dplyr::n())
    )

  sample_data <- sample_data[
    sample(seq_len(nrow(sample_data))),
    ,
    drop = FALSE
  ]

  prepared <- icjr:::prepare_elasticnet_data(
    x = data$x,
    sample_data = sample_data,
    sample_id = sample_id,
    outcome = outcome,
    required_columns = c("outcome", "auxiliary")
  )

  expect_identical(
    rownames(prepared$x),
    prepared$required_data$sample_id
  )

  expect_identical(
    prepared$required_data$outcome,
    prepared$y
  )
})

test_that("shared preparation removes rows missing any required column", {
  data <- simulate_gaussian_data()

  sample_data <- data$sample_data
  sample_data$outcome[1] <- NA_real_
  sample_data$auxiliary <- seq_len(nrow(sample_data))
  sample_data$auxiliary[2] <- NA_integer_

  prepared <- icjr:::prepare_elasticnet_data(
    x = data$x,
    sample_data = sample_data,
    sample_id = sample_id,
    outcome = outcome,
    required_columns = c("outcome", "auxiliary")
  )

  expect_equal(
    prepared$sample_summary$n_incomplete_outcome_or_covariates,
    2
  )

  expect_equal(
    prepared$sample_summary$n_samples_modeled,
    nrow(data$x) - 2
  )
})

test_that("Gaussian elastic-net results are invariant to worker count", {
  data <- simulate_gaussian_data()

  old_plan <- future::plan()

  on.exit(
    future::plan(old_plan),
    add = TRUE
  )

  fit_sequential <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    covariates = ~ age + sex,
    nfolds = 3,
    n_reps = 3,
    feature_filter = feature_filter_none(),
    seed = 106,
    workers = 1L
  )

  fit_parallel <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    covariates = ~ age + sex,
    nfolds = 3,
    n_reps = 3,
    feature_filter = feature_filter_none(),
    seed = 106,
    workers = 2L
  )

  expect_identical(
    fit_sequential$selected_features,
    fit_parallel$selected_features
  )

  expect_identical(
    fit_sequential$covariate_coefficients,
    fit_parallel$covariate_coefficients
  )
})

test_that("Gaussian elastic-net fit accepts variables holding column names", {
  data <- simulate_gaussian_data()

  sample_id_col <- "sample_id"
  outcome_col <- "outcome"

  fit <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id_col,
    outcome = outcome_col,
    covariates = ~ age + sex,
    nfolds = 3,
    n_reps = 2,
    feature_filter = feature_filter_none(),
    seed = 411
  )

  expect_s3_class(fit, "icjr_elasticnet_fit")
  expect_identical(fit$specification$family, "gaussian")
})
