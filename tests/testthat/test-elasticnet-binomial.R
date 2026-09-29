test_that("binomial elastic-net fit returns the expected object", {
  data <- simulate_binomial_data()

  fit <- fit_elasticnet_binomial(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    event_level = "case",
    covariates = ~ age + sex,
    nfolds = 3,
    n_reps = 3,
    feature_filter = feature_filter_none(),
    seed = 201
  )

  expect_s3_class(fit, "icjr_elasticnet_fit")
  expect_identical(fit$specification$family, "binomial")
  expect_identical(fit$specification$type_measure, "deviance")
  expect_equal(
    fit$sample_summary$n_cases + fit$sample_summary$n_controls,
    nrow(data$x)
  )
  expect_true(is.data.frame(fit$selected_features))
  expect_true(is.data.frame(fit$covariate_coefficients))
})

test_that("binomial factors require an explicit event level", {
  data <- simulate_binomial_data()

  expect_error(
    fit_elasticnet_binomial(
      x = data$x,
      sample_data = data$sample_data,
      sample_id = sample_id,
      outcome = outcome,
      nfolds = 3,
      n_reps = 2
    ),
    "event_level"
  )
})

test_that("binomial event level must be observed", {
  data <- simulate_binomial_data()

  expect_error(
    fit_elasticnet_binomial(
      x = data$x,
      sample_data = data$sample_data,
      sample_id = sample_id,
      outcome = outcome,
      event_level = "missing_level",
      nfolds = 3,
      n_reps = 2
    ),
    "one observed outcome level"
  )
})

test_that("binomial models accept numeric zero-one outcomes", {
  data <- simulate_binomial_data()

  numeric_data <- data$sample_data |>
    dplyr::mutate(outcome = as.integer(outcome == "case"))

  fit <- fit_elasticnet_binomial(
    x = data$x,
    sample_data = numeric_data,
    sample_id = sample_id,
    outcome = outcome,
    covariates = ~ age + sex,
    nfolds = 3,
    n_reps = 2,
    feature_filter = feature_filter_none(),
    seed = 202
  )

  expect_identical(fit$specification$event_level, NULL)
  expect_equal(
    fit$sample_summary$n_cases,
    sum(numeric_data$outcome == 1L)
  )
})

test_that("binomial models reject invalid numeric outcomes", {
  data <- simulate_binomial_data()

  invalid_data <- data$sample_data |>
    dplyr::mutate(outcome = seq_len(dplyr::n()))

  expect_error(
    fit_elasticnet_binomial(
      x = data$x,
      sample_data = invalid_data,
      sample_id = sample_id,
      outcome = outcome,
      nfolds = 3,
      n_reps = 2
    ),
    "only finite values 0 and 1"
  )
})

test_that("binomial models require both classes after filtering", {
  data <- simulate_binomial_data()

  expect_error(
    fit_elasticnet_binomial(
      x = data$x,
      sample_data = data$sample_data,
      sample_id = sample_id,
      outcome = outcome,
      event_level = "case",
      subset = outcome == "case",
      nfolds = 3,
      n_reps = 2
    ),
    "exactly two observed levels"
  )
})

test_that("binomial fits are invariant to metadata row order", {
  data <- simulate_binomial_data()

  fit_original <- fit_elasticnet_binomial(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    event_level = "case",
    covariates = ~ age + sex,
    nfolds = 3,
    n_reps = 3,
    feature_filter = feature_filter_none(),
    seed = 203
  )

  shuffled_data <- data$sample_data[
    sample(seq_len(nrow(data$sample_data))),
    ,
    drop = FALSE
  ]

  fit_shuffled <- fit_elasticnet_binomial(
    x = data$x,
    sample_data = shuffled_data,
    sample_id = sample_id,
    outcome = outcome,
    event_level = "case",
    covariates = ~ age + sex,
    nfolds = 3,
    n_reps = 3,
    feature_filter = feature_filter_none(),
    seed = 203
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

test_that("Binomial elastic-net results are invariant to worker count", {
  data <- simulate_binomial_data()

  old_plan <- future::plan()

  on.exit(
    future::plan(old_plan),
    add = TRUE
  )

  fit_sequential <- fit_elasticnet_binomial(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    event_level = "case",
    covariates = ~ age + sex,
    nfolds = 3,
    n_reps = 3,
    feature_filter = feature_filter_none(),
    seed = 201,
    workers = 1L
  )

  fit_parallel <- fit_elasticnet_binomial(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    event_level = "case",
    covariates = ~ age + sex,
    nfolds = 3,
    n_reps = 3,
    feature_filter = feature_filter_none(),
    seed = 201,
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
