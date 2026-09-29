test_that("Cox elastic-net fit returns the expected object", {
  data <- simulate_cox_data()

  fit <- fit_elasticnet_cox(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    time = followup_time,
    status = event_status,
    covariates = ~ age + sex,
    nfolds = 3,
    n_reps = 3,
    feature_filter = feature_filter_none(),
    seed = 301
  )

  expect_s3_class(fit, "icjr_elasticnet_fit")
  expect_identical(fit$specification$family, "cox")
  expect_identical(fit$specification$type_measure, "C")
  expect_identical(fit$specification$cox_ties, "breslow")
  expect_equal(
    fit$sample_summary$n_events,
    sum(data$sample_data$event_status == 1L)
  )
  expect_true(is.data.frame(fit$selected_features))
  expect_true(is.data.frame(fit$covariate_coefficients))
})

test_that("Cox elastic-net fit accepts logical status", {
  data <- simulate_cox_data()

  logical_data <- data$sample_data |>
    dplyr::mutate(event_status = as.logical(event_status))

  fit <- fit_elasticnet_cox(
    x = data$x,
    sample_data = logical_data,
    sample_id = sample_id,
    time = followup_time,
    status = event_status,
    nfolds = 3,
    n_reps = 2,
    feature_filter = feature_filter_none(),
    seed = 302
  )

  expect_equal(
    fit$sample_summary$n_events,
    sum(logical_data$event_status)
  )
})

test_that("Cox elastic-net fit rejects invalid follow-up time", {
  data <- simulate_cox_data()

  invalid_data <- data$sample_data
  invalid_data$followup_time[1] <- -1

  expect_error(
    fit_elasticnet_cox(
      x = data$x,
      sample_data = invalid_data,
      sample_id = sample_id,
      time = followup_time,
      status = event_status,
      nfolds = 3,
      n_reps = 2
    ),
    "greater than or equal to 0"
  )
})

test_that("Cox elastic-net fit rejects invalid status", {
  data <- simulate_cox_data()

  invalid_data <- data$sample_data
  invalid_data$event_status[1] <- 2

  expect_error(
    fit_elasticnet_cox(
      x = data$x,
      sample_data = invalid_data,
      sample_id = sample_id,
      time = followup_time,
      status = event_status,
      nfolds = 3,
      n_reps = 2
    ),
    "finite values 0 and 1"
  )
})

test_that("Cox elastic-net fit requires observed events", {
  data <- simulate_cox_data()

  no_event_data <- data$sample_data |>
    dplyr::mutate(event_status = 0L)

  expect_error(
    fit_elasticnet_cox(
      x = data$x,
      sample_data = no_event_data,
      sample_id = sample_id,
      time = followup_time,
      status = event_status,
      nfolds = 3,
      n_reps = 2
    ),
    "At least 2 observed events"
  )
})

test_that("Cox elastic-net fit records incomplete time or status", {
  data <- simulate_cox_data()

  incomplete_data <- data$sample_data
  incomplete_data$followup_time[1] <- NA_real_
  incomplete_data$event_status[2] <- NA_integer_

  fit <- fit_elasticnet_cox(
    x = data$x,
    sample_data = incomplete_data,
    sample_id = sample_id,
    time = followup_time,
    status = event_status,
    nfolds = 3,
    n_reps = 2,
    feature_filter = feature_filter_none(),
    seed = 303
  )

  expect_equal(
    fit$sample_summary$n_incomplete_outcome_or_covariates,
    2
  )
  expect_equal(
    fit$sample_summary$n_samples_modeled,
    nrow(data$x) - 2
  )
})

test_that("Cox fits are invariant to metadata row order", {
  data <- simulate_cox_data()

  fit_original <- fit_elasticnet_cox(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    time = followup_time,
    status = event_status,
    covariates = ~ age + sex,
    nfolds = 3,
    n_reps = 3,
    feature_filter = feature_filter_none(),
    seed = 304
  )

  shuffled_data <- data$sample_data[
    sample(seq_len(nrow(data$sample_data))),
    ,
    drop = FALSE
  ]

  fit_shuffled <- fit_elasticnet_cox(
    x = data$x,
    sample_data = shuffled_data,
    sample_id = sample_id,
    time = followup_time,
    status = event_status,
    covariates = ~ age + sex,
    nfolds = 3,
    n_reps = 3,
    feature_filter = feature_filter_none(),
    seed = 304
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

test_that("Cox elastic-net results are invariant to worker count", {
  data <- simulate_cox_data()

  old_plan <- future::plan()

  on.exit(
    future::plan(old_plan),
    add = TRUE
  )

  fit_sequential <- fit_elasticnet_cox(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    time = followup_time,
    status = event_status,
    covariates = ~ age + sex,
    nfolds = 3,
    n_reps = 3,
    feature_filter = feature_filter_none(),
    seed = 304,
    workers = 1L
  )

  fit_parallel <- fit_elasticnet_cox(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    time = followup_time,
    status = event_status,
    covariates = ~ age + sex,
    nfolds = 3,
    n_reps = 3,
    feature_filter = feature_filter_none(),
    seed = 304,
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
