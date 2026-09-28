test_that("notable_features returns a compact classified table", {
  data <- simulate_gaussian_data()

  fit <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    covariates = ~ age + sex,
    nfolds = 3,
    n_reps = 5,
    feature_filter = feature_filter_none(),
    seed = 901
  )

  result <- notable_features(
    fit,
    min_percent = 0,
    min_abs_coefficient = 0
  )

  expect_true(is.data.frame(result))

  expect_true(
    all(
      c(
        "feature",
        "percent",
        "median_coefficient",
        "min_coefficient",
        "max_coefficient",
        "frequency_class",
        "effect_class",
        "signal_class"
      ) %in%
        names(result)
    )
  )

  expect_true(all(result$percent >= 0))
  expect_true(all(abs(result$median_coefficient) >= 0))
})

test_that("notable_features applies thresholds and limits output", {
  data <- simulate_gaussian_data()

  fit <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    nfolds = 3,
    n_reps = 5,
    feature_filter = feature_filter_none(),
    seed = 902
  )

  result <- notable_features(
    fit,
    min_percent = 20,
    min_abs_coefficient = 0.01,
    n_features = 2
  )

  expect_lte(nrow(result), 2)
  expect_true(all(result$percent >= 20))
  expect_true(all(abs(result$median_coefficient) >= 0.01))
})

test_that("notable_features orders features by stability and magnitude", {
  data <- simulate_gaussian_data()

  fit <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    nfolds = 3,
    n_reps = 5,
    feature_filter = feature_filter_none(),
    seed = 903
  )

  result <- notable_features(
    fit,
    min_percent = 0,
    min_abs_coefficient = 0
  )

  expected <- result[
    order(
      -result$percent,
      -abs(result$median_coefficient),
      result$feature
    ),
    ,
    drop = FALSE
  ]

  expect_identical(result, expected)
})

test_that("notable_features supports custom classification cutoffs", {
  data <- simulate_gaussian_data()

  fit <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    nfolds = 3,
    n_reps = 5,
    feature_filter = feature_filter_none(),
    seed = 904
  )

  result <- notable_features(
    fit,
    min_percent = 0,
    min_abs_coefficient = 0,
    frequency_cutoffs = c(
      Infrequent = 0,
      Frequent = 50
    ),
    effect_cutoffs = c(
      Small = 0,
      Large = 0.10
    )
  )

  expect_true(
    all(
      as.character(result$frequency_class) %in%
        c("Infrequent", "Frequent")
    )
  )

  expect_true(
    all(
      as.character(result$effect_class) %in%
        c("Small", "Large")
    )
  )
})

test_that("notable_features returns no rows when nothing meets thresholds", {
  data <- simulate_gaussian_data()

  fit <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    nfolds = 3,
    n_reps = 3,
    feature_filter = feature_filter_none(),
    seed = 905
  )

  result <- notable_features(
    fit,
    min_percent = 100,
    min_abs_coefficient = 100
  )

  expect_equal(nrow(result), 0)

  expect_true(
    all(
      c(
        "feature",
        "percent",
        "median_coefficient",
        "min_coefficient",
        "max_coefficient",
        "frequency_class",
        "effect_class",
        "signal_class"
      ) %in%
        names(result)
    )
  )
})

test_that("notable_features validates arguments", {
  data <- simulate_gaussian_data()

  fit <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    nfolds = 3,
    n_reps = 2,
    feature_filter = feature_filter_none(),
    seed = 906
  )

  expect_error(
    notable_features(fit, min_percent = 101),
    "0 to 100"
  )

  expect_error(
    notable_features(fit, min_abs_coefficient = -0.01),
    "greater than or equal to 0"
  )

  expect_error(
    notable_features(fit, n_features = 0),
    "greater than or equal to 1"
  )

  expect_error(
    notable_features(list()),
    "`x` must be an `icjr_elasticnet_fit` object"
  )
})

test_that("notable_features works for all elastic-net families", {
  gaussian_data <- simulate_gaussian_data()

  gaussian_fit <- fit_elasticnet_gaussian(
    x = gaussian_data$x,
    sample_data = gaussian_data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    nfolds = 3,
    n_reps = 2,
    feature_filter = feature_filter_none(),
    seed = 907
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
    seed = 908
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
    seed = 909
  )

  results <- lapply(
    list(gaussian_fit, binomial_fit, cox_fit),
    function(fit) {
      notable_features(
        fit,
        min_percent = 0,
        min_abs_coefficient = 0
      )
    }
  )

  for (result in results) {
    expect_true(is.data.frame(result))
    expect_true("feature" %in% names(result))
  }
})

test_that("notable_features includes coefficient range summaries", {
  data <- simulate_gaussian_data()

  fit <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    nfolds = 3,
    n_reps = 5,
    feature_filter = feature_filter_none(),
    seed = 910
  )

  result <- notable_features(
    fit,
    min_percent = 0,
    min_abs_coefficient = 0
  )

  expect_true(
    all(
      c("min_coefficient", "max_coefficient") %in% names(result)
    )
  )

  expect_true(
    all(result$min_coefficient <= result$median_coefficient)
  )

  expect_true(
    all(result$median_coefficient <= result$max_coefficient)
  )
})
