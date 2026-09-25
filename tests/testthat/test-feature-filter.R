test_that("feature-filter constructors create valid specifications", {
  filter_none <- feature_filter_none()
  filter_mad <- feature_filter_mad()
  filter_variance <- feature_filter_variance(quantile = 0.25)
  filter_threshold <- feature_filter_mad_threshold(threshold = 0.1)

  expect_s3_class(filter_none, "icjr_feature_filter")
  expect_identical(filter_none$method, "none")
  expect_identical(filter_mad$method, "mad_quantile")
  expect_identical(filter_mad$quantile, 0.5)
  expect_identical(filter_variance$method, "variance_quantile")
  expect_identical(filter_variance$quantile, 0.25)
  expect_identical(filter_threshold$method, "mad_threshold")
  expect_identical(filter_threshold$threshold, 0.1)
})

test_that("feature-filter constructors reject invalid values", {
  expect_error(feature_filter_mad(-0.01), "in \\[0, 1\\)")
  expect_error(feature_filter_mad(1), "in \\[0, 1\\)")
  expect_error(feature_filter_variance(NA_real_), "finite numeric")
  expect_error(feature_filter_mad_threshold(NA_real_), "finite numeric")
  expect_error(feature_filter_mad_threshold(Inf), "finite numeric")
})

test_that("none filter retains finite non-constant features", {
  x <- cbind(
    constant = c(1, 1, 1, 1),
    variable = c(1, 2, 3, 4),
    nonfinite = c(1, 2, Inf, 4)
  )
  rownames(x) <- paste0("sample_", seq_len(nrow(x)))

  selected <- icjr:::select_model_features(x, feature_filter_none())

  expect_identical(selected, "variable")
})

test_that("MAD and variance filters retain expected features", {
  x <- cbind(
    low = c(0, 0, 1, 1, 0),
    medium = c(0, 1, 2, 3, 4),
    high = c(0, 4, 8, 12, 16)
  )
  rownames(x) <- paste0("sample_", seq_len(nrow(x)))

  expect_identical(
    icjr:::select_model_features(x, feature_filter_mad(0.5)),
    "high"
  )

  expect_identical(
    icjr:::select_model_features(x, feature_filter_variance(0.5)),
    "high"
  )
})

test_that("feature matrix validation catches invalid identifiers", {
  x <- matrix(seq_len(6), nrow = 3)
  rownames(x) <- c("a", "a", "c")
  colnames(x) <- c("feature_1", "feature_2")

  expect_error(
    icjr:::validate_feature_matrix(x),
    regexp = "must be unique"
  )
})
