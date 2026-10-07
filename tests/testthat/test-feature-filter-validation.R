test_that("validate_feature_filter rejects bad inputs", {
  expect_error(validate_feature_filter(list()), "feature_filter_")

  bad_method <- feature_filter_mad()
  bad_method$method <- "bogus"
  expect_error(validate_feature_filter(bad_method), "unrecognized method")

  bad_quantile <- feature_filter_variance()
  bad_quantile$quantile <- 1.5
  expect_error(validate_feature_filter(bad_quantile), "quantile")

  bad_threshold <- feature_filter_mad_threshold(1)
  bad_threshold$threshold <- Inf
  expect_error(validate_feature_filter(bad_threshold), "threshold")
})

test_that("validate_filter_quantile rejects invalid values", {
  for (q in list(c(0.1, 0.2), NA_real_, Inf, -0.1, 1, "a")) {
    expect_error(validate_filter_quantile(q), "quantile")
  }
})

test_that("validate_feature_matrix rejects malformed matrices", {
  good <- matrix(
    as.numeric(1:6),
    nrow = 2,
    dimnames = list(c("s1", "s2"), c("f1", "f2", "f3"))
  )
  expect_no_error(validate_feature_matrix(good))

  expect_error(validate_feature_matrix(data.frame(good)), "numeric matrix")
  expect_error(
    validate_feature_matrix(matrix(numeric(0), 0, 3)),
    "at least one"
  )

  no_rows <- good
  rownames(no_rows) <- NULL
  expect_error(validate_feature_matrix(no_rows), "sample IDs")

  dup_rows <- good
  rownames(dup_rows) <- c("s1", "s1")
  expect_error(validate_feature_matrix(dup_rows), "unique")

  no_cols <- good
  colnames(no_cols) <- NULL
  expect_error(validate_feature_matrix(no_cols), "feature IDs")

  dup_cols <- good
  colnames(dup_cols) <- c("f1", "f1", "f2")
  expect_error(validate_feature_matrix(dup_cols), "unique")
})
