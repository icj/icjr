filter_matrix <- function() {
  matrix(
    c(
      1, 2, 3, 4, 5, 6,
      1, 1, 1, 1, 1, 1,
      2, 4, 6, 8, 10, 30,
      0.1, 0.2, 0.1, 0.2, 0.1, 0.2,
      1, 2, NA, 4, 5, 6,
      5, 1, 4, 2, 3, 9
    ),
    nrow = 6,
    dimnames = list(
      paste0("s", 1:6),
      c("a", "const", "b", "small", "has_na", "c")
    )
  )
}

test_that("select_model_features drops constant and non-finite columns", {
  x <- filter_matrix()
  x[1, "b"] <- Inf

  out <- select_model_features(x, feature_filter_none())

  expect_equal(out, c("a", "small", "c"))
})

test_that("select_model_features errors when no feature is eligible", {
  x <- matrix(
    1,
    nrow = 4,
    ncol = 2,
    dimnames = list(paste0("s", 1:4), c("a", "b"))
  )

  expect_error(
    select_model_features(x, feature_filter_none()),
    "No finite, non-constant features"
  )

  x2 <- matrix(
    c(1, NA, 3, 4, Inf, 2, 3, 4),
    nrow = 4,
    dimnames = list(paste0("s", 1:4), c("a", "b"))
  )

  expect_error(
    select_model_features(x2, feature_filter_none()),
    "No finite, non-constant features"
  )
})

test_that("MAD threshold filter keeps features strictly above the threshold", {
  x <- filter_matrix()
  eligible <- c("a", "b", "small", "c")
  mads <- apply(x[, eligible], 2, stats::mad, constant = 1)

  out <- select_model_features(x, feature_filter_mad_threshold(1))

  expect_equal(out, names(mads)[mads > 1])
  expect_false("small" %in% out)
})

test_that("variance quantile filter keeps features above the quantile", {
  x <- filter_matrix()
  eligible <- c("a", "b", "small", "c")
  vars <- apply(x[, eligible], 2, stats::var)
  cutoff <- stats::quantile(vars, probs = 0.25, names = FALSE)

  out <- select_model_features(x, feature_filter_variance(0.25))

  expect_equal(out, names(vars)[vars > cutoff])
})

test_that("filters error when nothing remains", {
  x <- filter_matrix()

  expect_error(
    select_model_features(x, feature_filter_mad_threshold(1000)),
    "No features remain after applying `mad_threshold`"
  )

  one_feature <- x[, "a", drop = FALSE]

  expect_error(
    select_model_features(one_feature, feature_filter_variance(0.5)),
    "No features remain after applying `variance_quantile`"
  )

  expect_error(
    select_model_features(one_feature, feature_filter_mad(0.5)),
    "No features remain after applying `mad_quantile`"
  )
})

test_that("select_model_features validates its inputs first", {
  expect_error(
    select_model_features(filter_matrix(), "none"),
    "feature_filter_"
  )
  expect_error(
    select_model_features(data.frame(a = 1:3), feature_filter_none()),
    "numeric matrix"
  )
})

test_that("validate_feature_filter rejects unknown methods and bad parameters", {
  unknown <- structure(
    list(method = "bogus", quantile = NULL, threshold = NULL),
    class = "icjr_feature_filter"
  )
  expect_error(validate_feature_filter(unknown), "unrecognized method")

  bad_quantile <- new_feature_filter("mad_quantile", quantile = 2)
  expect_error(validate_feature_filter(bad_quantile), "`quantile`")

  bad_threshold <- new_feature_filter("mad_threshold", threshold = NA_real_)
  expect_error(validate_feature_filter(bad_threshold), "`threshold`")

  expect_invisible(validate_feature_filter(feature_filter_none()))
  expect_invisible(validate_feature_filter(feature_filter_mad_threshold(0.5)))
})

test_that("constructors reject malformed thresholds and quantiles", {
  for (bad in list(NA_real_, Inf, "a", c(1, 2), NULL)) {
    expect_error(feature_filter_mad_threshold(bad), "`threshold`")
  }

  for (bad in list(-0.1, 1, NA_real_, Inf, "a", c(0.1, 0.2))) {
    expect_error(feature_filter_mad(bad), "`quantile`")
    expect_error(feature_filter_variance(bad), "`quantile`")
  }
})

test_that("validate_feature_matrix covers every identifier check", {
  good <- matrix(
    1:4,
    nrow = 2,
    dimnames = list(c("s1", "s2"), c("a", "b"))
  )
  expect_invisible(validate_feature_matrix(good))

  expect_error(validate_feature_matrix(matrix(letters[1:4], 2)), "numeric matrix")
  expect_error(validate_feature_matrix(matrix(numeric(0), 0, 2)), "at least one")
  expect_error(validate_feature_matrix(matrix(numeric(0), 2, 0)), "at least one")

  no_rows <- good
  rownames(no_rows) <- NULL
  expect_error(validate_feature_matrix(no_rows), "sample IDs")

  blank_rows <- good
  rownames(blank_rows) <- c("s1", "")
  expect_error(validate_feature_matrix(blank_rows), "sample IDs")

  na_rows <- good
  rownames(na_rows) <- c("s1", NA)
  expect_error(validate_feature_matrix(na_rows), "sample IDs")

  dup_rows <- good
  rownames(dup_rows) <- c("s1", "s1")
  expect_error(validate_feature_matrix(dup_rows), "rownames\\(x\\)` must be unique")

  no_cols <- good
  colnames(no_cols) <- NULL
  expect_error(validate_feature_matrix(no_cols), "feature IDs")

  blank_cols <- good
  colnames(blank_cols) <- c("a", "")
  expect_error(validate_feature_matrix(blank_cols), "feature IDs")

  na_cols <- good
  colnames(na_cols) <- c("a", NA)
  expect_error(validate_feature_matrix(na_cols), "feature IDs")

  dup_cols <- good
  colnames(dup_cols) <- c("a", "a")
  expect_error(validate_feature_matrix(dup_cols), "colnames\\(x\\)` must be unique")
})
