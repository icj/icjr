test_that("validate_elasticnet_settings accepts valid settings", {
  expect_true(
    validate_elasticnet_settings(
      alpha = 0.5, nfolds = 5, n_reps = 3, seed = 1, lambda = "lambda.1se"
    )
  )
  expect_true(
    validate_elasticnet_settings(
      alpha = 0, nfolds = 2, n_reps = 1, seed = NULL, lambda = "lambda.min"
    )
  )
})

test_that("validate_elasticnet_settings rejects invalid settings", {
  ok <- list(
    alpha = 0.5, nfolds = 5, n_reps = 3, seed = 1, lambda = "lambda.1se"
  )
  check <- function(...) {
    do.call(validate_elasticnet_settings, utils::modifyList(ok, list(...)))
  }

  expect_error(check(alpha = -0.1), "`alpha`")
  expect_error(check(alpha = 1.5), "`alpha`")
  expect_error(check(alpha = NA_real_), "`alpha`")
  expect_error(check(alpha = c(0.1, 0.2)), "`alpha`")
  expect_error(check(alpha = "a"), "`alpha`")

  expect_error(check(nfolds = 1), "`nfolds`")
  expect_error(check(nfolds = 2.5), "`nfolds`")
  expect_error(check(nfolds = NA_real_), "`nfolds`")

  expect_error(check(n_reps = 0), "`n_reps`")
  expect_error(check(n_reps = 1.5), "`n_reps`")
  expect_error(check(n_reps = NA_real_), "`n_reps`")

  expect_error(check(seed = 1.5), "`seed`")
  expect_error(check(seed = NA_real_), "`seed`")
  expect_error(check(seed = c(1, 2)), "`seed`")
  expect_error(check(seed = "a"), "`seed`")

  expect_error(check(lambda = "lambda.max"), "`lambda`")
  expect_error(check(lambda = c("lambda.min", "lambda.1se")), "`lambda`")
})

test_that("validate_workers accepts whole numbers and rejects others", {
  expect_identical(validate_workers(1), 1L)
  expect_identical(validate_workers(2L), 2L)

  expect_error(validate_workers(0), "`workers`")
  expect_error(validate_workers(1.5), "`workers`")
  expect_error(validate_workers(NA_real_), "`workers`")
  expect_error(validate_workers(Inf), "`workers`")
  expect_error(validate_workers(c(1, 2)), "`workers`")
  expect_error(validate_workers("a"), "`workers`")
})
