test_that("prepare_elasticnet_data validates arguments", {
  x <- matrix(
    as.numeric(1:8),
    nrow = 4,
    dimnames = list(paste0("s", 1:4), c("f1", "f2"))
  )
  meta <- data.frame(id = paste0("s", 1:4), y = c(0.1, 0.5, 0.9, 1.3), age = 1:4)

  expect_error(
    prepare_elasticnet_data(x, list(), id, y),
    "data frame or tibble"
  )
  expect_error(
    prepare_elasticnet_data(x, meta, id, y, covariates = "age"),
    "one-sided formula"
  )
  expect_error(
    prepare_elasticnet_data(x, meta, "nope", y),
    "`sample_id` must name one column"
  )
  expect_error(
    prepare_elasticnet_data(x, meta, id, y, required_columns = ""),
    "non-empty character vector"
  )
  expect_error(
    prepare_elasticnet_data(x, meta, id, y, required_columns = "zzz"),
    "not found in `sample_data`"
  )
  expect_error(
    prepare_elasticnet_data(
      x, meta, id, y,
      subset_expression = rlang::quo(age > 100)
    ),
    "No metadata rows remain"
  )
})

test_that("resolve_column_name accepts strings, symbols, and variables", {
  meta <- data.frame(id = "a", y = 1)
  col <- "id"

  expect_equal(resolve_column_name(meta, rlang::quo("id"), "sample_id"), "id")
  expect_equal(resolve_column_name(meta, rlang::quo(id), "sample_id"), "id")
  expect_equal(resolve_column_name(meta, rlang::quo(col), "sample_id"), "id")
  expect_error(
    resolve_column_name(meta, rlang::quo(1 + 1), "outcome"),
    "outcome"
  )
})

test_that("resolve_column_name gives a clear error for unknown bare symbols", {
  meta <- data.frame(id = "a", y = 1)

  expect_error(
    resolve_column_name(meta, rlang::quo(nope), "sample_id"),
    "`sample_id` must name one column in `sample_data`"
  )

  not_a_string <- 42
  expect_error(
    resolve_column_name(meta, rlang::quo(not_a_string), "outcome"),
    "`outcome` must name one column"
  )
})
