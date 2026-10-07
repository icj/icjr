make_prep_inputs <- function() {
  x <- matrix(
    as.numeric(1:12),
    nrow = 6,
    dimnames = list(paste0("s", 1:6), c("f1", "f2"))
  )

  meta <- data.frame(
    id = paste0("s", 1:6),
    y = c(0.1, 0.5, 0.9, 1.3, 1.7, 2.1),
    age = c(30, 40, 50, 60, 70, 80),
    stringsAsFactors = FALSE
  )

  list(x = x, meta = meta)
}

test_that("prepare_elasticnet_data rejects bad sample IDs", {
  inputs <- make_prep_inputs()

  missing_id <- inputs$meta
  missing_id$id[2] <- NA
  expect_error(
    prepare_elasticnet_data(inputs$x, missing_id, id, y),
    "missing or empty values"
  )

  duplicated_id <- inputs$meta
  duplicated_id$id[2] <- "s1"
  expect_error(
    prepare_elasticnet_data(inputs$x, duplicated_id, id, y),
    "uniquely identify rows"
  )

  unmatched_id <- inputs$meta
  unmatched_id$id <- paste0("other", 1:6)
  expect_error(
    prepare_elasticnet_data(inputs$x, unmatched_id, id, y),
    "matching `sample_id` to `rownames\\(x\\)`"
  )
})

test_that("prepare_elasticnet_data rejects bad covariates", {
  inputs <- make_prep_inputs()

  expect_error(
    prepare_elasticnet_data(
      inputs$x, inputs$meta, id, y,
      covariates = ~ missing_variable
    ),
    "not found in `sample_data`: missing_variable"
  )

  constant_group <- inputs$meta
  constant_group$group <- "a"
  expect_error(
    prepare_elasticnet_data(
      inputs$x, constant_group, id, y,
      covariates = ~ group
    ),
    "Could not construct the covariate model matrix"
  )
})

test_that("prepare_elasticnet_data requires enough complete samples", {
  inputs <- make_prep_inputs()
  inputs$meta$y[1:4] <- NA

  expect_error(
    prepare_elasticnet_data(inputs$x, inputs$meta, id, y),
    "Fewer than 3 complete samples"
  )
})

test_that("prepare_elasticnet_data rejects non-finite feature values", {
  inputs <- make_prep_inputs()
  inputs$x[2, 1] <- Inf

  expect_error(prepare_elasticnet_data(inputs$x, inputs$meta, id, y))
})

test_that("prepare_elasticnet_data aligns and accounts for samples", {
  inputs <- make_prep_inputs()

  meta <- rbind(
    inputs$meta,
    data.frame(id = "s7", y = 2.5, age = 90)
  )
  meta$y[meta$id == "s2"] <- NA
  meta <- meta[c(6, 3, 1, 7, 2, 5, 4), ]

  prepared <- prepare_elasticnet_data(
    x = inputs$x,
    sample_data = meta,
    sample_id = id,
    outcome = y,
    covariates = ~ age
  )

  expect_identical(rownames(prepared$x), c("s1", "s3", "s4", "s5", "s6"))
  expect_identical(rownames(prepared$covariate_matrix), rownames(prepared$x))
  expect_identical(ncol(prepared$covariate_matrix), 1L)
  expect_equal(prepared$y, c(0.1, 0.9, 1.3, 1.7, 2.1))

  expect_identical(prepared$sample_summary$n_metadata_input, 7L)
  expect_identical(prepared$sample_summary$n_missing_from_matrix, 1L)
  expect_identical(
    prepared$sample_summary$n_incomplete_outcome_or_covariates,
    1L
  )
  expect_identical(prepared$sample_summary$n_samples_modeled, 5L)
  expect_identical(prepared$sample_summary$n_features_input, 2L)
})
