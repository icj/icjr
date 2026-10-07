test_that("prepare_binomial_outcome handles logical and numeric outcomes", {
  expect_identical(prepare_binomial_outcome(c(TRUE, FALSE, TRUE)), c(1L, 0L, 1L))
  expect_identical(prepare_binomial_outcome(c(1, 0, 1)), c(1L, 0L, 1L))

  expect_error(
    prepare_binomial_outcome(c(TRUE, FALSE), event_level = "yes"),
    "must be NULL when `outcome` is logical"
  )
  expect_error(
    prepare_binomial_outcome(c(1, 0), event_level = "1"),
    "must be NULL when `outcome` is numeric"
  )
  expect_error(
    prepare_binomial_outcome(c(0, 1, 2)),
    "only finite values 0 and 1"
  )
  expect_error(
    prepare_binomial_outcome(c(0, 1, Inf)),
    "only finite values 0 and 1"
  )
})

test_that("prepare_binomial_outcome rejects unsupported outcomes", {
  expect_error(
    prepare_binomial_outcome(list(1, 0)),
    "must be logical, numeric 0/1"
  )
  expect_error(
    prepare_binomial_outcome(factor(c("a", "b", NA))),
    "missing values"
  )
  expect_error(
    prepare_binomial_outcome(c("a", "b", "c")),
    "exactly two observed levels"
  )
})

test_that("extract_nonzero_coefficients returns empty when coef fails", {
  result <- extract_nonzero_coefficients(
    model = NULL,
    lambda = "lambda.1se",
    terms = c("f1", "f2")
  )

  expect_s3_class(result, "tbl_df")
  expect_identical(nrow(result), 0L)
  expect_named(result, c("feature", "coefficient"))
})

test_that("summarize_selected_features handles empty inputs", {
  from_no_models <- summarize_selected_features(
    coefficient_list = list(),
    n_models_successful = 0L,
    annotation = NULL,
    effect_transform = identity,
    effect_label = "Effect"
  )
  expect_identical(nrow(from_no_models), 0L)
  expect_true("selection_rate" %in% names(from_no_models))

  from_no_selection <- summarize_selected_features(
    coefficient_list = list(
      tibble::tibble(feature = character(), coefficient = numeric())
    ),
    n_models_successful = 3L,
    annotation = NULL,
    effect_transform = identity,
    effect_label = "Effect"
  )
  expect_identical(nrow(from_no_selection), 0L)
})

test_that("summarize_selected_features summarizes and annotates", {
  coefficient_list <- list(
    tibble::tibble(feature = c("f1", "f2"), coefficient = c(0.5, -0.2)),
    tibble::tibble(feature = "f1", coefficient = 0.7),
    tibble::tibble(feature = character(), coefficient = numeric())
  )

  summarized <- summarize_selected_features(
    coefficient_list = coefficient_list,
    n_models_successful = 3L,
    annotation = NULL,
    effect_transform = identity,
    effect_label = "Effect"
  )

  expect_identical(summarized$feature[1], "f1")
  expect_equal(summarized$selection_rate[1], 2 / 3)
  expect_equal(summarized$n_selected, c(2L, 1L))
  expect_equal(summarized$sign_consistency, c(1, 1))

  annotated <- summarize_selected_features(
    coefficient_list = coefficient_list,
    n_models_successful = 3L,
    annotation = tibble::tibble(
      feature = c("f1", "f2"),
      feature_label = c("Feature one", "Feature two")
    ),
    effect_transform = identity,
    effect_label = "Effect"
  )

  expect_identical(names(annotated)[2], "feature_label")
  expect_identical(annotated$feature_label[1], "Feature one")

  expect_error(
    summarize_selected_features(
      coefficient_list = coefficient_list,
      n_models_successful = 3L,
      annotation = data.frame(feature = "f1"),
      effect_transform = identity,
      effect_label = "Effect"
    ),
    "`feature_label`"
  )
})
