plot_helper_fit <- function() {
  data <- simulate_gaussian_data()

  fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    covariates = ~ age + sex,
    nfolds = 3,
    n_reps = 3,
    feature_filter = feature_filter_none(),
    seed = 601
  )
}

test_that("prepare_elasticnet_plot_data validates its inputs", {
  fit <- plot_helper_fit()

  expect_error(
    prepare_elasticnet_plot_data(list(), 5, 0, TRUE),
    "icjr_elasticnet_fit"
  )

  for (bad in list(0, NA_real_, "a", c(1, 2))) {
    expect_error(
      prepare_elasticnet_plot_data(fit, bad, 0, TRUE),
      "`n_features`"
    )
  }

  for (bad in list(-1, 101, NA_real_, "a", c(1, 2))) {
    expect_error(
      prepare_elasticnet_plot_data(fit, 5, bad, TRUE),
      "`min_percent`"
    )
  }

  for (bad in list(NA, "yes", c(TRUE, FALSE))) {
    expect_error(
      prepare_elasticnet_plot_data(fit, 5, 0, bad),
      "`labels`"
    )
  }
})

test_that("prepare_elasticnet_plot_data errors when nothing passes min_percent", {
  fit <- plot_helper_fit()

  # selected_features() percent is capped at 100, so a threshold of exactly
  # 100 can still pass; use a mock to force an empty result.
  local_mocked_bindings(
    selected_features = function(x, ...) {
      data.frame(
        feature = "f1",
        percent = 10,
        median_coefficient = 0.5,
        stringsAsFactors = FALSE
      )
    }
  )

  expect_error(
    prepare_elasticnet_plot_data(fit, 5, 50, TRUE),
    "min_percent"
  )
})

test_that("prepare_elasticnet_plot_data orders, labels, and classifies features", {
  fit <- plot_helper_fit()

  local_mocked_bindings(
    selected_features = function(x, ...) {
      data.frame(
        feature = c("a", "b", "c", "d"),
        percent = c(80, 100, 80, 60),
        median_coefficient = c(-2, 5, 2, 0),
        median_effect = c(-0.2, 0.5, 0.2, 0),
        feature_label = c("Alpha", NA, "Alpha", ""),
        stringsAsFactors = FALSE
      )
    }
  )

  out <- prepare_elasticnet_plot_data(fit, 3, 0, TRUE)

  expect_equal(nrow(out), 3L)
  expect_equal(out$feature, c("b", "a", "c"))
  expect_equal(out$plot_label, c("b", "Alpha", "Alpha.1"))
  expect_equal(out$direction, c("Positive", "Negative", "Positive"))
  expect_equal(out$absolute_coefficient, c(0.5, 0.2, 0.2))

  all_rows <- prepare_elasticnet_plot_data(fit, 10, 0, TRUE)
  expect_equal(all_rows$direction[all_rows$feature == "d"], "Zero")
  expect_equal(all_rows$plot_label[all_rows$feature == "d"], "d")

  raw <- prepare_elasticnet_plot_data(fit, 3, 0, FALSE)
  expect_equal(raw$plot_label, c("b", "a", "c"))
})

test_that("validate_elasticnet_plot_labels accepts NULL and strings", {
  expect_invisible(
    validate_elasticnet_plot_labels(NULL, NULL, NULL, NULL, NULL, NULL)
  )
  expect_invisible(
    validate_elasticnet_plot_labels("t", "s", "c", "g", "x", "y")
  )
})

test_that("validate_elasticnet_plot_labels rejects invalid labels", {
  for (bad in list(NA_character_, 1, c("a", "b"), character(0))) {
    expect_error(
      validate_elasticnet_plot_labels(bad, NULL, NULL, NULL, NULL, NULL),
      "Plot labels"
    )
    expect_error(
      validate_elasticnet_plot_labels(NULL, NULL, NULL, NULL, NULL, bad),
      "Plot labels"
    )
  }
})

test_that("elasticnet_effect_label matches the model family", {
  expect_equal(
    elasticnet_effect_label(list(specification = list(family = "gaussian"))),
    "Median standardized coefficient (per 1-SD increase)"
  )
  expect_equal(
    elasticnet_effect_label(list(specification = list(family = "binomial"))),
    "Median standardized log odds ratio (per 1-SD increase)"
  )
  expect_equal(
    elasticnet_effect_label(list(specification = list(family = "cox"))),
    "Median standardized log hazard ratio (per 1-SD increase)"
  )
  expect_equal(
    elasticnet_effect_label(list(specification = list(family = "other"))),
    "Median standardized coefficient (per 1-SD increase)"
  )
})
