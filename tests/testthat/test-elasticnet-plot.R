test_that("elastic-net plot returns a ggplot object", {
  data <- simulate_gaussian_data()

  fit <- fit_elasticnet_gaussian(
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

  plot <- plot(
    fit,
    n_features = 5,
    stability_threshold = 50
  )

  expect_s3_class(plot, "ggplot")

  plot_data <- plot$data

  expect_true(is.data.frame(plot_data))
  expect_lte(nrow(plot_data), 5)
  expect_true(all(plot_data$percent >= 0))
  expect_true(all(plot_data$percent <= 100))
  expect_true(all(c("plot_label", "direction") %in% names(plot_data)))
})

test_that("elastic-net plot respects min_percent", {
  data <- simulate_gaussian_data()

  fit <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    nfolds = 3,
    n_reps = 3,
    feature_filter = feature_filter_none(),
    seed = 602
  )

  plot <- plot(
    fit,
    n_features = Inf,
    min_percent = 50
  )

  plot_data <- plot$data

  expect_true(all(plot_data$percent >= 50))
})

test_that("elastic-net plot validates arguments", {
  data <- simulate_gaussian_data()

  fit <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    nfolds = 3,
    n_reps = 2,
    feature_filter = feature_filter_none(),
    seed = 603
  )

  expect_error(
    plot(fit, n_features = 0),
    "greater than or equal to 1"
  )

  expect_error(
    plot(fit, min_percent = 101),
    "0 to 100"
  )

  expect_error(
    plot(fit, stability_threshold = -1),
    "0 to 100"
  )

  expect_error(
    plot(fit, labels = "yes"),
    "must be.*TRUE.*FALSE"
  )
})

test_that("elastic-net plot dispatches across outcome families", {
  gaussian_data <- simulate_gaussian_data()

  gaussian_fit <- fit_elasticnet_gaussian(
    x = gaussian_data$x,
    sample_data = gaussian_data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    nfolds = 3,
    n_reps = 2,
    feature_filter = feature_filter_none(),
    seed = 604
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
    seed = 605
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
    seed = 606
  )

  expect_s3_class(plot(gaussian_fit), "ggplot")
  expect_s3_class(plot(binomial_fit), "ggplot")
  expect_s3_class(plot(cox_fit), "ggplot")
})

test_that("elastic-net plot uses the requested stability threshold", {
  data <- simulate_gaussian_data()

  fit <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    nfolds = 3,
    n_reps = 3,
    feature_filter = feature_filter_none(),
    seed = 607
  )

  plot <- plot(fit, stability_threshold = 50)

  expect_true(
    any(vapply(
      plot$layers,
      function(layer) inherits(layer$geom, "GeomVline"),
      logical(1)
    ))
  )
})

test_that("elastic-net plot omits the threshold layer when requested", {
  data <- simulate_gaussian_data()

  fit <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    nfolds = 3,
    n_reps = 3,
    feature_filter = feature_filter_none(),
    seed = 608
  )

  plot <- plot(fit, stability_threshold = NULL)

  expect_false(
    any(vapply(
      plot$layers,
      function(layer) inherits(layer$geom, "GeomVline"),
      logical(1)
    ))
  )
})

test_that("elastic-net plot accepts explicit plot labels", {
  data <- simulate_gaussian_data()

  fit <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    nfolds = 3,
    n_reps = 3,
    feature_filter = feature_filter_none(),
    seed = 610
  )

  plot <- plot(
    fit,
    title = "Custom title",
    subtitle = "Custom subtitle",
    caption = "Custom caption",
    tag = "A",
    x_label = "Custom x-axis",
    y_label = "Custom y-axis"
  )

  expect_s3_class(plot, "ggplot")
  expect_identical(plot$labels$title, "Custom title")
  expect_identical(plot$labels$subtitle, "Custom subtitle")
  expect_identical(plot$labels$caption, "Custom caption")
  expect_identical(plot$labels$tag, "A")
  expect_identical(plot$labels$x, "Custom x-axis")
  expect_identical(plot$labels$y, "Custom y-axis")
})

test_that("elastic-net plot rejects unused arguments", {
  data <- simulate_gaussian_data()

  fit <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    nfolds = 3,
    n_reps = 2,
    feature_filter = feature_filter_none(),
    seed = 611
  )

  expect_error(
    plot(fit, alpha = 0.5),
    "unused"
  )
})

test_that("plot method delegates to the stability plot", {
  data <- simulate_gaussian_data()

  fit <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    nfolds = 3,
    n_reps = 3,
    feature_filter = feature_filter_none(),
    seed = 701
  )

  method_plot <- plot(fit, n_features = 5)
  stability_plot <- plot_elasticnet_stability(fit, n_features = 5)

  expect_s3_class(method_plot, "ggplot")
  expect_s3_class(stability_plot, "ggplot")

  expect_identical(
    method_plot$data,
    stability_plot$data
  )
})

test_that("elastic-net effects plot returns effect plot data", {
  data <- simulate_gaussian_data()

  fit <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    nfolds = 3,
    n_reps = 3,
    feature_filter = feature_filter_none(),
    seed = 702
  )

  plot <- plot_elasticnet_effects(
    fit,
    n_features = 5,
    min_percent = 0
  )

  plot_data <- plot$data

  expect_s3_class(plot, "ggplot")
  expect_true(is.data.frame(plot_data))
  expect_lte(nrow(plot_data), 5)
  expect_true("absolute_coefficient" %in% names(plot_data))
  expect_true(all(plot_data$absolute_coefficient >= 0))
  expect_true(all(plot_data$percent >= 0))
  expect_true(all(plot_data$percent <= 100))
})

test_that("elastic-net effects plot uses family-specific x-axis labels", {
  gaussian_data <- simulate_gaussian_data()

  gaussian_fit <- fit_elasticnet_gaussian(
    x = gaussian_data$x,
    sample_data = gaussian_data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    nfolds = 3,
    n_reps = 2,
    feature_filter = feature_filter_none(),
    seed = 703
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
    seed = 704
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
    seed = 705
  )

  expect_identical(
    plot_elasticnet_effects(gaussian_fit)$labels$x,
    "Absolute standardized coefficient (per 1-SD increase)"
  )

  expect_identical(
    plot_elasticnet_effects(binomial_fit)$labels$x,
    "Absolute standardized log odds ratio (per 1-SD increase)"
  )

  expect_identical(
    plot_elasticnet_effects(cox_fit)$labels$x,
    "Absolute standardized log hazard ratio (per 1-SD increase)"
  )
})

test_that("elastic-net stability plot classifies plotted features", {
  data <- simulate_gaussian_data()

  fit <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    nfolds = 3,
    n_reps = 3,
    feature_filter = feature_filter_none(),
    seed = 801
  )

  plot <- plot_elasticnet_stability(fit, n_features = 10)

  expect_true(
    all(
      c(
        "frequency_class",
        "effect_class",
        "signal_class",
        "point_label"
      ) %in%
        names(plot$data)
    )
  )

  expect_true(all(is.na(plot$data$point_label)))
})

test_that("elastic-net effects plot labels requested signal classes", {
  data <- simulate_gaussian_data()

  fit <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    nfolds = 3,
    n_reps = 5,
    feature_filter = feature_filter_none(),
    seed = 802
  )

  unlabelled_plot <- plot_elasticnet_effects(
    fit,
    n_features = Inf
  )

  available_signal <- as.character(
    unlabelled_plot$data$signal_class[[1L]]
  )

  labelled_plot <- plot_elasticnet_effects(
    fit,
    n_features = Inf,
    label_signal = available_signal
  )

  labeled <- labelled_plot$data[
    !is.na(labelled_plot$data$point_label),
    ,
    drop = FALSE
  ]

  expect_true(nrow(labeled) > 0L)

  expect_true(
    all(
      as.character(labeled$signal_class) == available_signal
    )
  )

  expect_identical(
    labeled$point_label,
    as.character(labeled$plot_label)
  )
})

test_that("elastic-net plots validate requested signal classes", {
  data <- simulate_gaussian_data()

  fit <- fit_elasticnet_gaussian(
    x = data$x,
    sample_data = data$sample_data,
    sample_id = sample_id,
    outcome = outcome,
    nfolds = 3,
    n_reps = 2,
    feature_filter = feature_filter_none(),
    seed = 803
  )

  expect_error(
    plot_elasticnet_stability(
      fit,
      label_signal = "Not_a_real_class"
    ),
    "unknown signal class"
  )

  expect_error(
    plot_elasticnet_effects(
      fit,
      label_signal = 1
    ),
    "must be `NULL` or a non-missing character vector"
  )
})
