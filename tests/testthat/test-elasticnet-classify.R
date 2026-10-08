test_that("elastic-net feature classification assigns default categories", {
  features <- data.frame(
    feature = c("feature_1", "feature_2", "feature_3", "feature_4"),
    percent = c(0, 20, 50, 80),
    median_standardized_coefficient = c(0, 0.05, -0.10, 0.30)
  )

  classified <- classify_elasticnet_features(features)

  expect_s3_class(classified, "data.frame")

  expect_identical(
    as.character(classified$frequency_class),
    c("Rare", "Low", "Medium", "High")
  )

  expect_identical(
    as.character(classified$effect_class),
    c("Negligible", "Weak", "Moderate", "Strong")
  )

  expect_identical(
    as.character(classified$signal_class),
    c(
      "Rare:Negligible",
      "Low:Weak",
      "Medium:Moderate",
      "High:Strong"
    )
  )
})

test_that("elastic-net feature classification uses absolute coefficient size", {
  features <- data.frame(
    percent = c(80, 80, 80),
    median_standardized_coefficient = c(-0.30, -0.10, -0.05)
  )

  classified <- classify_elasticnet_features(features)

  expect_identical(
    as.character(classified$effect_class),
    c("Strong", "Moderate", "Weak")
  )
})

test_that("elastic-net feature classification respects inclusive boundaries", {
  features <- data.frame(
    percent = c(
      0,
      19.999,
      20,
      49.999,
      50,
      79.999,
      80,
      100
    ),
    median_standardized_coefficient = c(
      0,
      0.049,
      0.05,
      0.099,
      0.10,
      0.299,
      0.30,
      1
    )
  )

  classified <- classify_elasticnet_features(features)

  expect_identical(
    as.character(classified$frequency_class),
    c(
      "Rare",
      "Rare",
      "Low",
      "Low",
      "Medium",
      "Medium",
      "High",
      "High"
    )
  )

  expect_identical(
    as.character(classified$effect_class),
    c(
      "Negligible",
      "Negligible",
      "Weak",
      "Weak",
      "Moderate",
      "Moderate",
      "Strong",
      "Strong"
    )
  )
})

test_that("elastic-net feature classification supports custom cutoffs", {
  features <- data.frame(
    percent = c(0, 25, 75),
    median_standardized_coefficient = c(0.01, 0.10, -0.50)
  )

  classified <- classify_elasticnet_features(
    features = features,
    frequency_cutoffs = c(
      Infrequent = 0,
      Frequent = 50
    ),
    effect_cutoffs = c(
      Small = 0,
      Large = 0.10
    )
  )

  expect_identical(
    as.character(classified$frequency_class),
    c("Infrequent", "Infrequent", "Frequent")
  )

  expect_identical(
    as.character(classified$effect_class),
    c("Small", "Large", "Large")
  )

  expect_identical(
    levels(classified$frequency_class),
    c("Infrequent", "Frequent")
  )

  expect_identical(
    levels(classified$effect_class),
    c("Small", "Large")
  )
})

test_that("elastic-net feature classification retains input columns and ordering", {
  features <- data.frame(
    feature = c("feature_b", "feature_a"),
    feature_label = c("Feature B", "Feature A"),
    percent = c(50, 20),
    median_standardized_coefficient = c(0.10, -0.05),
    stringsAsFactors = FALSE
  )

  classified <- classify_elasticnet_features(features)

  expect_identical(
    names(classified),
    c(
      "feature",
      "feature_label",
      "percent",
      "median_standardized_coefficient",
      "frequency_class",
      "effect_class",
      "signal_class"
    )
  )

  expect_identical(classified$feature, features$feature)
  expect_identical(classified$feature_label, features$feature_label)
})

test_that("elastic-net feature classification handles no selected features", {
  features <- data.frame(
    percent = numeric(),
    median_standardized_coefficient = numeric()
  )

  classified <- classify_elasticnet_features(features)

  expect_equal(nrow(classified), 0)
  expect_true(is.ordered(classified$frequency_class))
  expect_true(is.ordered(classified$effect_class))
  expect_equal(
    levels(classified$frequency_class),
    c("Rare", "Low", "Medium", "High")
  )
  expect_equal(
    levels(classified$effect_class),
    c("Negligible", "Weak", "Moderate", "Strong")
  )
})

test_that("elastic-net feature classification validates feature input", {
  expect_error(
    classify_elasticnet_features(list()),
    "`features` must be a data frame"
  )

  expect_error(
    classify_elasticnet_features(
      data.frame(percent = 50)
    ),
    "must contain these columns"
  )

  expect_error(
    classify_elasticnet_features(
      data.frame(median_standardized_coefficient = 0.1)
    ),
    "must contain these columns"
  )

  expect_error(
    classify_elasticnet_features(
      data.frame(
        percent = "50",
        median_standardized_coefficient = 0.1
      )
    ),
    "must both be numeric"
  )

  expect_error(
    classify_elasticnet_features(
      data.frame(
        percent = 101,
        median_standardized_coefficient = 0.1
      )
    ),
    "0 to 100"
  )

  expect_error(
    classify_elasticnet_features(
      data.frame(
        percent = NA_real_,
        median_standardized_coefficient = 0.1
      )
    ),
    "finite, non-missing"
  )

  expect_error(
    classify_elasticnet_features(
      data.frame(
        percent = 50,
        median_standardized_coefficient = Inf
      )
    ),
    "finite, non-missing"
  )
})

test_that("elastic-net feature classification validates cutoff vectors", {
  features <- data.frame(
    percent = 50,
    median_standardized_coefficient = 0.1
  )

  expect_error(
    classify_elasticnet_features(
      features,
      frequency_cutoffs = c(0, 50)
    ),
    "named numeric vector"
  )

  expect_error(
    classify_elasticnet_features(
      features,
      frequency_cutoffs = c(Low = 10, High = 50)
    ),
    "must begin at 0"
  )

  expect_error(
    classify_elasticnet_features(
      features,
      frequency_cutoffs = c(Low = 0, High = 0)
    ),
    "strictly increasing"
  )

  expect_error(
    classify_elasticnet_features(
      features,
      effect_cutoffs = c(None = 0, Small = NA_real_)
    ),
    "named numeric vector"
  )
})

test_that("elastic-net feature signal classes retain all category levels", {
  features <- data.frame(
    percent = 100,
    median_standardized_coefficient = 1
  )

  classified <- classify_elasticnet_features(features)

  expect_identical(
    levels(classified$signal_class),
    c(
      "Rare:Negligible",
      "Low:Negligible",
      "Medium:Negligible",
      "High:Negligible",
      "Rare:Weak",
      "Low:Weak",
      "Medium:Weak",
      "High:Weak",
      "Rare:Moderate",
      "Low:Moderate",
      "Medium:Moderate",
      "High:Moderate",
      "Rare:Strong",
      "Low:Strong",
      "Medium:Strong",
      "High:Strong"
    )
  )
})
