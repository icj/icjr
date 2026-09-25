simulate_gaussian_data <- function(
  n_samples = 40,
  n_features = 20,
  seed = 1
) {
  set.seed(seed)

  x <- matrix(
    stats::rnorm(n_samples * n_features),
    nrow = n_samples,
    ncol = n_features
  )

  rownames(x) <- paste0("sample_", seq_len(n_samples))
  colnames(x) <- paste0("feature_", seq_len(n_features))

  sample_data <- tibble::tibble(
    sample_id = rownames(x),
    age = stats::rnorm(n_samples, mean = 50, sd = 10),
    sex = rep(c("female", "male"), length.out = n_samples)
  ) |>
    dplyr::mutate(
      outcome = 1.5 *
        x[, "feature_1"] -
        1.0 * x[, "feature_2"] +
        0.05 * age +
        stats::rnorm(n_samples, sd = 1)
    )

  list(x = x, sample_data = sample_data)
}

simulate_binomial_data <- function(
  n_samples = 80,
  n_features = 20,
  seed = 2
) {
  set.seed(seed)

  x <- matrix(
    stats::rnorm(n_samples * n_features),
    nrow = n_samples,
    ncol = n_features
  )

  rownames(x) <- paste0("sample_", seq_len(n_samples))
  colnames(x) <- paste0("feature_", seq_len(n_features))

  sample_data <- tibble::tibble(
    sample_id = rownames(x),
    age = stats::rnorm(n_samples, mean = 55, sd = 10),
    sex = factor(rep(c("female", "male"), length.out = n_samples))
  ) |>
    dplyr::mutate(
      linear_predictor = -0.35 +
        1.25 * x[, "feature_1"] -
        1.00 * x[, "feature_2"] +
        0.03 * age +
        dplyr::if_else(sex == "male", 0.35, 0),
      probability = stats::plogis(linear_predictor),
      outcome = factor(
        dplyr::if_else(
          stats::runif(n_samples) < probability,
          "case",
          "control"
        ),
        levels = c("control", "case")
      )
    ) |>
    dplyr::select(-linear_predictor, -probability)

  list(x = x, sample_data = sample_data)
}

simulate_cox_data <- function(
  n_samples = 100,
  n_features = 20,
  seed = 3
) {
  set.seed(seed)

  x <- matrix(
    stats::rnorm(n_samples * n_features),
    nrow = n_samples,
    ncol = n_features
  )

  rownames(x) <- paste0("sample_", seq_len(n_samples))
  colnames(x) <- paste0("feature_", seq_len(n_features))

  sample_data <- tibble::tibble(
    sample_id = rownames(x),
    age = stats::rnorm(n_samples, mean = 55, sd = 10),
    sex = factor(rep(c("female", "male"), length.out = n_samples))
  ) |>
    dplyr::mutate(
      linear_predictor = 0.70 *
        x[, "feature_1"] -
        0.60 * x[, "feature_2"] +
        0.02 * age +
        dplyr::if_else(sex == "male", 0.25, 0),
      event_time = stats::rexp(
        n_samples,
        rate = 0.08 * exp(linear_predictor)
      ),
      censor_time = stats::rexp(n_samples, rate = 0.04),
      followup_time = pmin(event_time, censor_time),
      event_status = as.integer(event_time <= censor_time)
    ) |>
    dplyr::select(
      -linear_predictor,
      -event_time,
      -censor_time
    )

  list(x = x, sample_data = sample_data)
}
