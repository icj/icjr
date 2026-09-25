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
