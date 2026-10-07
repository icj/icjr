#' Simulate example data for elastic-net models
#'
#' Generates a small feature matrix and matching sample table for use in
#' examples, tests, and vignettes. `feature_1` and `feature_2` carry true
#' signal; all other features are noise. The caller's random-number state is
#' restored on exit.
#'
#' @param type Outcome type: `"gaussian"`, `"binomial"`, or `"cox"`.
#' @param n_samples Number of samples. Defaults depend on `type` (40, 80, 100).
#' @param n_features Number of candidate features.
#' @param seed Integer seed. Defaults depend on `type` (1, 2, 3).
#'
#' @return A list with:
#'   - `x`: numeric matrix, samples in rows and features in columns.
#'   - `sample_data`: tibble with `sample_id`, `age`, `sex`, and the outcome
#'     column(s): `outcome` (gaussian, binomial) or `followup_time` and
#'     `event_status` (cox).
#' @export
#' @examples
#' dat <- simulate_elasticnet_data("gaussian")
#' dim(dat$x)
#' head(dat$sample_data)
simulate_elasticnet_data <- function(
  type = c("gaussian", "binomial", "cox"),
  n_samples = NULL,
  n_features = 20,
  seed = NULL
) {
  type <- match.arg(type)

  defaults <- list(
    gaussian = list(n = 40, seed = 1),
    binomial = list(n = 80, seed = 2),
    cox = list(n = 100, seed = 3)
  )[[type]]

  n_samples <- rlang::`%||%`(n_samples, defaults$n)
  seed <- rlang::`%||%`(seed, defaults$seed)

  restore_random_seed()
  set.seed(seed)

  x <- matrix(
    stats::rnorm(n_samples * n_features),
    nrow = n_samples,
    ncol = n_features
  )
  rownames(x) <- paste0("sample_", seq_len(n_samples))
  colnames(x) <- paste0("feature_", seq_len(n_features))

  sample_data <- switch(
    type,
    gaussian = sim_gaussian_samples(x),
    binomial = sim_binomial_samples(x),
    cox = sim_cox_samples(x)
  )

  list(x = x, sample_data = sample_data)
}

# Restore the caller's RNG state when the calling function exits.
restore_random_seed <- function(env = parent.frame()) {
  global <- globalenv()
  had_seed <- exists(".Random.seed", envir = global, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = global) else NULL

  do.call(
    on.exit,
    list(
      bquote({
        if (is.null(.(old_seed))) {
          if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
            rm(".Random.seed", envir = globalenv())
          }
        } else {
          assign(".Random.seed", .(old_seed), envir = globalenv())
        }
      }),
      add = TRUE
    ),
    envir = env
  )
  invisible(NULL)
}

sim_gaussian_samples <- function(x) {
  n_samples <- nrow(x)

  tibble::tibble(
    sample_id = rownames(x),
    age = stats::rnorm(n_samples, mean = 50, sd = 10),
    sex = rep(c("female", "male"), length.out = n_samples)
  ) |>
    dplyr::mutate(
      outcome = 1.5 *
        x[, "feature_1"] -
        1.0 * x[, "feature_2"] +
        0.05 * .data$age +
        stats::rnorm(n_samples, sd = 1)
    )
}

sim_binomial_samples <- function(x) {
  n_samples <- nrow(x)

  tibble::tibble(
    sample_id = rownames(x),
    age = stats::rnorm(n_samples, mean = 55, sd = 10),
    sex = factor(rep(c("female", "male"), length.out = n_samples))
  ) |>
    dplyr::mutate(
      linear_predictor = -0.35 +
        1.25 * x[, "feature_1"] -
        1.00 * x[, "feature_2"] +
        0.03 * .data$age +
        dplyr::if_else(.data$sex == "male", 0.35, 0),
      probability = stats::plogis(.data$linear_predictor),
      outcome = factor(
        dplyr::if_else(
          stats::runif(n_samples) < .data$probability,
          "case",
          "control"
        ),
        levels = c("control", "case")
      )
    ) |>
    dplyr::select(-"linear_predictor", -"probability")
}

sim_cox_samples <- function(x) {
  n_samples <- nrow(x)

  tibble::tibble(
    sample_id = rownames(x),
    age = stats::rnorm(n_samples, mean = 55, sd = 10),
    sex = factor(rep(c("female", "male"), length.out = n_samples))
  ) |>
    dplyr::mutate(
      linear_predictor = 0.70 *
        x[, "feature_1"] -
        0.60 * x[, "feature_2"] +
        0.02 * .data$age +
        dplyr::if_else(.data$sex == "male", 0.25, 0),
      event_time = stats::rexp(
        n_samples,
        rate = 0.08 * exp(.data$linear_predictor)
      ),
      censor_time = stats::rexp(n_samples, rate = 0.04),
      followup_time = pmin(.data$event_time, .data$censor_time),
      event_status = as.integer(.data$event_time <= .data$censor_time)
    ) |>
    dplyr::select(-"linear_predictor", -"event_time", -"censor_time")
}
