simulate_gaussian_data <- function(n_samples = 40, n_features = 20, seed = 1) {
  simulate_elasticnet_data("gaussian", n_samples, n_features, seed)
}

simulate_binomial_data <- function(n_samples = 80, n_features = 20, seed = 2) {
  simulate_elasticnet_data("binomial", n_samples, n_features, seed)
}

simulate_cox_data <- function(n_samples = 100, n_features = 20, seed = 3) {
  simulate_elasticnet_data("cox", n_samples, n_features, seed)
}
