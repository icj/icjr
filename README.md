
<!-- README.md is generated from README.Rmd. Please edit that file. -->

# icjr

`icjr` contains reusable R helpers for high-dimensional elastic-net
modeling and related analysis workflows.

The elastic-net helpers support repeated cross-validated models for:

- Continuous outcomes with Gaussian elastic-net regression.
- Binary outcomes with binomial elastic-net regression.
- Time-to-event outcomes with Cox elastic-net regression.

The package is designed for analyses with a numeric feature matrix—such
as gene expression, pathway scores, protein abundance, or other
molecular features—and a sample-level data frame containing outcomes and
covariates.

## Installation

Install the development version from GitHub:

``` r
# install.packages("pak")
pak::pak("icj/icjr")
```

Then load the package:

``` r
library(icjr)
```

## Repeated elastic-net models

### Data conventions

All three elastic-net fitting functions use the same core data
conventions:

- `x` is a numeric matrix with **samples in rows** and **features in
  columns**.
- `rownames(x)` must contain unique sample identifiers.
- `sample_data` is a data frame with one row per candidate sample.
- `sample_id` is an unquoted column name in `sample_data` whose values
  match `rownames(x)`.
- Outcome variables and covariates are columns in `sample_data`.
- Matrix features are penalized and eligible for elastic-net selection.
- Formula covariates are unpenalized.
- Repeated fits rerun cross-validation. A feature’s selection percentage
  is the proportion of successful repetitions in which it has a nonzero
  penalized coefficient.

The fitting functions align the feature matrix and sample data by
identifier, remove samples with model-relevant missing data, and retain
the corresponding modeled feature set in the returned fit object.

### Example data

This example creates a small synthetic binary-outcome data set with two
unpenalized covariates and 100 penalized molecular features.

``` r
set.seed(20260928)

n_samples <- 120
n_features <- 100

sample_ids <- paste0("sample_", seq_len(n_samples))
feature_ids <- paste0("feature_", seq_len(n_features))

x <- matrix(
  rnorm(n_samples * n_features),
  nrow = n_samples,
  ncol = n_features,
  dimnames = list(sample_ids, feature_ids)
)

sample_data <- data.frame(
  sample_id = sample_ids,
  age = round(rnorm(n_samples, mean = 55, sd = 10)),
  sex = sample(c("Female", "Male"), n_samples, replace = TRUE),
  stringsAsFactors = FALSE
)

linear_predictor <- (
  0.75 * x[, "feature_1"] -
    0.60 * x[, "feature_2"] +
    0.45 * x[, "feature_3"] +
    0.03 * sample_data$age +
    ifelse(sample_data$sex == "Male", 0.25, 0)
)

sample_data$outcome <- factor(
  rbinom(
    n = n_samples,
    size = 1,
    prob = plogis(linear_predictor)
  ),
  levels = c(0, 1),
  labels = c("control", "case")
)
```

### Fit a binomial model

Use `fit_elasticnet_binomial()` for a two-level outcome. Set
`event_level` to the level that represents the event, case, or positive
class.

``` r
fit <- fit_elasticnet_binomial(
  x = x,
  sample_data = sample_data,
  sample_id = sample_id,
  outcome = outcome,
  event_level = "case",
  covariates = ~ age + sex,
  alpha = 0.5,
  nfolds = 3,
  n_reps = 25,
  feature_filter = feature_filter_none(),
  seed = 20260928
)
```

The example uses a modest number of repetitions so it renders quickly.
In a real analysis, use enough repetitions to assess the stability of
feature selection and choose `nfolds` that is appropriate for the
available outcome information.

### Inspect a fitted model

A fitted object has compact print and summary methods:

``` r
fit

summary(fit)
```

Use extractors for programmatic downstream work:

``` r
model_summary(fit)

modeled_features(fit)

repetition_summary(fit)

covariate_coefficients(fit)
```

`selected_features()` returns penalized molecular features selected in
at least one successful fit.

``` r
features <- selected_features(fit)

head(features)
```

Important columns include:

- `feature`: stable feature identifier.
- `feature_label`: optional human-readable label when supplied by the
  analysis.
- `n`: number of successful repeated fits that selected the feature.
- `percent`: percentage of successful repeated fits selecting the
  feature.
- `median_coefficient`: median nonzero penalized coefficient across
  repeated fits in which the feature was selected.
- `median_effect`: a family-specific effect-scale summary.
- `sign_consistency`: consistency of coefficient direction across
  selected fits.

For binomial models, the coefficient scale is the log odds-ratio scale.
For Cox models, it is the log hazard-ratio scale. The `median_effect`
column provides the corresponding exponentiated odds-ratio or
hazard-ratio scale where appropriate.

### Classify feature signals

`classify_elasticnet_features()` adds interpretable categories for
selection frequency and absolute median coefficient magnitude.

``` r
classified_features <- features |>
  classify_elasticnet_features()

classified_features[, c(
  "feature",
  "percent",
  "median_coefficient",
  "frequency_class",
  "effect_class",
  "signal_class"
)]
```

The defaults classify features as:

| Measure                        | Categories                                 |
|:-------------------------------|:-------------------------------------------|
| Selection frequency            | `Rare`, `Low`, `Medium`, `High`            |
| Absolute coefficient magnitude | `Negligible`, `Weak`, `Moderate`, `Strong` |

The combined `signal_class` values include labels such as
`"High:Strong"` and `"Medium:Moderate"`.

To focus on repeatedly selected features with moderate or strong
coefficients:

``` r
classified_features |>
  dplyr::filter(
    frequency_class %in% c("Medium", "High"),
    effect_class %in% c("Moderate", "Strong")
  ) |>
  dplyr::arrange(
    dplyr::desc(percent),
    dplyr::desc(abs(median_coefficient))
  )
```

The default category cutoffs can be changed:

``` r
classify_elasticnet_features(
  features,
  frequency_cutoffs = c(
    Infrequent = 0,
    Recurrent = 25,
    Stable = 50,
    Dominant = 80
  ),
  effect_cutoffs = c(
    Small = 0,
    Moderate = 0.10,
    Large = 0.30
  )
)
```

### Plot selection stability

The default `plot()` method produces a stability-first view:

``` r
plot(
  fit,
  n_features = 20,
  min_percent = 10,
  stability_threshold = 50
)
```

The equivalent explicit function is:

``` r
plot_elasticnet_stability(
  fit,
  n_features = 20,
  min_percent = 10,
  stability_threshold = 50,
  title = "Elastic-net feature selection stability",
  caption = "Dashed line marks the 50% selection-frequency threshold."
)
```

In the stability plot:

- The x-axis is selection frequency.
- Point size is absolute median coefficient magnitude.
- Point color is coefficient direction.
- The dashed line marks the requested stability threshold.

You can annotate only selected frequency/effect categories:

``` r
plot_elasticnet_stability(
  fit,
  n_features = 20,
  min_percent = 10,
  stability_threshold = 50,
  label_signal = c(
    "High:Strong",
    "Medium:Strong"
  ),
  title = "Stable elastic-net features"
)
```

### Plot effect magnitude

`plot_elasticnet_effects()` emphasizes effect magnitude while retaining
selection stability as the point-size encoding.

``` r
plot_elasticnet_effects(
  fit,
  n_features = 20,
  min_percent = 10,
  label_signal = "High:Strong",
  title = "Elastic-net effect magnitude",
  subtitle = "Point size represents selection frequency"
)
```

In the effect-magnitude plot:

- The x-axis is absolute median penalized coefficient.
- Point size is selection frequency.
- Point color is coefficient direction.

Both plotting functions return `ggplot` objects, so they can be
customized with ordinary `ggplot2` layers:

``` r
plot_elasticnet_stability(
  fit,
  n_features = 15,
  title = "Stable predictors"
) +
  ggplot2::theme(
    legend.position = "right",
    plot.title = ggplot2::element_text(face = "bold")
  )
```

The prepared plotting data are available through the returned plot
object’s `data` component:

``` r
stability_plot <- plot_elasticnet_stability(fit)

stability_plot$data
```

### Other outcome families

Use `fit_elasticnet_gaussian()` for continuous outcomes:

``` r
gaussian_fit <- fit_elasticnet_gaussian(
  x = x,
  sample_data = sample_data,
  sample_id = sample_id,
  outcome = age,
  covariates = ~ sex,
  alpha = 0.5,
  nfolds = 3,
  n_reps = 25,
  feature_filter = feature_filter_none(),
  seed = 20260929
)
```

Use `fit_elasticnet_cox()` for time-to-event outcomes:

``` r
set.seed(20260930)

sample_data$followup_time <- rexp(n_samples, rate = 0.10)
sample_data$event_status <- rbinom(n_samples, size = 1, prob = 0.60)

cox_fit <- fit_elasticnet_cox(
  x = x,
  sample_data = sample_data,
  sample_id = sample_id,
  time = followup_time,
  status = event_status,
  covariates = ~ age + sex,
  alpha = 0.5,
  nfolds = 3,
  n_reps = 25,
  feature_filter = feature_filter_none(),
  seed = 20260930
)
```

The same extractors, classifier, and plotting functions work across
Gaussian, binomial, and Cox elastic-net fits:

``` r
summary(cox_fit)

selected_features(cox_fit) |>
  classify_elasticnet_features()

plot_elasticnet_stability(cox_fit)

plot_elasticnet_effects(cox_fit)
```

## Feature filtering

Feature filters are supplied through the `feature_filter` argument.

Keep all eligible features:

``` r
feature_filter_none()
```

Retain features according to median absolute deviation:

``` r
feature_filter_mad()
```

Retain features according to variance:

``` r
feature_filter_variance()
```

Retain features above a specified median-absolute-deviation threshold:

``` r
feature_filter_mad_threshold(0.10)
```

Choose filtering before fitting and record it with the model
specification. Filtering should be selected based on the scientific
setting and should not be repeatedly tuned against a downstream outcome
without appropriate validation.

## Interpretation

Selection frequency and coefficient magnitude answer different
questions:

- A high selection frequency suggests that a feature recurs across
  repeated cross-validation fits under the specified modeling choices.
- A large absolute median coefficient suggests a stronger penalized
  association on the model’s coefficient scale among fits that selected
  that feature.
- A feature with a large coefficient but low selection frequency may be
  unstable.
- A feature with a high selection frequency but modest coefficient may
  be a reproducible, small association.

These summaries are exploratory model-selection outputs. They do not by
themselves provide valid post-selection p-values, confidence intervals,
or causal-effect estimates. Use independent validation, resampling,
appropriate inference methods, and domain knowledge before treating
selected features as confirmed findings.

## Development

For development from a local clone:

``` r
devtools::load_all()
devtools::test()
devtools::check()
```

Regenerate package documentation after editing roxygen comments:

``` r
devtools::document()
```
