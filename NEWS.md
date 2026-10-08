# icjr 0.0.0.9000

- Added `feature_standard_deviations()` to retrieve the per-feature standard
  deviations used for 1-SD elastic-net effect reporting.
- Improved elastic-net stability and effect-magnitude plots to use standardized
  per-1-SD feature effects for ranking, direction, and magnitude displays.
  Plot labels now state the standardized effect scale explicitly.
- Improved elastic-net feature classification and `notable_features()` to use
  median standardized coefficients for effect-magnitude thresholds and
  reporting.
- Added repeated Gaussian elastic-net modeling with configurable feature filtering,
  unpenalized adjustment covariates, repeated cross-validation, and
  feature-selection stability summaries.
- Added repeated binomial elastic-net modeling with explicit event-level coding,
  deviance-based cross-validation, and odds-ratio feature summaries.
- Added repeated Cox elastic-net modeling with time-to-event validation,
  event-aware cross-validation, and hazard-ratio feature summaries.
- Added user-facing extractors for elastic-net fit objects.
- Added summary methods for elastic-net fit objects.
- Added stability and effect-magnitude plotting helpers for elastic-net fits.
- Added helpers for classifying elastic-net features by selection frequency and effect magnitude.
- Added `notable_features()` to create compact reporting tables of features
  meeting selection-frequency and effect-magnitude thresholds. Output includes
  median, minimum, and maximum penalized coefficients across selected repeated
  fits.
- Added optional parallel repeated fitting via `workers` to
  `fit_elasticnet_gaussian()`, `fit_elasticnet_binomial()`, and
  `fit_elasticnet_cox()`. Results are reproducible and invariant to
  `workers` when inputs and `seed` are unchanged.
- Improved elastic-net input handling for missing metadata and sample-ID
  alignment.
- Improved elastic-net column argument handling: `sample_id`, outcomes, and
  Cox `time` and `status` arguments can now be supplied through variables
  containing column names.
