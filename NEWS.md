# icjr 0.0.0.9000

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
- Improved elastic-net input handling for missing metadata and sample-ID
  alignment.
  