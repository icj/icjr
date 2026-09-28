#' @importFrom rlang .data
NULL


prepare_elasticnet_plot_data <- function(
  x,
  n_features,
  min_percent,
  labels
) {
  if (!inherits(x, "icjr_elasticnet_fit")) {
    rlang::abort("`x` must be an `icjr_elasticnet_fit` object.")
  }

  if (
    length(n_features) != 1L ||
      !is.numeric(n_features) ||
      is.na(n_features) ||
      n_features < 1
  ) {
    rlang::abort("`n_features` must be one number greater than or equal to 1.")
  }

  if (
    length(min_percent) != 1L ||
      !is.numeric(min_percent) ||
      is.na(min_percent) ||
      min_percent < 0 ||
      min_percent > 100
  ) {
    rlang::abort("`min_percent` must be one number from 0 to 100.")
  }

  if (!is.logical(labels) || length(labels) != 1L || is.na(labels)) {
    rlang::abort("`labels` must be `TRUE` or `FALSE`.")
  }

  features <- selected_features(x)

  features <- features[
    features$percent >= min_percent,
    ,
    drop = FALSE
  ]

  if (nrow(features) == 0L) {
    rlang::abort(
      paste0(
        "No selected features meet `min_percent`; lower the threshold or inspect ",
        "`selected_features(x)`."
      )
    )
  }

  features <- features[
    order(
      -features$percent,
      -abs(features$median_coefficient),
      features$feature
    ),
    ,
    drop = FALSE
  ]

  features <- utils::head(features, n = n_features)

  features$plot_label <- features$feature

  if (
    labels &&
      "feature_label" %in% names(features) &&
      !all(is.na(features$feature_label))
  ) {
    use_label <- !is.na(features$feature_label) &
      nzchar(features$feature_label)

    features$plot_label[use_label] <- features$feature_label[use_label]
  }

  features$plot_label <- make.unique(features$plot_label)

  features$direction <- ifelse(
    features$median_coefficient > 0,
    "Positive",
    ifelse(
      features$median_coefficient < 0,
      "Negative",
      "Zero"
    )
  )

  features$absolute_coefficient <- abs(features$median_coefficient)

  features
}


validate_elasticnet_plot_labels <- function(
  title,
  subtitle,
  caption,
  tag,
  x_label,
  y_label
) {
  text_labels <- list(
    title = title,
    subtitle = subtitle,
    caption = caption,
    tag = tag,
    x_label = x_label,
    y_label = y_label
  )

  invalid_labels <- vapply(
    text_labels,
    function(value) {
      !is.null(value) &&
        (length(value) != 1L ||
          !is.character(value) ||
          is.na(value))
    },
    logical(1)
  )

  if (any(invalid_labels)) {
    rlang::abort(
      "Plot labels must be `NULL` or one non-missing character string."
    )
  }

  invisible(NULL)
}


elasticnet_effect_label <- function(x) {
  switch(
    x$specification$family,
    gaussian = "Median coefficient",
    binomial = "Median log odds ratio",
    cox = "Median log hazard ratio",
    "Median coefficient"
  )
}


add_elasticnet_plot_labels <- function(
  features,
  label_signal,
  frequency_cutoffs,
  effect_cutoffs
) {
  features <- classify_elasticnet_features(
    features = features,
    frequency_cutoffs = frequency_cutoffs,
    effect_cutoffs = effect_cutoffs
  )

  if (is.null(label_signal)) {
    features$point_label <- NA_character_
    return(features)
  }

  if (
    !is.character(label_signal) ||
      anyNA(label_signal) ||
      any(label_signal == "")
  ) {
    rlang::abort(
      "`label_signal` must be `NULL` or a non-missing character vector."
    )
  }

  available_signal <- levels(features$signal_class)
  unknown_signal <- setdiff(label_signal, available_signal)

  if (length(unknown_signal) > 0L) {
    rlang::abort(
      paste0(
        "`label_signal` contains unknown signal class(es): ",
        paste(unknown_signal, collapse = ", "),
        ". Available classes are: ",
        paste(available_signal, collapse = ", "),
        "."
      )
    )
  }

  features$point_label <- ifelse(
    as.character(features$signal_class) %in% label_signal,
    as.character(features$plot_label),
    NA_character_
  )

  features
}


#' Plot elastic-net feature selection stability
#'
#' Plots selection frequency for the most stable penalized features across
#' repeated elastic-net fits. Point color shows coefficient direction and point
#' size shows the absolute median penalized coefficient.
#'
#' @param x An `icjr_elasticnet_fit` object.
#' @param n_features Maximum number of selected features to display. Set to
#'   `Inf` to display all eligible features.
#' @param min_percent Minimum selection frequency required for display.
#' @param stability_threshold Optional selection-frequency reference line.
#'   Set to `NULL` to omit the line.
#' @param labels Use human-readable feature labels when available.
#' @param label_signal Optional character vector of `signal_class` values to
#'   label. For example, `c("High:Strong", "Medium:Strong")`. Use `NULL` to
#'   draw no feature labels.
#' @param frequency_cutoffs Named numeric vector of inclusive lower bounds for
#'   selection-frequency categories used to classify and optionally label
#'   features.
#' @param effect_cutoffs Named numeric vector of inclusive lower bounds for
#'   absolute median-coefficient categories used to classify and optionally
#'   label features.
#' @param title Plot title.
#' @param subtitle Plot subtitle. If `NULL`, a subtitle describing the number
#'   of displayed features is used.
#' @param caption Plot caption.
#' @param tag Plot tag.
#' @param x_label X-axis label.
#' @param y_label Y-axis label.
#'
#' @return A `ggplot` object. Its `data` element contains the plotted features
#'   plus `frequency_class`, `effect_class`, `signal_class`, and `point_label`.
#' @export
plot_elasticnet_stability <- function(
  x,
  n_features = 20,
  min_percent = 0,
  stability_threshold = 50,
  labels = TRUE,
  label_signal = NULL,
  frequency_cutoffs = c(
    Rare = 0,
    Low = 20,
    Medium = 50,
    High = 80
  ),
  effect_cutoffs = c(
    Negligible = 0,
    Weak = 0.05,
    Moderate = 0.10,
    Strong = 0.30
  ),
  title = "Elastic-net feature selection stability",
  subtitle = NULL,
  caption = NULL,
  tag = NULL,
  x_label = "Selection frequency (%)",
  y_label = NULL
) {
  if (
    !is.null(stability_threshold) &&
      (length(stability_threshold) != 1L ||
        !is.numeric(stability_threshold) ||
        is.na(stability_threshold) ||
        stability_threshold < 0 ||
        stability_threshold > 100)
  ) {
    rlang::abort(
      "`stability_threshold` must be `NULL` or one number from 0 to 100."
    )
  }

  validate_elasticnet_plot_labels(
    title = title,
    subtitle = subtitle,
    caption = caption,
    tag = tag,
    x_label = x_label,
    y_label = y_label
  )

  features <- prepare_elasticnet_plot_data(
    x = x,
    n_features = n_features,
    min_percent = min_percent,
    labels = labels
  )

  features <- add_elasticnet_plot_labels(
    features = features,
    label_signal = label_signal,
    frequency_cutoffs = frequency_cutoffs,
    effect_cutoffs = effect_cutoffs
  )

  features$plot_label <- stats::reorder(
    features$plot_label,
    features$percent
  )

  effect_label <- elasticnet_effect_label(x)

  if (is.null(subtitle)) {
    subtitle <- paste0(
      "Top ",
      nrow(features),
      " selected features across repeated fits"
    )
  }

  threshold_layer <- if (is.null(stability_threshold)) {
    NULL
  } else {
    ggplot2::geom_vline(
      xintercept = stability_threshold,
      linetype = "dashed",
      color = "grey35"
    )
  }

  label_layer <- if (all(is.na(features$point_label))) {
    NULL
  } else {
    ggplot2::geom_text(
      data = features[!is.na(features$point_label), , drop = FALSE],
      ggplot2::aes(
        label = .data$point_label
      ),
      hjust = -0.1,
      show.legend = FALSE,
      inherit.aes = TRUE
    )
  }

  plot <- ggplot2::ggplot(
    features,
    ggplot2::aes(
      x = .data$percent,
      y = .data$plot_label,
      color = .data$direction,
      size = .data$absolute_coefficient
    )
  ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = 0,
        xend = .data$percent,
        y = .data$plot_label,
        yend = .data$plot_label
      ),
      color = "grey75",
      linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    threshold_layer +
    ggplot2::geom_point() +
    label_layer +
    ggplot2::scale_color_manual(
      values = c(
        "Positive" = "#2C7FB8",
        "Negative" = "#D95F0E",
        "Zero" = "#666666"
      ),
      name = "Coefficient direction"
    ) +
    ggplot2::scale_size_continuous(
      name = paste0("Absolute ", tolower(effect_label)),
      range = c(2.5, 7)
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 100),
      expand = ggplot2::expansion(mult = c(0, 0.15))
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      tag = tag,
      x = x_label,
      y = y_label
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.box = "vertical",
      plot.margin = ggplot2::margin(r = 35)
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        order = 1,
        override.aes = list(size = 5)
      ),
      size = ggplot2::guide_legend(order = 2)
    )

  plot
}


#' Plot elastic-net feature effect magnitudes
#'
#' Plots absolute median penalized coefficient magnitude for the most stable
#' selected features. Point color shows coefficient direction and point size
#' shows selection frequency across repeated fits.
#'
#' @param x An `icjr_elasticnet_fit` object.
#' @param n_features Maximum number of selected features to display. Set to
#'   `Inf` to display all eligible features.
#' @param min_percent Minimum selection frequency required for display.
#' @param labels Use human-readable feature labels when available.
#' @param label_signal Optional character vector of `signal_class` values to
#'   label. For example, `c("High:Strong", "Medium:Strong")`. Use `NULL` to
#'   draw no feature labels.
#' @param frequency_cutoffs Named numeric vector of inclusive lower bounds for
#'   selection-frequency categories used to classify and optionally label
#'   features.
#' @param effect_cutoffs Named numeric vector of inclusive lower bounds for
#'   absolute median-coefficient categories used to classify and optionally
#'   label features.
#' @param title Plot title.
#' @param subtitle Plot subtitle. If `NULL`, a subtitle describing the number
#'   of displayed features is used.
#' @param caption Plot caption.
#' @param tag Plot tag.
#' @param x_label X-axis label. If `NULL`, a family-specific coefficient-scale
#'   label is used.
#' @param y_label Y-axis label.
#'
#' @return A `ggplot` object. Its `data` element contains the plotted features
#'   plus `frequency_class`, `effect_class`, `signal_class`, and `point_label`.
#' @export
plot_elasticnet_effects <- function(
  x,
  n_features = 20,
  min_percent = 0,
  labels = TRUE,
  label_signal = NULL,
  frequency_cutoffs = c(
    Rare = 0,
    Low = 20,
    Medium = 50,
    High = 80
  ),
  effect_cutoffs = c(
    Negligible = 0,
    Weak = 0.05,
    Moderate = 0.10,
    Strong = 0.30
  ),
  title = "Elastic-net feature effect magnitudes",
  subtitle = NULL,
  caption = NULL,
  tag = NULL,
  x_label = NULL,
  y_label = NULL
) {
  validate_elasticnet_plot_labels(
    title = title,
    subtitle = subtitle,
    caption = caption,
    tag = tag,
    x_label = x_label,
    y_label = y_label
  )

  features <- prepare_elasticnet_plot_data(
    x = x,
    n_features = n_features,
    min_percent = min_percent,
    labels = labels
  )

  features <- add_elasticnet_plot_labels(
    features = features,
    label_signal = label_signal,
    frequency_cutoffs = frequency_cutoffs,
    effect_cutoffs = effect_cutoffs
  )

  features <- features[
    order(
      -features$absolute_coefficient,
      -features$percent,
      features$feature
    ),
    ,
    drop = FALSE
  ]

  features$plot_label <- stats::reorder(
    features$plot_label,
    features$absolute_coefficient
  )

  effect_label <- elasticnet_effect_label(x)

  if (is.null(subtitle)) {
    subtitle <- paste0(
      "Top ",
      nrow(features),
      " selected features across repeated fits"
    )
  }

  if (is.null(x_label)) {
    x_label <- paste0("Absolute ", tolower(effect_label))
  }

  label_layer <- if (all(is.na(features$point_label))) {
    NULL
  } else {
    ggplot2::geom_text(
      data = features[!is.na(features$point_label), , drop = FALSE],
      ggplot2::aes(
        label = .data$point_label
      ),
      hjust = -0.1,
      show.legend = FALSE,
      inherit.aes = TRUE
    )
  }

  plot <- ggplot2::ggplot(
    features,
    ggplot2::aes(
      x = .data$absolute_coefficient,
      y = .data$plot_label,
      color = .data$direction,
      size = .data$percent
    )
  ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = 0,
        xend = .data$absolute_coefficient,
        y = .data$plot_label,
        yend = .data$plot_label
      ),
      color = "grey75",
      linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point() +
    label_layer +
    ggplot2::scale_color_manual(
      values = c(
        "Positive" = "#2C7FB8",
        "Negative" = "#D95F0E",
        "Zero" = "#666666"
      ),
      name = "Coefficient direction"
    ) +
    ggplot2::scale_size_continuous(
      name = "Selection frequency (%)",
      range = c(2.5, 7)
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.15))
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      tag = tag,
      x = x_label,
      y = y_label
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.box = "vertical",
      plot.margin = ggplot2::margin(r = 35)
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        order = 1,
        override.aes = list(size = 5)
      ),
      size = ggplot2::guide_legend(order = 2)
    )

  plot
}
