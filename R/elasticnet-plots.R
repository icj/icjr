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
      "No selected features meet `min_percent`; lower the threshold or inspect ",
      "`selected_features(x)`."
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
#' @param title Plot title.
#' @param subtitle Plot subtitle. If `NULL`, a subtitle describing the number
#'   of displayed features is used.
#' @param caption Plot caption.
#' @param tag Plot tag.
#' @param x_label X-axis label.
#' @param y_label Y-axis label.
#'
#' @return A `ggplot` object.
#' @export
plot_elasticnet_stability <- function(
  x,
  n_features = 20,
  min_percent = 0,
  stability_threshold = 50,
  labels = TRUE,
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
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
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
      legend.box = "vertical"
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
#' @param title Plot title.
#' @param subtitle Plot subtitle. If `NULL`, a subtitle describing the number
#'   of displayed features is used.
#' @param caption Plot caption.
#' @param tag Plot tag.
#' @param x_label X-axis label. If `NULL`, a family-specific coefficient-scale
#'   label is used.
#' @param y_label Y-axis label.
#'
#' @return A `ggplot` object.
#' @export
plot_elasticnet_effects <- function(
  x,
  n_features = 20,
  min_percent = 0,
  labels = TRUE,
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
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
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
      legend.box = "vertical"
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
