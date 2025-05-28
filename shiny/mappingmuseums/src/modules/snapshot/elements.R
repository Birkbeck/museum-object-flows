get_museums_in_snapshot <- function(museums,
                                    size_filter,
                                    governance_filter,
                                    subject_filter,
                                    subject_specific_filter,
                                    region_filter,
                                    accreditation_filter,
                                    start,
                                    end) {
  museums |>
    filter(
      size %in% size_filter,
      governance %in% governance_filter
      | governance_main %in% governance_filter,
      main_subject %in% subject_filter,
      subject_matter %in% subject_specific_filter,
      region %in% region_filter
      | nation %in% region_filter,
      accreditation %in% accreditation_filter,
      year_closed_2 > start,
      year_opened_1 < end
    )
}

museum_map <- function(museums, dimension, year_or_range, start, end, main_axis_filter) {
  if (year_or_range == "Single year") {
    period <- end 
  } else {
    period <- paste0(start, "-", end)
  }
  min_x <- min(museums$bng_x)
  max_x <- max(museums$bng_x)
  min_y <- min(museums$bng_y)
  max_y <- max(museums$bng_y)
  padding <- 10000
  map <- ggplot(museums |> filter(.data[[dimension]] %in% main_axis_filter)) +
    geom_polygon(data=regions, aes(x=x, y=y, group=group), linewidth=0.1, label=NA, colour="black", fill=NA) +
    geom_point(aes(label=name_of_museum, x=bng_x, y=bng_y, colour=.data[[dimension]]), size=0.5) +
    scale_x_continuous(limits=c(min_x - padding, max_x + padding)) +
    scale_y_continuous(limits=c(min_y - padding, max_y + padding)) +
    scale_colour_manual(values=museum_attribute_colours, labels=tidy_labels) +
    coord_fixed() +
    labs(
      title = paste("Museums in the UK", period),
      x = "",
      y = "",
    ) +
    standard_bars_theme +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(colour="white")
    )
  map |> ggplotly(tooltip=c("label", "colour"))
}

museum_map_small <- function(museums, dimension, year_or_range, start, end, main_axis_filter) {
  if (year_or_range == "Single year") {
    period <- end 
  } else {
    period <- paste0(start, "-", end)
  }
  ggplot(museums |> filter(.data[[dimension]] %in% main_axis_filter)) +
    geom_polygon(data=regions, aes(x=x, y=y, group=group), linewidth=0.1, label=NA, colour="black", fill=NA) +
    geom_point(aes(label=name_of_museum, x=bng_x, y=bng_y, colour=.data[[dimension]]), size=0.5) +
    scale_colour_manual(values=museum_attribute_colours, labels=tidy_labels) +
    coord_fixed() +
    labs(
      title=paste("Museums in the UK", period),
      x="",
      y="",
    ) +
    theme_minimal() +
    theme(
      legend.position="Non",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(colour="white")
    )
}

snapshot_bar_chart <- function(data, dimension, measure, title, y_label, x_label, main_axis_filter) {
  fill_scale <- scale_fill_manual(values=c("TRUE"=blue, "FALSE"=red))
  bar_chart <- ggplot(
    data |> filter(.data[[dimension]] %in% main_axis_filter),
    aes(
      x=.data[[measure]],
      y=.data[[dimension]],
      label=.data[[dimension]],
      fill=.data[[measure]] > 0
    )
  ) +
    geom_bar(position="dodge", stat="identity", show.legend=FALSE, alpha=0.7) +
    scale_y_discrete(labels=tidy_labels) +
    fill_scale +
    guides(fill=FALSE) +
    geom_text(
      aes(label=.data[[measure]]),
      position=position_dodge(),
      size=6,
    ) +
    labs(
      title = title,
      y = y_label,
      x = x_label
    ) +
    standard_bars_theme
  bar_chart |> ggplotly(tooltip=c("label", "x"))
}

snapshot_bar_chart_small <- function(data, dimension, measure, title, x_label, main_axis_filter) {
  fill_scale <- scale_fill_manual(values=c("TRUE"=blue, "FALSE"=red))
  if (measure == "end_total") {
    axis_max <- max(c(max(data$start_total), max(data$end_total)))
    axis_min <- 0
  } else {
    axis_max <- max(data[[measure]])
    axis_min <- min(data[[measure]])
    if (axis_max < 0) {
      axis_max <- 0
    }
    if (axis_min > 0) {
      axis_min <- 0
    }
  }
  largest_magnitude <- max(axis_max, abs(axis_min))
  if (largest_magnitude > 100) {
    scaling_factor <- 0.01
  } else {
    scaling_factor <- 0.1
  }
  axis_max <- ceiling(axis_max * scaling_factor) / scaling_factor
  axis_min <- floor(axis_min * scaling_factor) / scaling_factor
  if (axis_min < 0) {
    limits <- c(axis_min*1.1, axis_max*1.1)
    breaks <- c(axis_min, 0, axis_max)
  } else {
    limits <- c(0, axis_max*1.1)
    breaks <- c(0, axis_max)
  }
  ggplot(
    data |> filter(.data[[dimension]] %in% main_axis_filter),
    aes(
      x=.data[[measure]],
      y=.data[[dimension]],
      fill=.data[[measure]] > 0
    )
  ) +
    geom_bar(position="dodge", stat="identity") +
    geom_vline(xintercept=0, colour="black") +
    scale_x_continuous(limits=limits, expand=c(0.001,0), breaks=breaks) +
    scale_y_discrete(labels=short_labels) +
    fill_scale +
    labs(
      title = title,
      y = "",
      x = "" 
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size=14),
      plot.subtitle = element_text(size=0),
      axis.title.x = element_text(size=14),
      axis.title.y = element_text(size=0),
      axis.text.y = element_text(size=11),
      axis.text.x = element_text(size=11),
      axis.line.x.bottom = element_line(colour="black"),
      legend.position="Non"
    )
}

snapshot_heatmap <- function(data,
                             metric,
                             year_or_range,
                             start,
                             end,
                             x_label,
                             y_label,
                             main_axis_filter,
                             second_axis_filter) {
  if (year_or_range == "Single year") {
    period <- end 
  } else {
    period <- paste0(start, "-", end)
  }
  x_lines <- data.frame(
    x=seq_along(
      unique(select(data, dimension_2))$dimension_2
    )
  ) |>
    mutate(x=x+0.5)
  y_lines <- data.frame(
    y=seq_along(
      unique(select(data, dimension_1))$dimension_1
    )
  ) |>
    mutate(y=y+0.5)
  heatmap <- ggplot(
    data |>
      filter(
        dimension_1 == "All" | dimension_1 %in% main_axis_filter,
        dimension_2 == "All" | dimension_2 %in% second_axis_filter
      ),
    aes(
      x=dimension_2,
      y=dimension_1,
      fill=.data[[metric]]
    )
  ) +
    geom_tile(show.legend=FALSE) +
    geom_text(aes(label=.data[[metric]]), size=6) +
    scale_x_discrete(labels=short_labels) +
    scale_y_discrete(labels=short_labels) +
    #scale_fill_continuous(transform="pseudo_log", low=white, high=blue) +
    scale_fill_continuous(low=white, high=blue) +
    labs(
      title=paste("Museums in the UK", period),
      x=x_label,
      y=y_label,
    ) +
    standard_bars_theme +
    theme(
      legend.position="Non",
      axis.text.x=element_text(angle=45, vjust=0.5, hjust=1)
    )
  if (metric %in% c("period_total_pc_x", "end_total_pc_x")) {
    heatmap <- heatmap + geom_hline(data=y_lines, aes(yintercept=y), colour="white")
  }
  if (metric %in% c("period_total_pc_y", "end_total_pc_y")) {
    heatmap <- heatmap + geom_vline(data=x_lines, aes(xintercept=x), colour="white")
  }
  heatmap <- heatmap +
    geom_hline(yintercept=1.5) +
    geom_vline(xintercept=1.5)
  heatmap
}


snapshot_heatmap_small <- function(data,
                                   metric,
                                   year_or_range,
                                   start,
                                   end,
                                   x_label,
                                   y_label,
                                   main_axis_filter,
                                   second_axis_filter) {
  if (year_or_range == "Single year") {
    period <- end 
  } else {
    period <- paste0(start, "-", end)
  }
  ggplot(
    data |>
      filter(
        dimension_1 == "All" | dimension_1 %in% main_axis_filter,
        dimension_2 == "All" | dimension_2 %in% second_axis_filter
      ),
    aes(
      x=dimension_2,
      y=dimension_1,
      fill=.data[[metric]]
    )
  ) +
    geom_tile() +
    geom_text(aes(label=.data[[metric]]), size=3) +
    scale_x_discrete(labels=very_short_labels) +
    scale_y_discrete(labels=short_labels) +
    #scale_fill_continuous(transform="pseudo_log", low=white, high=blue) +
    scale_fill_continuous(low=white, high=blue) +
    labs(
      title=paste0(x_label, " vs ", y_label),
      x="",
      y="",
    ) +
    theme_minimal() +
    theme(
      legend.position="Non",
      plot.title=element_text(size=14),
      axis.text.x=element_text(size=11, angle=45, vjust=0.5, hjust=1),
      axis.text.y=element_text(size=11)
    )
  }
