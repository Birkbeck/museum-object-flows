openings_vs_closures_scatter <- function(data, dimension) {
  data <- data |>
    mutate(`total openings and closures`=turnover)
  ggplot(data, aes(x=closure_rate, y=opening_rate)) +
    geom_point(aes(colour=.data[[dimension]], size=`total openings and closures`), alpha=0.7) +
    geom_abline(colour="grey") +
    geom_text(aes(label=tidy_labels[.data[[dimension]]]), size=6, nudge_y=3) +
    scale_x_continuous(expand=c(0,1), limits=c(0,80)) +
    scale_y_continuous(expand=c(0,1), limits=c(0,80)) +
    scale_size_continuous(
      limits=c(1, 2000),
      range=c(2, 40),
      breaks=c(1, 5, 10, 50, 100, 500, 1000),
      labels=c("1", "5", "10", "50", "100", "500", "1000")
    ) +
    scale_colour_manual(values=museum_attribute_colours) +
    coord_fixed() +
    labs(
      title = "Opening vs Closure Rates",
      x = "Closures per hundred museums",
      y = "Openings per hundred museums"
    ) +
    guides(
      colour="none",
      size=guide_legend(title="Total Openings and Closures")
    ) +
    standard_bars_theme +
    theme(
      legend.position = "right"
    )
}

openings_vs_closures_scatter_small <- function(data, dimension) {
  ggplot(data, aes(x=closure_rate, y=opening_rate)) +
    geom_point(aes(colour=.data[[dimension]], size=turnover)) +
    geom_text_repel(aes(label=very_short_labels[.data[[dimension]]])) +
    geom_abline(colour="grey") +
    scale_x_continuous(expand=c(0,1), limits=c(0,80)) +
    scale_y_continuous(expand=c(0,1), limits=c(0,80)) +
    scale_colour_manual(values=museum_attribute_colours) +
    labs(
      title = "Opening vs Closure Rates",
      x = "Closure Rate",
      y = "Opening Rate"
    ) +
    theme_minimal() +
    theme(
      legend.position = "None",
      axis.title = element_text(size=14),
      axis.text = element_text(size=11),
      axis.line.x.bottom = element_line(colour="black"),
      axis.line.y.left = element_line(colour="black")
    )
}

bar_chart <- function(data, dimension, measure, title, y_label, x_label) {
  if (measure %in% c("closures")) {
    fill_scale <- scale_fill_manual(values=c("TRUE"=red, "FALSE"=blue))
  } else {
    fill_scale <- scale_fill_manual(values=c("TRUE"=blue, "FALSE"=red))
  }
  bar_chart <- ggplot(
    data,
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

bar_chart_small <- function(data, dimension, measure, title, x_label) {
  if (measure %in% c("closures")) {
    fill_scale <- scale_fill_manual(values=c("TRUE"=red, "FALSE"=blue))
  } else {
    fill_scale <- scale_fill_manual(values=c("TRUE"=blue, "FALSE"=red))
  }
  if (measure %in% c("openings", "closures")) {
    axis_max <- max(c(max(data$openings), max(data$closures)))
    axis_min <- 0
  } else if (measure %in% c("start_total", "end_total")) {
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
    data,
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

two_measure_bar_chart <- function(data,
                                  dimension,
                                  measures,
                                  title,
                                  y_label,
                                  x_label,
                                  fill_labels,
                                  fill_values
                                  ) {
  colnames(data) <- ifelse(
    colnames(data) %in% names(fill_labels),
    fill_labels[colnames(data)],
    colnames(data)
  )
  data <- data |>
    pivot_longer(
      cols=measures,
      names_to="label",
      values_to="count"
    )
  fill_scale <- scale_fill_manual(values=fill_values)
  bar_chart <- ggplot(
    data,
    aes(
      x=count,
      y=.data[[dimension]],
      label=.data[[dimension]],
      fill=label
    )
  ) +
    geom_bar(position="dodge", stat="identity", alpha=0.7) +
    scale_y_discrete(labels=tidy_labels) +
    fill_scale +
    guides(fill=guide_legend(reverse=TRUE)) +
    geom_text(
      data=data |> filter(label==measures[1]),
      aes(label=count),
      nudge_y=0.25,
      size=6
    ) +
    geom_text(
      data=data |> filter(label==measures[2]),
      aes(label=count),
      nudge_y=-0.25,
      size=6
    ) +
    labs(
      title = title,
      y = y_label,
      x = x_label,
      fill = ""
    ) +
  standard_bars_theme +
    theme(
      legend.position="bottom"
    )
  bar_chart |> ggplotly(tooltip=c("label", "x", "fill"))
}

two_measure_bar_chart_small <- function(data, dimension, measures, title, fill_labels, fill_values) {
  colnames(data) <- ifelse(
    colnames(data) %in% names(fill_labels),
    fill_labels[colnames(data)],
    colnames(data)
  )
  data <- data |>
    pivot_longer(
      cols=measures,
      names_to="label",
      values_to="count"
    )
  fill_scale <- scale_fill_manual(values=fill_values)
  ggplot(
    data,
    aes(
      x=count,
      y=.data[[dimension]],
      fill=label
    )
  ) +
    geom_bar(position="dodge", stat="identity") +
    geom_vline(xintercept=0, colour="black") +
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
      legend.title = element_text(size=0),
      legend.text = element_text(size=11),
      axis.title.x = element_text(size=0),
      axis.title.y = element_text(size=0),
      axis.text.y = element_text(size=11),
      axis.text.x = element_text(size=11),
      axis.line.x.bottom = element_line(colour="black"),
      legend.position="bottom"
    )
}

changes_map <- function(museums, dimension, measure, start, end) {
  if (measure == "openings") {
    data <- museums |>
      filter(year_opened_2 > start & year_opened_1 < end)
  } else if (measure == "closures") {
    data <- museums |>
      filter(year_closed_2 > start & year_closed_1 < end)
  }
  min_x <- min(museums$bng_x)
  max_x <- max(museums$bng_x)
  min_y <- min(museums$bng_y)
  max_y <- max(museums$bng_y)
  padding <- 10000
  map <- ggplot(data) +
    geom_polygon(data=regions, aes(x=x, y=y, group=group), linewidth=0.1, label=NA, colour="black", fill=NA) +
    geom_point(aes(label=name_of_museum, x=bng_x, y=bng_y, colour=.data[[dimension]]), size=0.5) +
    scale_x_continuous(limits=c(min_x - padding, max_x + padding)) +
    scale_y_continuous(limits=c(min_y - padding, max_y + padding)) +
    scale_colour_manual(values=museum_attribute_colours, labels=tidy_labels) +
    coord_fixed() +
    labs(
      title=paste("Museum", measure, start, "-", end),
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

changes_map_small <- function(museums, dimension, measure, start, end) {
  if (measure == "openings") {
    data <- museums |>
      filter(year_opened_2 > start & year_opened_1 < end)
  } else if (measure == "closures") {
    data <- museums |>
      filter(year_closed_2 > start & year_closed_1 < end)
  }
  ggplot(data) +
    geom_polygon(data=regions, aes(x=x, y=y, group=group), linewidth=0.1, label=NA, colour="black", fill=NA) +
    geom_point(aes(label=name_of_museum, x=bng_x, y=bng_y, colour=.data[[dimension]]), size=0.5) +
    scale_colour_manual(values=museum_attribute_colours, labels=tidy_labels) +
    coord_fixed() +
    labs(
      title=paste("Museum", measure, start, "-", end),
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

heatmap <- function(data, dimension, dimension2, measure, title, y_label, x_label) {
  data <- data |>
    mutate(
      fill_metric=ifelse(
        is.infinite(.data[[measure]]),
        NA,
        .data[[measure]]
      )
    )
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
  if (measure %in% c("openings", "openings_rate", "start_total", "end_total", "start_total_pc", "end_total_pc")) {
    #fill_scale <- scale_fill_gradient(transform="pseudo_log", low=white, high=blue)
    fill_scale <- scale_fill_gradient(low=white, high=blue)
  } else if (measure %in% c("closures", "closures_rate")) {
    #fill_scale <- scale_fill_gradient(transform="pseudo_log", low=white, high=red)
    fill_scale <- scale_fill_gradient(low=white, high=red)
  } else {
    #fill_scale <- scale_fill_gradient2(transform="pseudo_log", low=red, mid=white, high=blue, midpoint=0)
    fill_scale <- scale_fill_gradient2(low=red, mid=white, high=blue, midpoint=0)
  }
  heatmap <- ggplot(
    data,
    aes(
      x=dimension_2,
      y=dimension_1,
      fill=fill_metric
    )
  ) +
    geom_tile(alpha=0.7, show.legend=FALSE) +
    geom_text(aes(label=.data[[measure]]), size=6) +
    scale_x_discrete(labels=short_labels) +
    scale_y_discrete(labels=short_labels) +
    fill_scale +
    labs(
      title=title,
      x=x_label,
      y=y_label
    ) +
  standard_bars_theme +
  theme(
    axis.text.x=element_text(angle=45, vjust=0.5, hjust=1)
  )

  if (measure %in% c("period_total_pc_x", "start_total_pc_x", "end_total_pc_x")) {
    heatmap <- heatmap + geom_hline(data=y_lines, aes(yintercept=y), colour="white")
  }
  if (measure %in% c("period_total_pc_y", "start_total_pc_y", "end_total_pc_y")) {
    heatmap <- heatmap + geom_vline(data=x_lines, aes(xintercept=x), colour="white")
  }
  heatmap <- heatmap +
    geom_hline(yintercept=1.5) +
    geom_vline(xintercept=1.5)


  heatmap |> ggplotly(tooltip=c("x", "y", "fill"))
}

heatmap_small <- function(museums, dimension, dimension2, measure, title) {
  if (measure %in% c("openings", "start_total", "end_total")) {
    #fill_scale <- scale_fill_gradient(transform="pseudo_log", low=white, high=blue)
    fill_scale <- scale_fill_gradient(low=white, high=blue)
  } else if (measure %in% c("closures")) {
    #fill_scale <- scale_fill_gradient(transform="pseudo_log", low=white, high=red)
    fill_scale <- scale_fill_gradient(low=white, high=red)
  } else {
    #fill_scale <- scale_fill_gradient2(transform="pseudo_log", low=red, mid=white, high=blue, midpoint=0)
    fill_scale <- scale_fill_gradient2(low=red, mid=white, high=blue, midpoint=0)
  }
  ggplot(
    museums,
    aes(
      x=dimension_2,
      y=dimension_1,
      fill=.data[[measure]]
    )
  ) +
    geom_tile(alpha=0.7) +
    geom_text(aes(label=round(.data[[measure]], 0)), size=4) +
    scale_x_discrete(labels=very_short_labels) +
    scale_y_discrete(labels=short_labels) +
    fill_scale +
    labs(
      title=title,
      x="",
      y="",
      ) +
    theme_minimal() +
    theme(
      legend.position="Non",
      plot.title=element_text(size=14),
      axis.text.y=element_text(size=11),
      axis.text.x=element_text(size=11, angle=45, vjust=1, hjust=1)
    )
}


cumulative_counts_by_dimension <- function(df, dimension) {
  min_year <- min(df$year_opened_1, df$year_closed_1[df$year_closed_1 != 9999])
  max_year <- max(df$year_opened_2, df$year_closed_2[df$year_closed_2 != 9999])
  all_years <- expand.grid(year = seq(min_year, max_year), dimension_value = unique(df[[dimension]]))
  colnames(all_years)[2] <- dimension
  openings <- df |>
    rowwise() |>
    mutate(
      year = list(seq(year_opened_1, year_opened_2)),
      opening_fraction = 1 / length(year)
    ) |>
    unnest(cols = c(year, opening_fraction)) |>
    group_by(year, .data[[dimension]]) |>
    summarize(opening_count = sum(opening_fraction)) |>
    ungroup()
  closures <- df |>
    rowwise() |>
    mutate(
      year = list(seq(year_closed_1, year_closed_2)),
      closure_fraction = 1 / length(year)
    ) |>
    filter(year_closed_1 != 9999) |>
    unnest(cols = c(year, closure_fraction)) |>
    group_by(year, .data[[dimension]]) |>
    summarize(closure_count = sum(closure_fraction)) |>
    ungroup()
  combined <- full_join(all_years, openings, by = c("year", dimension)) |>
    full_join(closures, by = c("year", dimension)) |>
    replace_na(list(opening_count = 0, closure_count = 0)) |>
    arrange(year) |>
    group_by(.data[[dimension]]) |>
    mutate(
      cumulative_opening = cumsum(opening_count),
      cumulative_closure = cumsum(closure_count),
      total = cumulative_opening - cumulative_closure,
      opening_rate = opening_count / lag(total, default = 1) * 100,
      closure_rate = closure_count / lag(total, default = 1) * 100,
      change = total - lag(total, default = 0),
      percentage_change = ifelse(lag(total, default = 0) == 0, NA, (change / lag(total, default = 0)) * 100)
    )
  return(combined)
}

time_series_line <- function(data, dimension, measure, title, y_label, start_year, end_year) {
  data <- data |>
    filter(
      year >= start_year,
      year <= end_year
    ) |>
    pivot_longer(cols=c(measure), names_to="measure", values_to="value")
  ggplot(
    data,
    aes(x=year, y=value, colour=.data[[dimension]], linetype=measure)
  ) + 
    geom_hline(yintercept=0, colour="black") +
    geom_line(alpha=0.6 , size=1) +
    geom_text(
      data=data |> filter(year==floor(mean(c(start_year, end_year)))),
      aes(y=value*1.1, label=tidy_labels[.data[[dimension]]]),
      size=6
    ) +
    scale_x_continuous(limits=c(start_year, end_year)) +
    scale_y_continuous(limits=function(x){c(min(0, min(x)), max(x))}) +
    scale_colour_manual(values=museum_attribute_colours) +
    open_close_line_type_scale +
    labs(
      title=title,
      y=y_label,
      x="Year"
    ) +
    standard_bars_theme
}

time_series_line_small <- function(data, dimension, measure, title, y_label, start_year, end_year) {
  if (measure %in% c("opening_count", "closure_count")) {
    axis_max <- max(c(max(data$opening_count), max(data$closure_count)))
    axis_min <- 0
  } else {
    axis_max <- max(data[[measure]])
    axis_min <- min(data[[measure]])
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
  data <- data |>
    filter(
      year >= start_year,
      year <= end_year
    ) |>
    pivot_longer(cols=c(measure), names_to="measure", values_to="value")
  ggplot(
    data,
    aes(x=year, y=value, colour=.data[[dimension]], linetype=measure)
  ) + 
    geom_line(alpha=0.6 , size=1) +
    geom_text(
      data=data|> filter(year==floor(mean(c(start_year, end_year)))),
      aes(y=value*1.1, label=tidy_labels[.data[[dimension]]])
    ) +
    scale_x_continuous(expand=c(0, 0), limits=c(start_year, end_year)) +
    scale_y_continuous(expand=c(0,0), limits=limits, breaks=breaks) +
    scale_colour_manual(values=museum_attribute_colours) +
    open_close_line_type_scale +
    labs(
      title=title,
      y=y_label,
      x="Year"
    ) +
    theme_minimal() +
    theme(
      legend.position = "None",
      plot.title = element_text(size=14),
      axis.title = element_text(size=14),
      axis.text = element_text(size=11),
      axis.line.x.bottom = element_line(colour="black"),
      axis.line.y.left = element_line(colour="black")
    )
}

time_series_line_double <- function(data,
                                    dimension,
                                    measures,
                                    title,
                                    y_label,
                                    start_year,
                                    end_year) {
  data <- data |>
    filter(
      year >= start_year,
      year <= end_year
    ) |>
    pivot_longer(cols=measures, names_to="measure", values_to="value")
  ggplot(
    data,
    aes(x=year, y=value, colour=.data[[dimension]], linetype=measure)
  ) + 
    geom_line(alpha=0.6, size=1) +
    geom_text(
      data=data|> filter(year==floor(mean(c(start_year, end_year)))),
      aes(y=value*1.1, label=tidy_labels[.data[[dimension]]])
    ) +
    scale_x_continuous(expand=c(0, 0), limits=c(start_year, end_year)) +
    scale_y_continuous(limits=function(x){c(min(0, min(x)), max(x))}) +
    scale_colour_manual(values=museum_attribute_colours) +
    open_close_line_type_scale +
    labs(
      title=title,
      y=y_label,
      x="Year"
    ) +
    theme_minimal() +
    theme(
      legend.position = "None",
      plot.title = element_text(size=14),
      axis.title = element_text(size=14),
      axis.text = element_text(size=11),
      axis.line.x.bottom = element_line(colour="black"),
      axis.line.y.left = element_line(colour="black")
    )
}

time_series_line_double_small <- function(data,
                                          dimension,
                                          measures,
                                          title,
                                          y_label,
                                          start_year,
                                          end_year) {
  if ("opening_count" %in% measures) {
    axis_max <- max(c(max(data$opening_count), max(data$closure_count)))
    axis_min <- 0
  } else {
    axis_max <- max(data[[measure]])
    axis_min <- min(data[[measure]])
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
  data <- data |>
    pivot_longer(cols=measures, names_to="measure", values_to="value")
  ggplot(
    data,
    aes(x=year, y=value, colour=.data[[dimension]], linetype=measure)
  ) + 
    geom_line(alpha=0.6 , size=1) +
    geom_text(
      data=data|> filter(year==floor(mean(c(start_year, end_year)))),
      aes(y=value*1.1, label=tidy_labels[.data[[dimension]]])
    ) +
    scale_x_continuous(expand=c(0, 0), limits=c(start_year, end_year)) +
    scale_y_continuous(expand=c(0,0), limits=limits, breaks=breaks) +
    scale_colour_manual(values=museum_attribute_colours) +
    open_close_line_type_scale +
    labs(
      title=title,
      y=y_label,
      x="Year"
    ) +
    theme_minimal() +
    theme(
      legend.position = "None",
      plot.title = element_text(size=14),
      axis.title = element_text(size=14),
      axis.text = element_text(size=11),
      axis.line.x.bottom = element_line(colour="black"),
      axis.line.y.left = element_line(colour="black")
    )
}
