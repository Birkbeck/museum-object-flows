closure_length_categories <- c(
  "All",
  "unknown",
  "< 1 year",
  "< 2 years",
  "< 4 years",
  "< 8 years",
  "< 16 years",
  "16+ years"
)

get_event_dates_table <- function() {
  closure_super_events <- dispersal_events |>
    mutate(museum_id=initial_museum_id) |>
    left_join(museums_including_crown_dependencies, by="museum_id") |>
    mutate(
      event_date=ifelse(
        year_closed_1==year_closed_2,
        year_closed_1,
        paste(year_closed_1, year_closed_2, sep="-")
      ),
      year1 = year_closed_1,
      year2 = year_closed_2,
      year = (year1 + year2) / 2
    ) |>
    mutate(
      event_level="super",
      event_description="closure"
    ) |>
    select(
      museum=initial_museum_name,
      museum_id=initial_museum_id,
      event_level,
      event_date,
      year1,
      year2,
      year,
      event_description
    ) |>
    distinct()
  closure_events <- dispersal_events |>
    filter(sender_name == initial_museum_name) |>
    mutate(
      event_level="sub",
      event_description=ifelse(
        is.na(collection_description),
        paste(collection_id, event_type, "to", recipient_name),
        paste(collection_description, event_type, "to", recipient_name)
      )
    ) |>
    select(
      museum=initial_museum_name,
      museum_id=initial_museum_id,
      event_level,
      event_date,
      event_description
    ) |>
    mutate(
      year1 = sub("/.*", "", event_date),
      year2 = sub(".*/", "", event_date),
      year1 = sub("-.*", "", year1),
      year2 = sub("-.*", "", year2),
      year1 = sub("\\?", "", year1),
      year2 = sub("\\?", "", year2),
      year1 = as.numeric(year1),
      year2 = as.numeric(year2),
      year2 = ifelse(is.na(year2), year1, year2),
      year = (year1 + year2) / 2,
    ) |>
    filter(year > 1000) |>
    select(
      museum,
      museum_id,
      event_level,
      event_date,
      year1,
      year2,
      year,
      event_description
    ) |>
    rbind(closure_super_events)

  museum_ordering <- closure_events |>
    filter(event_level=="super") |>
    distinct() |>
    arrange(year)
  
  closure_lengths <- closure_events |>
    filter(event_level=="sub") |>
    filter(!is.na(year)) |>
    group_by(museum_id) |>
    summarize(
      earliest = min(year),
      latest = max(year)
    ) |>
    left_join(closure_events |> filter(event_level=="super") |> select(museum_id, year), by="museum_id") |>
    rename(closure_date=year) |>
    mutate(
      time_between_closure_and_earliest = earliest - closure_date,
      latest_including_closure_date = ifelse(closure_date > latest, closure_date, latest),
      length_of_closure = latest_including_closure_date - closure_date
    )

  closure_events <- closure_events |> left_join(closure_lengths, by="museum_id") |>
    filter(
      closure_date < 9999,
      event_level == "super" | year1 >= closure_date
    ) |>
    mutate(
      closure_length_category = case_when(
        is.na(length_of_closure) ~ "unknown",
        length_of_closure < 1 ~ "< 1 year",
        length_of_closure < 2 ~ "< 2 years",
        length_of_closure < 4 ~ "< 4 years",
        length_of_closure < 8 ~ "< 8 years",
        length_of_closure < 16 ~ "< 16 years",
        TRUE ~ "16+ years"
      ),
      closure_length_category = factor(closure_length_category, closure_length_categories)
    )
}

get_lengths_table <- function(event_dates_table) {
  event_dates_table |>
    filter(event_level=="super") |>
    left_join(museums_including_crown_dependencies, by="museum_id") |>
    select(
      museum_id,
      museum,
      closure_date=event_date,
      earliest,
      latest,
      length_of_closure,
      closure_length_category,
      all,
      size,
      governance,
      governance_main,
      subject_matter,
      main_subject,
      region,
      accreditation
    )
} 

get_lengths_two_way_table <- function(lengths_table, museum_grouping) {
  heatmap_data_2_way <- lengths_table |>
    group_by(closure_length_category, .data[[museum_grouping]]) |>
    summarize(count=n()) |>
    ungroup() |>
    mutate(percentage=round(count / sum(count) * 100, 1)) |>
    group_by(.data[[museum_grouping]]) |>
    mutate(percentage_y=round(count / sum(count) * 100, 1)) |>
    ungroup() |>
    group_by(closure_length_category) |>
    mutate(percentage_x=round(count / sum(count) * 100, 1)) |>
    ungroup()
  heatmap_data_museum_totals <- lengths_table |>
    group_by(.data[[museum_grouping]]) |>
    summarize(count=n()) |>
    ungroup() |>
    mutate(
      closure_length_category="All",
      percentage=round(count / sum(count) * 100, 1),
      percentage_x=percentage,
      percentage_y=100
    )
  heatmap_data_length_totals <- lengths_table |>
    group_by(closure_length_category) |>
    summarize(count=n()) |>
    ungroup() |>
    mutate(
      !!sym(museum_grouping):="All",
      percentage=round(count / sum(count) * 100, 1),
      percentage_x=100,
      percentage_y=percentage
    )
  heatmap_data_all_totals <- lengths_table |>
    summarize(
      !!sym(museum_grouping):="All",
      closure_length_category="All",
      count=n(),
      percentage=100,
      percentage_x=100,
      percentage_y=100
    )
  heatmap_data_2_way |>
    rbind(heatmap_data_museum_totals) |>
    rbind(heatmap_data_length_totals) |>
    rbind(heatmap_data_all_totals) |>
    mutate(
      !!sym(museum_grouping):=factor(.data[[museum_grouping]], museum_attribute_ordering),
      closure_length_category = factor(closure_length_category, closure_length_categories)
    )
}

length_tile_chart <- function(heatmap_data, count_or_percentage, museum_grouping) {
  x_lines <- data.frame(
    x=seq_along(
      unique(select(heatmap_data, closure_length_category))$closure_length_category
    )
  ) |>
    mutate(x=x+0.5)
  y_lines <- data.frame(
    y=seq_along(
      unique(select(heatmap_data, .data[[museum_grouping]]))[[museum_grouping]]
    )
  ) |>
    mutate(y=y+0.5)
  heatmap <- ggplot(
    heatmap_data,
    aes(
      x=closure_length_category,
      y=.data[[museum_grouping]],
      fill=.data[[count_or_percentage]]
    )
  ) +
    geom_tile(show.legend=FALSE) +
    geom_text(aes(label=.data[[count_or_percentage]])) +
    scale_y_discrete(labels=short_labels) +
    heatmap_fill_scale +
    labs(
      title = "Length of closure",
      x = "Length of closure (years)",
      y = ""
    ) +
    standard_bars_theme +
    theme(
      axis.text.x=element_text(angle=45, vjust=0.5, hjust=1)
    )
  if (count_or_percentage == "percentage_y") {
    heatmap <- heatmap + geom_hline(data=y_lines, aes(yintercept=y), colour="white")
  }
  if (count_or_percentage == "percentage_x") {
    heatmap <- heatmap + geom_vline(data=x_lines, aes(xintercept=x), colour="white")
  }
  heatmap <- heatmap +
    geom_hline(yintercept=1.5) +
    geom_vline(xintercept=1.5)
  heatmap |> ggplotly(tooltip=c("x", "y", "fill"))
}

length_tile_chart_small <- function(heatmap_data, museum_grouping) {
  ggplot(
    heatmap_data,
    aes(
      x=closure_length_category,
      y=.data[[museum_grouping]]
    )
  ) +
    geom_tile(aes(fill=count)) +
    geom_text(aes(label=count)) +
    scale_y_discrete(labels=short_labels) +
    heatmap_fill_scale +
    labs(
      title = "Length of closure",
      x = "Length of closure (years)",
      y = ""
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size=14),
      plot.subtitle = element_text(size=0),
      axis.title.x = element_text(size=14),
      axis.title.y = element_text(size=0),
      axis.text.y = element_text(size=11),
      axis.text.x = element_text(size=11, angle=45, hjust=1),
      axis.line.x.bottom = element_line(colour="black"),
      legend.position="Non"
    )
}

length_line_chart <- function(lengths_table, count_or_percentage, museum_grouping) {
  line_data <- lengths_table |>
    group_by(closure_length_category, .data[[museum_grouping]]) |>
    summarize(count=n()) |>
    ungroup() |>
    group_by(.data[[museum_grouping]]) |>
    mutate(percentage=round(count / sum(count) * 100, 1)) |>
    ungroup()
  ggplot(
    line_data,
    aes(
      x=closure_length_category,
      y=.data[[count_or_percentage]],
      group=.data[[museum_grouping]],
      colour=.data[[museum_grouping]]
    )
  ) +
    geom_point() +
    geom_line() +
    geom_text(
      data=line_data |> filter(closure_length_category=="< 4 years"),
      aes(y=.data[[count_or_percentage]]*1.1, label=tidy_labels[.data[[museum_grouping]]])
    ) +
    scale_colour_manual(values=museum_attribute_colours) +
    guides(colour="none") +
    labs(
      title = "Length of closure",
      x = "Length of closure (years)",
      y = "Number of Museums"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size=14),
      plot.subtitle = element_text(size=0),
      axis.title.x = element_text(size=14),
      axis.title.y = element_text(size=14),
      axis.text.y = element_text(size=11),
      axis.text.x = element_text(size=11, angle=45, hjust=1),
      axis.line.x.bottom = element_line(colour="black"),
      legend.position="Non"
    )
}

length_line_chart_small <- function(lengths_table, museum_grouping) {
  line_data <- lengths_table |>
    group_by(closure_length_category, .data[[museum_grouping]]) |>
    summarize(count=n()) |>
    ungroup()
  ggplot(
    line_data,
    aes(
      x=closure_length_category,
      y=count,
      group=.data[[museum_grouping]],
      colour=.data[[museum_grouping]]
    )
  ) +
    geom_point() +
    geom_line() +
    geom_text(
      data=line_data |> filter(closure_length_category=="< 4 years"),
      aes(y=count*1.1, label=tidy_labels[.data[[museum_grouping]]])
    ) +
    scale_colour_manual(values=museum_attribute_colours) +
    labs(
      title = "Length of closure",
      x = "Length of closure (years)",
      y = "Number of Museums"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size=14),
      plot.subtitle = element_text(size=0),
      axis.title.x = element_text(size=14),
      axis.title.y = element_text(size=14),
      axis.text.y = element_text(size=11),
      axis.text.x = element_text(size=11, angle=45, hjust=1),
      axis.line.x.bottom = element_line(colour="black"),
      legend.position="Non"
    )
}

length_scatter <- function(lengths_table, museum_grouping) {
  ggplot(
    lengths_table,
    aes(
      x=length_of_closure,
      y=.data[[museum_grouping]],
      label=museum
    )
  ) +
    geom_point(position=position_jitter(height=0.3, width=0, seed=1), alpha=0.5) +
    scale_y_discrete(labels=short_labels) +
    labs(
      title = "Length of closure",
      x = "Length of closure (years)",
      y = ""
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

length_scatter_small <- function(lengths_table, museum_grouping) {
  ggplot(
    lengths_table,
    aes(
      x=length_of_closure,
      y=.data[[museum_grouping]]
    )
  ) +
    geom_point(position=position_jitter(height=0.3, width=0, seed=1), alpha=0.5) +
    scale_y_discrete(labels=short_labels) +
    labs(
      title = "Length of closure",
      x = "Length of closure (years)",
      y = ""
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

example_timelines <- function(events_table, example_museum_id) {
  events <- events_table |>
    filter(museum_id==example_museum_id) |>
    mutate(
      event_id = row_number(),
      ymax=event_id * 2,
      ymin=ymax - 0.5,
      year2 = year2 + 1
    )
  ggplot(
    events
  ) +
    geom_rect(
      aes(
        xmin=year1,
        xmax=year2,
        ymin=ymin,
        ymax=ymax,
        fill=event_level,
        label=event_description
      ),
      colour="black"
    ) +
    scale_fill_manual(values=c("sub"=green, "super"=red)) +
    labs(x = "Date", y = "", title = "Timeline of Post-closure events") +
    theme_minimal() +
    theme(
      plot.title = element_text(size=14),
      plot.subtitle = element_text(size=0),
      axis.title.x = element_text(size=14),
      axis.text.x = element_text(size=11),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x.bottom = element_line(colour="black"),
      legend.position="Non"
    )
}

example_timelines_small <- function(events_table, example_museum_id) {
  events <- events_table |>
    filter(museum_id==example_museum_id) |>
    mutate(
      event_id = row_number(),
      ymax=event_id * 2,
      ymin=ymax - 0.5,
      year2 = year2 + 1
    )
  ggplot(
    events
  ) +
    geom_rect(
      aes(
        xmin=year1,
        xmax=year2,
        ymin=ymin,
        ymax=ymax,
        fill=event_level
      ),
      colour="black"
    ) +
    scale_fill_manual(values=c("sub"=green, "super"=red)) +
    labs(x = "Date", y = "", title = "Timeline of Post-closure events") +
    theme_minimal() +
    theme(
      plot.title = element_text(size=14),
      plot.subtitle = element_text(size=0),
      axis.title.x = element_text(size=14),
      axis.text.x = element_text(size=11),
      axis.title.y = element_text(size=0),
      axis.text.y = element_text(size=0),
      axis.line.x.bottom = element_line(colour="black"),
      legend.position="Non"
    )
}
