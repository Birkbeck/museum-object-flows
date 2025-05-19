closure_outcomes_summary_table <- function(museums_table,
                                           outcome_type,
                                           outcome_filter,
                                           size_filter,
                                           governance_filter,
                                           accreditation_filter,
                                           subject_filter,
                                           specific_subject_filter,
                                           region_filter) {
  museums_table |>
    filter(.data[[outcome_type]] %in% outcome_filter) |>
    filter(size %in% size_filter) |>
    filter(governance %in% governance_filter | governance_main %in% governance_filter) |>
    filter(accreditation %in% accreditation_filter) |>
    filter(main_subject %in% subject_filter) |>
    filter(subject_matter %in% specific_subject_filter) |>
    filter(region %in% region_filter | nation %in% region_filter) |>
    group_by(.data[[outcome_type]]) |>
    summarize(frequency=n()) |>
    ungroup() |>
    mutate(percentage=round(frequency / sum(frequency) * 100, 1))
}

closure_outcomes_two_way_summary_table <- function(museums_table,
                                                   outcome_type,
                                                   outcome_filter,
                                                   museum_grouping,
                                                   size_filter,
                                                   governance_filter,
                                                   accreditation_filter,
                                                   subject_filter,
                                                   specific_subject_filter,
                                                   region_filter) {
  number_of_closed_museums <- museums_table |>
    select(museum_id) |>
    distinct() |>
    nrow()

  if(museum_grouping =="closure_reason_top_level") {
    museums_table <- museums_table |>
      left_join(closure_reasons, by="museum_id")
  }

  number_of_closed_museums_by_outcome_type <- museums_table |>
    filter(!is.na(.data[[outcome_type]])) |>
    select(museum_id, .data[[outcome_type]]) |>
    distinct() |>
    group_by(.data[[outcome_type]]) |>
    summarize(number_of_closures=n())

  if(outcome_type==museum_grouping) {
    return(
      closure_outcomes_summary_table(
        museums_table,
        outcome_type,
        outcome_filter,
        size_filter,
        governance_filter,
        accreditation_filter,
        subject_filter,
        specific_subject_filter,
        region_filter
      ) |>
        group_by(.data[[outcome_type]]) |>
        mutate(
          percentage_x=round(frequency / sum(frequency) * 100, 1),
          percentage_y=round(frequency / sum(frequency) * 100, 1)
        ) |>
        ungroup()
    )
  }
  
  museums_table |>
    filter(.data[[outcome_type]] %in% outcome_filter) |>
    filter(size %in% size_filter) |>
    filter(governance %in% governance_filter | governance_main %in% governance_filter) |>
    filter(accreditation %in% accreditation_filter) |>
    filter(main_subject %in% subject_filter) |>
    filter(subject_matter %in% specific_subject_filter) |>
    filter(region %in% region_filter | nation %in% region_filter) |>
    group_by(.data[[museum_grouping]], .data[[outcome_type]]) |>
    summarize(
      frequency=n(),
      percentage=round(frequency / number_of_closed_museums * 100, 1)
    ) |>
    ungroup() |>
    left_join(number_of_closed_museums_by_outcome_type, by=outcome_type) |>
    group_by(.data[[outcome_type]]) |>
    mutate(
      percentage_y=round(frequency / number_of_closures * 100, 1)
    ) |>
    ungroup() |>
    group_by(.data[[museum_grouping]]) |>
    mutate(
      percentage_x=round(frequency / sum(frequency) * 100, 1)
    ) |>
    ungroup()
}

closure_outcomes_over_time_table <- function(museums_table,
                                             outcome_type,
                                             outcome_filter,
                                             size_filter,
                                             governance_filter,
                                             accreditation_filter,
                                             subject_filter,
                                             specific_subject_filter,
                                             region_filter) {
  museums_table |>
    filter(.data[[outcome_type]] %in% outcome_filter) |>
    filter(size %in% size_filter) |>
    filter(governance %in% governance_filter | governance_main %in% governance_filter) |>
    filter(accreditation %in% accreditation_filter) |>
    filter(main_subject %in% subject_filter) |>
    filter(subject_matter %in% specific_subject_filter) |>
    filter(region %in% region_filter | nation %in% region_filter) |>
    filter(!is.na(year_closed_1) & !is.na(year_closed_2)) |>
    filter(year_closed_1 != 9999) |>
    rowwise() |>
    mutate(
      year_closed = mean(c(year_closed_1, year_closed_2)),
      period_of_closure = ifelse(
        year_closed > 1999 & year_closed < 2005,
        "2000-2004",
        ifelse(
          year_closed < 2010,
          "2005-2009",
          ifelse(
            year_closed < 2015,
            "2010-2014",
            ifelse(
              year_closed < 2020,
              "2015-2019",
              "2020-2024"
            )
          )
        )
      ),
      period_of_closure = factor(
        period_of_closure, 
        levels = c("2000-2004", "2005-2009", "2010-2014", "2015-2019", "2020-2024"),
        ordered = TRUE
      )
    ) |>
    group_by(.data[[outcome_type]], period_of_closure) |>
    summarize(frequency=n()) |>
    ungroup() |>
    group_by(period_of_closure) |>
    mutate(percentage=round(frequency / sum(frequency) * 100, 1)) |>
    ungroup()
}
        
closure_outcomes_bar_chart <- function(summary_table, count_or_percentage, outcome_type, outcome_type_name) {
  if (count_or_percentage == "frequency") {
    x_title <- "Number of museum closures with outcome"
  } else {
    x_title <- "Percentage of museum closures with outcome"
  }
  ggplot(summary_table, aes(x=.data[[count_or_percentage]], y=reorder(.data[[outcome_type]], .data[[count_or_percentage]]))) +
    geom_col(fill=purple) +
    geom_text(aes(label=.data[[count_or_percentage]]), hjust="left", nudge_x=1, size=6) +
    labs(
      title="Outcomes of Museum Closure, 2000-2024",
      y=outcome_type_name,
      x=x_title
    ) +
    standard_bars_theme
}

closure_outcomes_bar_chart_small <- function(summary_table, outcome_type) {
  ggplot(summary_table, aes(x=frequency, y=reorder(.data[[outcome_type]], frequency))) +
    geom_col(fill=purple) +
    geom_text(aes(label=frequency), hjust="left", nudge_x=1, size=3) +
    labs(
      title="Outcomes of Museum Closure, 2000-2024",
      y="",
      x="Number of museums"
    ) +
    theme_minimal()
}

closure_outcomes_heatmap <- function(summary_table, count_or_percentage, outcome_type, outcome_type_name, museum_grouping, museum_grouping_name) {
  ggplot(
    summary_table,
    aes(
      x=fct_rev(factor(.data[[museum_grouping]], museum_attribute_ordering)),
      y=.data[[outcome_type]],
      fill=.data[[count_or_percentage]]
    )
  ) +
    geom_tile(show.legend=FALSE) +
    geom_text(aes(label=.data[[count_or_percentage]]), size=6) +
    scale_x_discrete(labels=short_labels) +
    heatmap_fill_scale +
    labs(
      title=paste0("Outcomes of Museum Closure by ", museum_grouping_name, " (Number of Closures)"),
      y=outcome_type_name,
      x=museum_grouping_name
    ) +
    standard_bars_theme +
    theme(
      axis.text.x = element_text(angle=45, hjust=1, vjust=1)
    )
}

closure_outcomes_heatmap_small <- function(summary_table, outcome_type, outcome_type_name, museum_grouping, museum_grouping_name) {
  ggplot(
    summary_table,
    aes(
      x=fct_rev(factor(.data[[museum_grouping]], museum_attribute_ordering)),
      y=.data[[outcome_type]],
      fill=frequency
    )
  ) +
    geom_tile(show.legend=FALSE) +
    geom_text(aes(label=frequency)) +
    scale_x_discrete(labels=short_labels) +
    scale_fill_continuous(low="white", high=purple) +
    labs(
      title=paste0("Outcomes vs ", museum_grouping_name),
      y=outcome_type_name,
      x=museum_grouping_name
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle=45, hjust=1, vjust=1)
    )
}

closure_outcomes_over_time <- function(outcomes_over_time_table, count_or_percentage, outcome_type) {
  if (count_or_percentage == "frequency") {
    y_title <- "Number of museum closures with outcome"
  } else {
    y_title <- "Percentage of museum closures with outcome"
  }
  ggplot(
    outcomes_over_time_table, 
    aes(x=period_of_closure, y=.data[[count_or_percentage]], colour=.data[[outcome_type]])
  ) +
    geom_line(alpha=0.7, aes(group=.data[[outcome_type]])) +
    geom_point() +
    geom_text(
      data=outcomes_over_time_table |> filter(period_of_closure=="2010-2014"),
      position=position_jitter(width=1, height=1, seed=1),
      aes(label=.data[[outcome_type]], colour=.data[[outcome_type]]),
      size=6
    ) +
    guides(
      colour="none"
    ) +
    labs(
      title="Changing Outcomes of Museum Closure Over Time",
      x="Year of Closure",
      y=y_title,
      colour="Outcome of closure"
    ) +
    standard_bars_theme
}

closure_outcomes_over_time_small <- function(outcomes_over_time_table, outcome_type) {
  ggplot(
    outcomes_over_time_table, 
    aes(x=period_of_closure, y=frequency, colour=.data[[outcome_type]])
  ) +
    geom_line(alpha=0.7, aes(group=.data[[outcome_type]])) +
    geom_point() +
    geom_text(
      data=outcomes_over_time_table |> filter(period_of_closure=="2010-2014"),
      position=position_jitter(width=1, height=1, seed=1),
      aes(label=.data[[outcome_type]], colour=.data[[outcome_type]])
    ) +
    guides(
      colour="none"
    ) +
    labs(
      title="Changing Outcomes of Museum Closure Over Time",
      x="Year of Closure",
      y="Number of museum closures with outcome",
      colour="Outcome of closure"
    ) +
    theme_minimal()
}

museum_closure_outcomes_table <- function(museums_including_crown_dependencies,
                                          outcome_type,
                                          outcome_filter,
                                          size_filter,
                                          governance_filter,
                                          accreditation_filter,
                                          subject_filter,
                                          specific_subject_filter,
                                          region_filter) {
  causes <- dispersal_events |>
    select(museum_id=initial_museum_id, reasons_for_closure=super_event_causes) |>
    distinct()
  museums_including_crown_dependencies |>
    filter(!is.na(outcome_event_type)) |>
    filter(.data[[outcome_type]] %in% outcome_filter) |>
    filter(size %in% size_filter) |>
    filter(governance %in% governance_filter | governance_main %in% governance_filter) |>
    filter(accreditation %in% accreditation_filter) |>
    filter(main_subject %in% subject_filter) |>
    filter(subject_matter %in% specific_subject_filter) |>
    filter(region %in% region_filter | nation %in% region_filter) |>
    left_join(causes, by="museum_id") |>
    mutate(
      year_opened = paste(year_opened_1, year_opened_2, sep=":"),
      year_closed = paste(year_closed_1, year_closed_2, sep=":")
    ) |>
    select(
      museum_id,
      museum_name=name_of_museum,
      year_opened,
      year_closed,
      reasons_for_closure,
      outcome_event_type,
      outcome_recipient_type,
      outcome_destination_type,
      size,
      governance,
      accreditation,
      subject_matter,
      region
    )
}
