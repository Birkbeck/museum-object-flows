set.seed(1)

dispersal_events_csv <- "data/query_results/dispersal_events.csv"
dispersal_events <- read_csv(dispersal_events_csv) |>
  mutate(
    initial_museum_all = "All",
    recipient_core_type = ifelse(!is.na(recipient_core_type), recipient_core_type, recipient_general_type)
  )

closure_reasons <- dispersal_events |>
    select(
      museum_id=initial_museum_id,
      museum_name=initial_museum_name,
      cause=super_event_cause_types,
      super_causes=super_event_causes
    ) |>
    distinct() |>
    separate_rows(cause, sep = "; ") |>
    separate_wider_delim(
      cause,
      " - ",
      names=c("closure_reason_top_level", "closure_reason_mid_level", "closure_reason_low_level"),
      too_few="align_start"
    ) |>
    mutate(
      closure_reason_mid_level = ifelse(
        is.na(closure_reason_mid_level),
        paste(closure_reason_top_level, "-", "other"),
        paste(closure_reason_top_level, "-", closure_reason_mid_level)
      ),
      closure_reason_low_level = ifelse(
        is.na(closure_reason_low_level),
        paste(closure_reason_mid_level, "-", "other"),
        paste(closure_reason_mid_level, "-", closure_reason_low_level)
      )
    )

closure_outcomes <- get_outcomes_by_museum(dispersal_events)
museums_including_crown_dependencies <- read_csv("data/all_museums_data.csv") |>
  left_join(closure_outcomes, by="museum_id") |>
  mutate(all="All")
museums <- museums_including_crown_dependencies |>
  filter(!nation %in% c("Channel Islands", "Isle of Man")) |>
  mutate(`No filter`="All") |>
  mutate(
    governance_main=factor(governance_main, museum_attribute_ordering),
    region=factor(region, museum_attribute_ordering)
  )

regions <- read_csv("data/regions.csv") |>
  mutate(group=paste(L1, L2, L3))

actor_types_csv <- "data/query_results/actor_types.csv"
actor_types <- read_csv(actor_types_csv)
 
event_types_csv <- "data/query_results/event_types.csv"
event_types <- read_csv(event_types_csv)

explanations <- read_csv("explanations.csv")

field_names <- data.frame(
  name=c("All", "Size", "Governance", "Accreditation", "Subject Matter", "Country/Region", "Country"),
  value=c("all", "size", "governance_main", "accreditation", "main_subject", "region", "nation")
)
filter_field_choices <- museums |>
  select(size, governance_main, accreditation, main_subject, region, nation) |>
  pivot_longer(
    cols=c(size, governance_main, accreditation, main_subject, region, nation), names_to=c("field")
  ) |>
  mutate(label=unname(tidy_labels[value])) |>
  unique() |>
  arrange(fct_rev(factor(value, museum_attribute_ordering)))
subject_filter_field_choices <- museums |>
  select(main_subject, subject_matter) |>
  unique() |>
  mutate(subject_matter=fct_rev(factor(subject_matter, levels=museum_attribute_ordering))) |>
  arrange(subject_matter)
by_default_ignore <- c("unknown", "Unknown", "Other_Government")

sector_type_ordering_table <- actor_types |>
  mutate(
    public_proportion=public_instances / total_instances,
    private_proportion=private_instances / total_instances,
    third_proportion=third_instances / total_instances,
    university_proportion=university_instances / total_instances,
    hybrid_proportion=hybrid_instances / total_instances,
    unknown_proportion=unknown_instances / total_instances
  ) |>
  select(type_name, public_proportion, private_proportion, third_proportion, university_proportion, hybrid_proportion, unknown_proportion) |>
  bind_rows(
    tibble(
      type_name = c("public", "private", "third", "hybrid"),
      public_proportion = c(1, 0, 0, 0),
      private_proportion = c(0, 1, 0, 0),
      third_proportion = c(0, 0, 1, 0),
      university_proportion = c(0, 0, 0, 0),
      hybrid_proportion = c(0, 0, 0, 1),
      unknown_proportion = c(0, 0, 0, 0)
    )
  ) |>
  mutate(
    type_name = paste0("NA@", type_name)
  ) |>
  bind_rows(
    tibble(
      type_name = c("National@public", "National@museum", "National@organisation"),
      public_proportion = c(4, 4, 4),
      private_proportion = c(0, 0, 0),
      third_proportion = c(0, 0, 0),
      university_proportion = c(0, 0, 0),
      hybrid_proportion = c(0, 0, 0),
      unknown_proportion = c(0, 0, 0)
    )
  ) |>
  bind_rows(
    tibble(
      type_name = c("Other_Government@public", "Other_Government@museum", "Other_Government@organisation"),
      public_proportion = c(3, 3, 3),
      private_proportion = c(0, 0, 0),
      third_proportion = c(0, 0, 0),
      university_proportion = c(0, 0, 0),
      hybrid_proportion = c(0, 0, 0),
      unknown_proportion = c(0, 0, 0)
    )
  ) |>
  bind_rows(
    tibble(
      type_name = c("Local_Authority@public", "Local_Authority@museum", "Local_Authority@organisation"),
      public_proportion = c(2, 2, 2),
      private_proportion = c(0, 0, 0),
      third_proportion = c(0, 0, 0),
      university_proportion = c(0, 0, 0),
      hybrid_proportion = c(0, 0, 0),
      unknown_proportion = c(0, 0, 0)
    )
  ) |>
  bind_rows(
    tibble(
      type_name = c("University@university", "University@museum", "University@organisation"),
      public_proportion = c(0, 0, 0),
      private_proportion = c(0, 0, 0),
      third_proportion = c(0, 0, 0),
      university_proportion = c(2, 2, 2),
      hybrid_proportion = c(0, 0, 0),
      unknown_proportion = c(0, 0, 0)
    )
  ) |>
  bind_rows(
    tibble(
      type_name = c("Unknown@unknown", "Unknown@museum", "Unknown@organisation"),
      public_proportion = c(0, 0, 0),
      private_proportion = c(0, 0, 0),
      third_proportion = c(0, 0, 0),
      university_proportion = c(0, 0, 0),
      hybrid_proportion = c(0, 0, 0),
      unknown_proportion = c(2, 2, 2)
    )
  ) |>
  bind_rows(
    tibble(
      type_name = c("Independent@third", "Independent@museum", "Independent@organisation"),
      public_proportion = c(0, 0, 0),
      private_proportion = c(0, 0, 0),
      third_proportion = c(6, 6, 6),
      university_proportion = c(0, 0, 0),
      hybrid_proportion = c(0, 0, 0),
      unknown_proportion = c(0, 0, 0)
    )
  ) |>
  bind_rows(
    tibble(
      type_name = c("Independent-Not_for_profit@third", "Independent-Not_for_profit@museum", "Independent-Not_for_profit@organisation"),
      public_proportion = c(0, 0, 0),
      private_proportion = c(0, 0, 0),
      third_proportion = c(6, 6, 6),
      university_proportion = c(0, 0, 0),
      hybrid_proportion = c(0, 0, 0),
      unknown_proportion = c(0, 0, 0)
    )
  ) |>
  bind_rows(
    tibble(
      type_name = c("Independent-English_Heritage@third", "Independent-English_Heritage@museum", "Independent-English_Heritage@organisation"),
      public_proportion = c(0, 0, 0),
      private_proportion = c(0, 0, 0),
      third_proportion = c(5, 5, 5),
      university_proportion = c(0, 0, 0),
      hybrid_proportion = c(0, 0, 0),
      unknown_proportion = c(0, 0, 0)
    )
  ) |>
  bind_rows(
    tibble(
      type_name = c("Independent-National_Trust@third", "Independent-National_Trust@museum", "Independent-National_Trust@organisation"),
      public_proportion = c(0, 0, 0),
      private_proportion = c(0, 0, 0),
      third_proportion = c(4, 4, 4),
      university_proportion = c(0, 0, 0),
      hybrid_proportion = c(0, 0, 0),
      unknown_proportion = c(0, 0, 0)
    )
  ) |>
  bind_rows(
    tibble(
      type_name = c("Independent-National_Trust_for_Scotland@third", "Independent-National_Trust_for_Scotland@museum", "Independent-National_Trust_for_Scotland@organisation"),
      public_proportion = c(0, 0, 0),
      private_proportion = c(0, 0, 0),
      third_proportion = c(3, 3, 3),
      university_proportion = c(0, 0, 0),
      hybrid_proportion = c(0, 0, 0),
      unknown_proportion = c(0, 0, 0)
    )
  ) |>
  bind_rows(
    tibble(
      type_name = c("Independent-Historic_Environment_Scotland@third", "Independent-Historic_Environment_Scotland@museum", "Independent-Historic_Environment_Scotland@organisation"),
      public_proportion = c(0, 0, 0),
      private_proportion = c(0, 0, 0),
      third_proportion = c(2, 2, 2),
      university_proportion = c(0, 0, 0),
      hybrid_proportion = c(0, 0, 0),
      unknown_proportion = c(0, 0, 0)
    )
  ) |>
  bind_rows(
    tibble(
      type_name = c("Private@private", "Private@museum", "Private@organisation"),
      public_proportion = c(0, 0, 0),
      private_proportion = c(2, 2, 2),
      third_proportion = c(0, 0, 0),
      university_proportion = c(0, 0, 0),
      hybrid_proportion = c(0, 0, 0),
      unknown_proportion = c(0, 0, 0)
    )
  ) |>
  mutate(
    ordering = public_proportion * 1e6
    + university_proportion * 1e5
    + hybrid_proportion * 1e4
    + unknown_proportion * 1e3
    + third_proportion * 1e2
    + private_proportion * 1e1
  ) |>
  arrange(ordering, desc=TRUE)

sector_type_ordering <- sector_type_ordering_table$type_name

