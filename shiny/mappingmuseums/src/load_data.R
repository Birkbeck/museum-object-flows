set.seed(1)

data_url <- "https://storage.googleapis.com/mapping-museums-database/latest/"
super_events_file <- "super_events.csv"
actor_types_file <- "actor_types.csv"
event_types_file <- "event_types.csv"
dispersal_events_file <- "dispersal_events.csv"
museums_file <- "museums.csv"

super_events_url <- paste0(data_url, super_events_file)
actor_types_url <- paste0(data_url, actor_types_file)
event_types_url <- paste0(data_url, event_types_file)
dispersal_events_url <- paste0(data_url, dispersal_events_file)
museums_url <- paste0(data_url, museums_file)

DEBOUNCE_TIME <- 1000

distance_categories <- c(
  "all",
  "unknown",
  "end of existence",
  "0",
  "0 - 1",
  "1 - 10",
  "10 - 100",
  "100 - 1,000",
  "1,000+"
)

super_events <- reactive({
  read_csv(super_events_url)
})

event_types <- reactive({
  read_csv(event_types_url)
})

dispersal_events <- reactive({
  read_csv(dispersal_events_url) |>
    mutate(
      distance_category=factor(
        distance_category,
        distance_categories
      ),
      distance_from_initial_museum_category=factor(
        distance_from_initial_museum_category,
        distance_categories
      )
    )
})

senders <- reactive({
  dispersal_events() |>
    select(
      actor_id=sender_id,
      name=sender_name,
      quantity=sender_quantity,
      sector=sender_sector,
      type=sender_type,
      size=sender_size,
      governance=sender_governance,
      accreditation=sender_accreditation,
      town=sender_town,
      county=sender_county,
      postcode=sender_postcode,
      region=sender_region,
      country=sender_country
    )
})

recipients <- reactive({
  dispersal_events() |>
    select(
      actor_id=recipient_id,
      name=recipient_name,
      quantity=recipient_quantity,
      sector=recipient_sector,
      type=recipient_type,
      size=recipient_size,
      governance=recipient_governance,
      accreditation=recipient_accreditation,
      town=recipient_town,
      county=recipient_county,
      postcode=recipient_postcode,
      region=recipient_region,
      country=recipient_country
    )
})

actors <- reactive({
  rbind(senders(), recipients()) |>
    unique()
})

initial_museums <- reactive({
  dispersal_events() |>
    select(
      museum_id=initial_museum_id,
      museum_name=initial_museum_name,
      size=initial_museum_size,
      governance=initial_museum_governance,
      governance_broad=initial_museum_governance_broad,
      subject_broad=initial_museum_subject_broad,
      subject=initial_museum_subject,
      region=initial_museum_region,
      country=initial_museum_country,
      accreditation=initial_museum_accreditation
    ) |>
    distinct() |>
    mutate(
      name=paste0(museum_name, " (", museum_id, ")")
    ) |>
    arrange(name)
})

collection_types <- reactive({
  dispersal_events() |>
    mutate(
      collection_type = str_remove_all(collection_types, "\\[|\\]|'") |>
        str_split(",\\s*")
    ) |>
    unnest(collection_type) |>
    select(collection_type) |>
    unique() |>
    arrange(collection_type) |>
    filter(collection_type != "")
})

not_really_museums <- read_csv("data/not-really-museums.csv")

museums_without_closure_info <- reactive({
  read_csv(museums_url) |>
    filter(!museum_id %in% not_really_museums$museum_id) |>
    mutate(
      year_opened=case_when(
        year_opened_1==year_opened_2 ~ as.character(year_opened_1),
        TRUE ~ paste(year_opened_1, year_opened_2, sep="/")
      ),
      year_closed=case_when(
        year_closed_1==9999 ~ "N/A",
        year_closed_1==year_closed_2 ~ as.character(year_closed_1),
        TRUE ~ paste(year_closed_1, year_closed_2, sep="/")
      ),
      all="all",
      all=factor(all, museum_attribute_ordering),
      size=factor(size, museum_attribute_ordering),
      governance=factor(governance, museum_attribute_ordering),
      governance_broad=factor(governance_broad, museum_attribute_ordering),
      subject=factor(subject, museum_attribute_ordering),
      subject_broad=factor(subject_broad, museum_attribute_ordering),
      accreditation=factor(accreditation, museum_attribute_ordering),
      region=factor(region, museum_attribute_ordering),
      country=factor(country, museum_attribute_ordering),
      lad=ifelse(country %in% c("Channel Islands", "Isle of Man"), country, lad)
    )
})

closure_reasons <- reactive({
  super_events() |>
    separate_rows(reason, sep = "; ") |>
    separate_wider_delim(
      reason,
      " - ",
      names=c("reason_core", "reason_core_or_child", "reason_specific"),
      too_few="align_start"
    ) |>
    mutate(
      reason_core_or_child=ifelse(
        is.na(reason_core_or_child),
        reason_core,
        paste(reason_core, "-", reason_core_or_child)
      ),
      reason_specific=ifelse(
        is.na(reason_specific),
        reason_core_or_child,
        paste(reason_core_or_child, "-", reason_specific)
      )
    ) |>
    left_join(museums_without_closure_info(), by="museum_id")
})

closure_outcomes <- reactive({
  get_outcomes_by_museum(super_events(), dispersal_events())
})
closure_lengths <- reactive({
  get_closure_lengths_by_museum(
    super_events(),
    dispersal_events(),
    event_types(),
    museums_without_closure_info()
  )
})
closure_timeline_events <- reactive({
  get_closure_timeline_events(
    super_events(),
    dispersal_events(),
    event_types(),
    museums_without_closure_info()
  )
})

museums_including_crown_dependencies <- reactive({
  museums_without_closure_info() |>
    left_join(closure_outcomes(), by="museum_id") |>
    left_join(
      closure_reasons() |>
        select(museum_id, reasons_for_closure=super_reasons) |>
        unique(),
      by="museum_id"
    ) |>
    mutate(
      place = gsub(
        "[[:punct:]]",
        "",
        paste(address_1, address_2, address_3, village_town_city, postcode, lad, region)
      ),
      outcome_event_type=factor(outcome_event_type, museum_attribute_ordering),
      outcome_recipient_type=factor(outcome_recipient_type, museum_attribute_ordering),
      outcome_recipient_count=factor(outcome_recipient_count, museum_attribute_ordering),
      outcome_largest_share=factor(outcome_largest_share, museum_attribute_ordering),
      outcome_destination_type=factor(outcome_destination_type, museum_attribute_ordering)
    )
})

museums <- reactive({
  museums_including_crown_dependencies() |>
    filter(!country %in% c("Channel Islands", "Isle of Man"))
})

size_labels <- reactive({
  museums_including_crown_dependencies() |>
    select(label=size) |>
    unique() |>
    arrange(desc(label))
})
governance_broad_labels <- reactive({
  museums_including_crown_dependencies() |>
    select(label=governance_broad) |>
    unique() |>
    arrange(desc(label))
})
governance_labels <- reactive({
  museums_including_crown_dependencies() |>
    select(label=governance) |>
    unique() |>
    arrange(desc(label))
})
subject_broad_labels <- reactive({
  museums_including_crown_dependencies() |>
    select(label=subject_broad) |>
    unique() |>
    arrange(desc(label))
})
subject_labels <- reactive({
  museums_including_crown_dependencies() |>
    select(label=subject) |>
    unique() |>
    arrange(desc(label))
})
accreditation_labels <- reactive({
  museums_including_crown_dependencies() |>
    select(label=accreditation) |>
    unique() |>
    arrange(desc(label))
})
lad_labels <- reactive({
  museums_including_crown_dependencies() |>
    select(label=lad) |>
    unique() |>
    arrange(label)
})
region_labels <- reactive({
  museums_including_crown_dependencies() |>
    select(label=region) |>
    unique() |>
    arrange(desc(label))
})
country_labels <- reactive({
  museums_including_crown_dependencies() |>
    select(label=country) |>
    unique() |>
    arrange(desc(label))
})
reason_core_labels <- reactive({
  closure_reasons() |>
    select(label=reason_core) |>
    unique() |>
    arrange(label)
})
event_core_types <- reactive({
  dispersal_events() |>
    select(label=event_core_type) |>
    unique() |>
    arrange(label)
})
sender_core_types <- reactive({
  dispersal_events() |>
    select(label=sender_core_type) |>
    unique() |>
    arrange(label)
})
recipient_core_types <- reactive({
  dispersal_events() |>
    select(label=recipient_core_type) |>
    unique() |>
    arrange(label)
})
collection_status_labels <- reactive({
  dispersal_events() |>
    select(label=collection_status) |>
    unique() |>
    arrange(label)
})

subject_labels_map <- reactive({
  museums_including_crown_dependencies() |>
    select(subject_broad, subject) |>
    unique() |>
    arrange(desc(subject))
})

regions <- read_csv("data/regions.csv") |>
  mutate(group=paste(L1, L2, L3))

actor_types <- reactive({
  read_csv(actor_types_url)
})
 
field_names <- reactive({
  data.frame(
    name=c(
      "All",
      "Size",
      "Governance",
      "Accreditation",
      "Subject Matter",
      "Country/Region",
      "Country"
    ),
    value=c(
      "all",
      "size",
      "governance_broad",
      "accreditation",
      "subject_broad",
      "region",
      "country"
    )
  )
})

filter_field_choices <- reactive({
  museums_including_crown_dependencies() |>
  select(all, size, governance_broad, accreditation, subject_broad, region, country) |>
  pivot_longer(
    cols=c(all, size, governance_broad, accreditation, subject_broad, region, country),
    names_to=c("field"),
    values_to=c("label")
  ) |>
  unique()
})

subject_filter_field_choices <- reactive({
  museums() |>
    select(subject_broad, subject) |>
    unique() |>
    mutate(subject=fct_rev(factor(subject, levels=museum_attribute_ordering))) |>
    arrange(subject)
})

by_default_ignore <- c("unknown", "Unknown", "Other_Government")

sector_type_ordering_table <- reactive({
  actor_types() |>
    mutate(
      public_proportion=public_instances / total_instances,
      private_proportion=private_instances / total_instances,
      third_proportion=third_instances / total_instances,
      university_proportion=university_instances / total_instances,
      hybrid_proportion=hybrid_instances / total_instances,
      unknown_proportion=unknown_instances / total_instances
    ) |>
    select(
      type_name,
      public_proportion,
      private_proportion,
      third_proportion,
      university_proportion,
      hybrid_proportion,
      unknown_proportion
    ) |>
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
})

sector_type_ordering <- reactive({
  sector_type_ordering_table()$type_name
})
