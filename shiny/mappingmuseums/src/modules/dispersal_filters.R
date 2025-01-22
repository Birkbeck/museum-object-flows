popover_js <- HTML("
$(document).ready(function() {
  $('[data-toggle=\"popover\"]').popover({
    trigger: 'hover', // Show on hover
    container: 'body', // Attach to body to avoid layout issues
    html: true
  });
});")

dispersalFiltersUI <- function(id) {
  tagList(
    h3("Filter Transactions"),
    tagList(
      tags$span(
        tags$strong("Group by: "),
        tags$i(
          class = "fa fa-info-circle", # Font Awesome info icon
          style = "color: #007bff; cursor: pointer;", # Style for visibility
          `data-toggle` = "popover", # Bootstrap popover attribute
          `data-placement` = "right", # Position above the icon
          title = "Group actors by",
          `data-content` = "<p>Select how to group and display actors on the diagram.</p><p>Museums from the Mapping Museums Database are always grouped according to governance.</p><p>Other actors can be grouped according to:</p><p><strong>Actor sector:</strong> The sector of the economy (<i>public</i>, <i>private</i>, <i>third</i>, <i>etc.</i>) that they belong to.</p><p><strong>Most specific actor type:</strong> The most specific actor type that actors are known to belong to.</p><p><strong>Core category actor type:</strong> The core category that they belong to. Refer to the actor types hierarchy to see which types are included as core categories.</p><p><strong>Most general actor type:</strong> The most general actor type that actors are known to belong to (<i>organisation</i>, <i>individual</i>, or <i>historic house</i>).</p>"
        )
      ),
      tags$script(popover_js),
      selectInput(
        NS(id, "grouping"),
        label="",
        choices=c("Actor Sector", "Actor Type (Core Categories)", "Actor Type (Most General)", "Actor Type (Most Specific)"),
        selected="Actor Sector"
      )
    ),

    tagList(
      tags$span(
        tags$strong("Show transfer types: "),
        tags$i(
          class = "fa fa-info-circle", # Font Awesome info icon
          style = "color: #007bff; cursor: pointer;", # Style for visibility
          `data-toggle` = "popover", # Bootstrap popover attribute
          `data-placement` = "right", # Position above the icon
          title = "Show transfer types",
          `data-content` = "<p>Select which transactions should appear on the diagram.</p><p>Most <strong>changes of ownership</strong> are also changes of custody, but occasionally an item is sold without being sent to its new owner.</p><p><strong>Changes of custody</strong> include a wider range of movements than changes of ownership (e.g. <i>loaned</i> and <i>stored</i>).</p><p><strong>End of existence</strong> is represented as a transfer to no recipient.</p>"
        )
      ),
      tags$script(popover_js),
      pickerInput(
        NS(id, "transactionTypeFilter"), 
        "", # Empty label because we've added custom label with icon above
        choices = c("Change of ownership", "Change of custody", "End of existence"),
        selected = c("Change of ownership", "Change of custody", "End of existence"),
        options = pickerOptions(
          actionsBox = TRUE, 
          size = 10,
          selectedTextFormat = "count > 3"
        ), 
        multiple = TRUE
      )
    ),

    tagList(
      tags$span(
        tags$strong("Show event types: "),
        tags$i(
          class = "fa fa-info-circle", # Font Awesome info icon
          style = "color: #007bff; cursor: pointer;", # Style for visibility
          `data-toggle` = "popover", # Bootstrap popover attribute
          `data-placement` = "right", # Position above the icon
          title = "Show event types",
          `data-content` = "<p>Select which (core category) event types should appear on the diagram.</p><p>Refer to the events tab to see which events belong to each core category and for a definition of each event type.</p>"
        )
      ),
      tags$script(popover_js),
      pickerInput(
        NS(id, "eventTypeFilter"), 
        "", 
        choices=NULL,
        selected=NULL,
        options=pickerOptions(
          actionsBox=TRUE, 
          size=10,
          selectedTextFormat="count > 3"
        ), 
        multiple=TRUE
      )  
    ),

    tagList(
      tags$span(
        tags$strong("Show events with uncertainty level: "),
        tags$i(
          class = "fa fa-info-circle", # Font Awesome info icon
          style = "color: #007bff; cursor: pointer;", # Style for visibility
          `data-toggle` = "popover", # Bootstrap popover attribute
          `data-placement` = "right", # Position above the icon
          title = "Show events with uncertainty level",
          `data-content` = "<p>Filter for events with certain or uncertain types.</p><p><strong>Certain: </strong>Events where the type of event is certain.</p><p><strong>?+: </strong>Events where the type of event is highly likely.</p><p><strong?: </strong>Events where the type of event is probable.</p><p><strong>?-: Events where the type of event is possible.</p>"
        )
      ),
      tags$script(popover_js),
      pickerInput(
        NS(id, "eventTypeUncertaintyFilter"), 
        "", 
        choices=c("certain", "?+", "?", "?-"),
        selected=c("certain", "?+", "?", "?-"),
        options=pickerOptions(
          actionsBox=TRUE, 
          size=10,
          selectedTextFormat="count > 3"
        ), 
        multiple=TRUE
      )  
    ),
    
    tagList(
      tags$span(
        tags$strong("Status of collection involved in events: "),
        tags$i(
          class = "fa fa-info-circle", # Font Awesome info icon
          style = "color: #007bff; cursor: pointer;", # Style for visibility
          `data-toggle` = "popover", # Bootstrap popover attribute
          `data-placement` = "right", # Position above the icon
          title = "Status of collection involved in events",
          `data-content` = "<p><strong>Items from a museum's collection: </strong>Items that originally formed part of a now closed museum's collection</p><p><strong>Items loaned to a museum: </strong>Loaned items that were in the custody of a museum when it closed</p><p><strong>Items for handling: </strong>Items that belonged to a now closed museum, but that were not part of the official collection and could be handled for educational purposes.</p><p><strong>Other items: </strong>Items that belonged to a now closed museum, but that were not museum objects. For example display cases and other furniture.</p>"
        )
      ),
      tags$script(popover_js),
      pickerInput(
        NS(id, "collectionStatusFilter"), 
        "", 
        choices=collection_status_labels$tidy_label,
        selected=filter(collection_status_labels, default_filter)$tidy_label,
        options=pickerOptions(
          actionsBox=TRUE, 
          size=10,
          selectedTextFormat="count > 3"
        ), 
        multiple=TRUE
      )   
    ),

    hr(style=hr_style),

    tagList(
      tags$span(
        tags$strong("Initial museum: "),
        tags$i(
          class = "fa fa-info-circle",
          style = "color: #007bff; cursor: pointer;",
          `data-toggle` = "popover",
          `data-placement` = "right",
          title = "Initial museum",
          `data-content` = "<p>The initial closed museums from which the depicted sequences begin.</p><p>This field updates with a list of museums according to the filters below.</p><p>It is possible to search for and select an individual museum so that only collection transfers starting at that museum are shown in the diagram.</p>"
        )
      ),
      tags$script(popover_js),
      virtualSelectInput(
        NS(id, "initialMuseum"),
        "",
        choices=NULL,
        selected=NULL,
        multiple=TRUE,
        disableSelectAll=FALSE,
        search=TRUE
      ),
      ),

    tagList(
      tags$span(
        tags$strong("Initial museum governance: "),
        tags$i(
          class = "fa fa-info-circle",
          style = "color: #007bff; cursor: pointer;",
          `data-toggle` = "popover",
          `data-placement` = "right",
          title = "Initial museum governance",
          `data-content` = "<p>The governance structure of the museum</p>"
        )
      ),
      tags$script(popover_js),
      pickerInput(
        NS(id, "startGovernanceFilter"), 
        "",
        choices=governance_labels$tidy_label,
        selected=filter(governance_labels, default_filter)$tidy_label,
        options=pickerOptions(
          actionsBox=TRUE, 
          size=10,
          selectedTextFormat="count > 3"
        ), 
        multiple=TRUE
      ) 
    ),

    tagList(
      tags$span(
        tags$strong("Initial museum size: "),
        tags$i(
          class = "fa fa-info-circle",
          style = "color: #007bff; cursor: pointer;",
          `data-toggle` = "popover",
          `data-placement` = "right",
          title = "Initial museum size",
          `data-content` = "<p>The size of the initial museum. Museum sizes are based on approximate annual visitor numbers:</p><p><strong>Small: </strong>0 - 10,000 annual visitors.</p><p><strong>Medium: </strong>10,000 - 50,000 annual visitors</p><p><strong>Large: </strong>50,000 - 1,000,000 annual visitors.</p><p><strong>Huge: </strong>More than 1,000,000 annual visitors.</p>"
        )
      ),
      tags$script(popover_js),
      pickerInput(
        NS(id, "startSizeFilter"), 
        "",
        choices=size_labels$tidy_label,
        selected=filter(size_labels, default_filter)$tidy_label,
        options=pickerOptions(
          actionsBox=TRUE, 
          size=10,
          selectedTextFormat="count > 3"
        ), 
        multiple=TRUE
      ) 
    ),

    tagList(
      tags$span(
        tags$strong("Initial museum subject: "),
        tags$i(
          class = "fa fa-info-circle",
          style = "color: #007bff; cursor: pointer;",
          `data-toggle` = "popover",
          `data-placement` = "right",
          title = "Initial museum subject",
          `data-content` = "<p>The subject matter of the initial museum.</p>"
        )
      ),
      tags$script(popover_js),
      pickerInput(
        NS(id, "startSubjectFilter"), 
        "",
        choices=subject_broad_labels$tidy_label,
        selected=filter(subject_broad_labels, default_filter)$tidy_label,
        options=pickerOptions(
          actionsBox=TRUE, 
          size=10,
          selectedTextFormat="count > 3"
        ), 
        multiple=TRUE
      )  
    ),


    tagList(
      tags$span(
        tags$strong("Initial museum subject (specific): "),
        tags$i(
          class = "fa fa-info-circle",
          style = "color: #007bff; cursor: pointer;",
          `data-toggle` = "popover",
          `data-placement` = "right",
          title = "Initial museum subject (specific)",
          `data-content` = "<p>Specific categories of museum subject matter.</p>"
        )
      ),
      tags$script(popover_js),
      pickerInput(
        NS(id, "startSubjectSpecificFilter"), 
        "",
        choices=NULL,
        selected=NULL,
        options=pickerOptions(
          actionsBox=TRUE, 
          size=10,
          selectedTextFormat="count > 3"
        ), 
        multiple=TRUE
      )
    ),
    
    tagList(
      tags$span(
        tags$strong("Initial museum country/region: "),
        tags$i(
          class = "fa fa-info-circle",
          style = "color: #007bff; cursor: pointer;",
          `data-toggle` = "popover",
          `data-placement` = "right",
          title = "Initial museum country or region",
          `data-content` = "<p>Where in the United Kingdom the museum is located.</p>"
        )
      ),
      tags$script(popover_js),
      pickerInput(
        NS(id, "startRegionFilter"), 
        "",
        choices=country_region_labels$tidy_label,
        selected=filter(country_region_labels, default_filter)$tidy_label,
        options=pickerOptions(
          actionsBox=TRUE, 
          size=10,
          selectedTextFormat="count > 3"
        ), 
        multiple=TRUE
      )   
    ),
    
    tagList(
      tags$span(
        tags$strong("Initial museum accreditation: "),
        tags$i(
          class = "fa fa-info-circle",
          style = "color: #007bff; cursor: pointer;",
          `data-toggle` = "popover",
          `data-placement` = "right",
          title = "Initial museum accreditation",
          `data-content` = "<p>Whether or not the museum was accredited at the time of closure.</p>"
        )
      ),
      tags$script(popover_js),
      pickerInput(
        NS(id, "startAccreditationFilter"), 
        "",
        choices=accreditation_labels$tidy_label,
        selected=filter(accreditation_labels, default_filter)$tidy_label,
        options=pickerOptions(
          actionsBox=TRUE, 
          size=10,
          selectedTextFormat="count > 3"
        ), 
        multiple=TRUE
      )   
    ),


    hr(style=hr_style),
    
    tagList(
      tags$span(
        tags$strong("Final destination: "),
        tags$i(
          class = "fa fa-info-circle",
          style = "color: #007bff; cursor: pointer;",
          `data-toggle` = "popover",
          `data-placement` = "right",
          title = "Final destination",
          `data-content` = "<p>The final actor in the sequence of transfers. The values in this field update according to how actors are grouped on the diagram.</p>"
        )
      ),
      tags$script(popover_js),
      pickerInput(
        NS(id, "sequenceEnd"), 
        "",
        choices=NULL,
        selected=NULL,
        options=pickerOptions(
          actionsBox=TRUE, 
          size=10,
          selectedTextFormat="count > 3"
        ), 
        multiple=TRUE
      )
    ),
    tagList(
      tags$span(
        tags$strong("Passes through: "),
        tags$i(
          class = "fa fa-info-circle",
          style = "color: #007bff; cursor: pointer;",
          `data-toggle` = "popover",
          `data-placement` = "right",
          title = "Passes through",
          `data-content` = "<p>Filter sequences that only pass through specified actors at some point in the sequence of transfers. The values in this field update according to how actors are grouped on the diagram.</p>"
        )
      ),
      tags$script(popover_js),
      pickerInput(
        NS(id, "sequencePassesThrough"), 
        "",
        choices=NULL,
        selected=NULL,
        options=pickerOptions(
          actionsBox=TRUE, 
          size=10,
          selectedTextFormat="count > 3"
        ), 
        multiple=TRUE
      )
    ),

    checkboxInput(
      NS(id, "showTransactionCounts"),
      label="Show Transaction Counts",
      value=FALSE
    ),
  )
}

dispersalFiltersServer <- function(id, stepsOrFirstLast) {
  moduleServer(id, function(input, output, session) {
    grouping_field <- reactive({input$grouping})

    transaction_type_filter <- reactive({input$transactionTypeFilter})

    event_type_filter <- reactive({input$eventTypeFilter})
    event_type_uncertainty_filter <- reactive({input$eventTypeUncertaintyFilter})
    collection_status_filter <- reactive({
      filter(
        collection_status_labels,
        tidy_label %in% input$collectionStatusFilter
      )$internal_label
    })

    size_filter_choices <- reactive({
      filter(
        size_labels,
        tidy_label %in% input$startSizeFilter
      )$internal_label
    })
    governance_filter_choices <- reactive({
      filter(
        governance_labels,
        tidy_label %in% input$startGovernanceFilter
      )$internal_label
    })
    subject_filter_choices <- reactive({
      filter(
        subject_broad_labels,
        tidy_label %in% input$startSubjectFilter
      )$internal_label
    })
    specific_subject_filter_choices <- reactive({
      filter(
        subject_full_labels,
        tidy_label %in% input$startSubjectSpecificFilter
      )$internal_label
    })
    region_filter_choices <- reactive({
      filter(
        country_region_labels,
        tidy_label %in% input$startRegionFilter
      )$internal_label
    })
    accreditation_filter_choices <- reactive({
      filter(
        accreditation_labels,
        tidy_label %in% input$startAccreditationFilter
      )$internal_label
    })
    initial_museum_filters <- reactive({
      list(
        input$startSizeFilter,
        input$startGovernanceFilter,
        input$startSubjectFilter,
        input$startSubjectSpecificFilter,
        input$startRegionFilter,
        input$startAccreditationFilter
      )
    })

    actor_choices_table <- reactive({
      get_actor_choices(grouping_field())
    })
    sequence_end <- reactive({
      filter(
        actor_choices_table(),
        label %in% input$sequenceEnd
      )$to
    })
    sequence_passes_through <- reactive({
      filter(
        actor_choices_table(),
        label %in% input$sequencePassesThrough
      )$to
    })

    observeEvent(transaction_type_filter(), {
      event_type_choices <- data.frame(type_name=c())
      core_event_types <- event_types |>
        filter(is_core_category)
      if ("Change of ownership" %in% transaction_type_filter()) {
        event_type_choices <- rbind(
          event_type_choices,
          select(filter(core_event_types, change_of_ownership), type_name)
        )
      }
      if ("Change of custody" %in% transaction_type_filter()) {
        event_type_choices <- rbind(
          event_type_choices,
          select(filter(core_event_types, change_of_custody), type_name)
        )
      }
      if ("End of existence" %in% transaction_type_filter()) {
        event_type_choices <- rbind(
          event_type_choices,
          select(filter(core_event_types, end_of_existence), type_name)
        )
      }
      event_type_choices <- event_type_choices |>
        distinct()
      updatePickerInput(
        session=session,
        inputId="eventTypeFilter",
        choices=event_type_choices$type_name,
        selected=event_type_choices$type_name
      )
    })

    observeEvent(subject_filter_choices(), {
      freezeReactiveValue(input, "startSubjectSpecificFilter")
      specific_subjects <- subject_full_labels |>
        filter(subject_broad %in% subject_filter_choices())
      updatePickerInput(
        session=session,
        inputId="startSubjectSpecificFilter",
        choices=specific_subjects$tidy_label,
        selected=specific_subjects$tidy_label,
      )
    })

    initial_museum_ids <- reactive({
      sapply(input$initialMuseum, function(text) sub(".*\\(([^()]*)\\)$", "\\1", text))
    })

    observeEvent(initial_museum_filters(), {
      freezeReactiveValue(input, "initialMuseum")
      museums_list <- dispersal_events |>
        filter(
          initial_museum_size %in% size_filter_choices(),
          initial_museum_governance %in% governance_filter_choices() | initial_museum_governance_broad %in% governance_filter_choices(),
          initial_museum_subject_matter_broad %in% subject_filter_choices(),
          initial_museum_subject_matter %in% specific_subject_filter_choices(),
          initial_museum_region %in% region_filter_choices() | initial_museum_country %in% region_filter_choices(),
          initial_museum_accreditation %in% accreditation_filter_choices()
        ) |>
        mutate(
          museum_id=initial_museum_id,
          name=paste0(initial_museum_name, " (", initial_museum_id, ")")
        ) |>
        arrange(name) |>
        select(name, museum_id, initial_museum_size) |>
        distinct()
      updateVirtualSelect(
        session=session,
        inputId="initialMuseum",
        choices=museums_list$name,
        selected=museums_list$name
      )
    })

    observeEvent(grouping_field(), {
      grouping_label <- list(
        "Actor Sector"="sector",
        "Actor Type (Core Categories)"="core type",
        "Actor Type (Most General)"="most general type",
        "Actor Type (Most Specific)"="most specific type"
      )[grouping_field()]
      updatePickerInput(
        session=session,
        inputId="sequenceEnd",
        label=paste0("(", grouping_label, ")"),
        choices=actor_choices_table()$label,
        selected=actor_choices_table()$label
      )
      updatePickerInput(
        session=session,
        inputId="sequencePassesThrough",
        label=paste0("(", grouping_label, ")"),
        choices=actor_choices_table()$label,
        selected=actor_choices_table()$label
      )
    })

    all_sequences <- reactive({all_sequence_data(transaction_type_filter())})
    filtered_sequences <- reactive({
      filtered_data <- filtered_sequence_data(
        all_sequences(),
        input$grouping,
        event_type_filter(),
        event_type_uncertainty_filter(),
        collection_status_filter(),
        initial_museum_ids(),
        sequence_end(),
        sequence_passes_through(),
        stepsOrFirstLast()
      )
    })
  })
}

get_actor_choices <- function(grouping_dimension) {
  grouping_dimension <- list(
    "Actor Sector"="sector",
    "Actor Type (Core Categories)"="core_type",
    "Actor Type (Most General)"="general_type",
    "Actor Type (Most Specific)"="type"
  )[grouping_dimension]
  recipient_grouping_dimension <- paste0("recipient_", grouping_dimension)
  choices_table <- dispersal_events |>
    group_by(recipient_governance_broad, .data[[recipient_grouping_dimension]]) |>
    summarize(
      to=paste(recipient_governance_broad, .data[[recipient_grouping_dimension]], sep="@"),
      label=ifelse(
        !is.na(recipient_governance_broad),
        paste(recipient_governance_broad, "museum"),
        .data[[recipient_grouping_dimension]]
      )
    ) |>
    ungroup() |>
    select(to, label) |>
    distinct()
  choices_table
}

grouping_dimension_and_governance_to_sector <- function(governance, grouping_dimension) {
  ifelse(
    governance %in% names(governance_to_sector),
    governance_to_sector[governance],
    grouping_dimension
  )
}

all_sequence_data <- function(show_transaction_types) {
  # find which events in the sequences to show
  show_ownership_changes <- "Change of ownership" %in% show_transaction_types
  show_custody_changes <- "Change of custody" %in% show_transaction_types
  show_ends_of_existence <- "End of existence" %in% show_transaction_types
  sequential_events <- dispersal_events |>
    mutate(
      show_event=show_ownership_changes & event_is_change_of_ownership |
        show_custody_changes & event_is_change_of_custody |
        show_ends_of_existence & event_is_end_of_existence
    )
}

filtered_sequence_data <- function(
    sequential_events,
    grouping_dimension,
    event_type_filter,
    event_type_uncertainty_filter,
    collection_status_filter,
    initial_museum_ids,
    show_ending_points,
    show_passes_through,
    steps_or_first_last
) {

  grouping_dimension <- list(
    "Actor Sector"="sector",
    "Actor Type (Core Categories)"="core_type",
    "Actor Type (Most General)"="general_type",
    "Actor Type (Most Specific)"="type"
  )[grouping_dimension]
  initial_museum_grouping_dimension <- paste0("initial_museum_", grouping_dimension)
  sender_grouping_dimension <- paste0("sender_", grouping_dimension)
  recipient_grouping_dimension <- paste0("recipient_", grouping_dimension)
  final_filtering_dimension <- paste0("final_", grouping_dimension)

  # find each event's previous event in the chain of shown events
  sequential_events <- sequential_events |>
    filter(initial_museum_id %in% initial_museum_ids) |>
    rowwise() |>
    mutate(
      previous_shown_event= {
        prev_event <- previous_event_id
        while (!is.na(prev_event)) {
          prev_row <- sequential_events |>
            filter(event_id==prev_event)
          if (nrow(prev_row) == 0) {
            prev_event <- NA
            break
          }
          if (prev_row$show_event) {
            prev_event <- prev_row$event_id
            break
          } else {
            prev_event <- prev_row$previous_event_id
          }
        }
        prev_event
      }
    ) |>
    ungroup() |>
    filter(show_event) |>
    arrange(original_collection_id, collection_id, event_stage_in_path) |>
    group_by(original_collection_id) |>
    mutate(
      event_stage_in_path=dense_rank(event_stage_in_path),
      sender_position=event_stage_in_path,
      recipient_position=event_stage_in_path+1,
    ) |>
    ungroup()

  # add details of the previous event to each event and determine the sender
  sequential_events <- sequential_events |>
    left_join(
      sequential_events |>
        mutate(
          previous_shown_event=event_id,
          from_name=recipient_name,
          from_type=recipient_type,
          from_core_type=recipient_core_type,
          from_general_type=recipient_general_type,
          from_sector=recipient_sector,
          from_governance=recipient_governance,
          from_governance_broad=recipient_governance_broad,
          from_town=recipient_town
        ) |>
        select(
          previous_shown_event,
          from_name,
          from_type,
          from_core_type,
          from_general_type,
          from_sector,
          from_governance,
          from_governance_broad,
          from_town
        ),
      by=c("previous_shown_event")
    ) |>
    mutate(
      sender_name=ifelse(event_stage_in_path==1, initial_museum_name, from_name),
      sender_type=ifelse(event_stage_in_path==1, initial_museum_type, from_type),
      sender_core_type=ifelse(event_stage_in_path==1, initial_museum_core_type, from_core_type),
      sender_general_type=ifelse(event_stage_in_path==1, initial_museum_general_type, from_general_type),
      sender_sector=ifelse(event_stage_in_path==1, initial_museum_sector, from_sector),
      sender_governance=ifelse(event_stage_in_path==1, initial_museum_governance, from_governance),
      sender_governance_broad=ifelse(event_stage_in_path==1, initial_museum_governance_broad, from_governance_broad),
      sender_town=ifelse(event_stage_in_path==1, initial_museum_town, from_town),
      from=ifelse(
        event_stage_in_path==1,
        paste(initial_museum_governance_broad, .data[[initial_museum_grouping_dimension]], 1, sep="@"),
        paste(sender_governance_broad, .data[[sender_grouping_dimension]], sender_position, sep="@")
      ),
      to=paste(recipient_governance_broad, .data[[recipient_grouping_dimension]], recipient_position, sep="@"),
    )

  # find end points of events 
  events_destinations <- sequential_events |>
    group_by(event_id, ancestor_events) |>
    filter(recipient_position==max(recipient_position)) |>
    summarize(
      destination=paste(recipient_governance_broad, .data[[recipient_grouping_dimension]], sep="@")
    ) |>
    ungroup()
  ancestor_destinations <- events_destinations |>
    mutate(
      ancestor_events = str_remove_all(ancestor_events, "\\[|\\]"),
      ancestor_events = strsplit(as.character(ancestor_events), ", ")
    ) |>
    unnest(ancestor_events) |>
    mutate(
      ancestor_events = str_remove_all(ancestor_events, "^'|'$")
    ) |>
    select(event_id=ancestor_events, destination)
  events_destinations <- rbind(
    events_destinations |> select(event_id, destination),
    ancestor_destinations
  )

  # find all recipients before event
  events_recipients <- sequential_events |>
    mutate(
      recipient=paste(recipient_governance_broad, .data[[recipient_grouping_dimension]], sep="@")
    )
  ancestor_recipients <- events_recipients |>
    mutate(
      ancestor_events = str_remove_all(ancestor_events, "\\[|\\]"),
      ancestor_events = strsplit(as.character(ancestor_events), ", ")
    ) |>
    unnest(ancestor_events) |>
    mutate(
      ancestor_events = str_remove_all(ancestor_events, "^'|'$")
    ) |>
    select(event_id=ancestor_events, recipient)
  events_recipients <- rbind(
    events_recipients |> select(event_id, recipient),
    ancestor_recipients
  )

  # remove events that don't lead to end points and filter by collection status and event type
  events_with_filtered_destinations <- events_destinations |>
    filter(destination %in% show_ending_points)
  events_with_filtered_recipients <- events_recipients |>
    filter(recipient %in% show_passes_through)

  sequential_events <- sequential_events |>
    filter(
      collection_status %in% collection_status_filter,
      event_id %in% events_with_filtered_destinations$event_id,
      event_id %in% events_with_filtered_recipients$event_id,
      event_core_type %in% c(event_type_filter),
      event_type_uncertainty %in% c(event_type_uncertainty_filter),
    )

  if (steps_or_first_last == "First and last actors") {
    sequential_events <- sequential_events |>
      group_by(collection_id) |>
      filter(recipient_position==max(recipient_position)) |>
      mutate(
        sender_id=initial_museum_id,
        sender_name=initial_museum_name,
        sender_type=initial_museum_type,
        sender_core_type=initial_museum_core_type,
        sender_general_type=initial_museum_general_type,
        sender_sector=initial_museum_sector,
        sender_size=initial_museum_size,
        sender_governance=initial_museum_governance,
        sender_governance_broad=initial_museum_governance_broad,
        sender_subject_matter=initial_museum_subject_matter,
        sender_subject_matter_broad=initial_museum_subject_matter_broad,
        sender_accreditation=initial_museum_accreditation,
        sender_town=initial_museum_town,
        sender_position=1,
        sender_quantity="1",
        recipient_position=2,
        from=paste(initial_museum_governance_broad, .data[[initial_museum_grouping_dimension]], sender_position, sep="@"),
        to=paste(recipient_governance_broad, .data[[recipient_grouping_dimension]], recipient_position, sep="@")
      ) |>
      ungroup()
    }
  sequential_events    
}
