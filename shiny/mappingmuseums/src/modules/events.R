sequences_table_choices <- c(
  "event_stage_in_path",
  "event_date",
  "event_date_from",
  "event_date_to",
  "collection_id",
  "collection_status",
  "collection_description",
  "collection_types",
  "collection_size",
  "initial_museum_governance",
  "initial_museum_size",
  "initial_museum_subject_matter",
  "initial_museum_subject_matter_broad",
  "initial_museum_region",
  "sender_name",
  "event_type",
  "event_core_type",
  "event_type_uncertainty",
  "recipient_name",
  "event_is_change_of_custody",
  "event_is_change_of_ownership",
  "sender_position",
  "sender_quantity",
  "sender_type",
  "sender_sector",
  "sender_governance",
  "sender_size",
  "sender_subject_matter_broad",
  "sender_accreditation",
  "sender_region",
  "sender_town",
  "recipient_position",
  "recipient_quantity",
  "recipient_type",
  "recipient_sector",
  "recipient_governance",
  "recipient_size",
  "recipient_subject_matter_broad",
  "recipient_accreditation",
  "recipient_region",
  "recipient_town"
)

eventsUI <- function(id) {
  fluidPage(
    fluidRow(
      p("What happens to museum collections?"),
      p("This tab introduces event types and the types of actor and collection involved in them."),
    ),
    fluidRow(
      h3("Event Types"),
      p("The types of event involving museum collections are shown below."),
      p("Events can be changes of ownership, changes of custody, both, or neither. Alternatively they can mark the end of a collection's or object's existence. The hierarchy colours each event type according to the default way in which it should be understood. Individual events can override this default. For example, some sales do not result in a change of custody."),
      plotOutput(NS(id, "eventTypes"), width="80%", height="900px")
    ),
    hr(style=hr_style),
    fluidRow(
      h3("Glossary of Event Types"),
      DTOutput(NS(id, "eventTypesGlossary"))
    ),
    hr(style=hr_style),

    fluidRow(
      h3("Events and Participants by Museum Type"),
      sidebarLayout(
        sidebarPanel(
          selectInput(
            NS(id, "museumsFilterField"),
            label="Filter museums by:",
            choices=c("No filter", field_names$name),
            selected="Governance"
          ),
          uiOutput(NS(id, "subjectChoices")),
          pickerInput(
            NS(id, "museumsChoicesField"),
            "Show Only:", 
            choices=NULL,
            selected=NULL,
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          ),
          selectInput(
            NS(id, "museumVsEventDimension2"),
            label="Event/recipient:",
            choices=c("First event", "Last known event", "First recipient", "Last known recipient"),
            selected="First event"
          ),
          selectInput(
            NS(id, "eventParticipantGranularity"),
            label="Event/recipient granularity:",
            choices=c("Most general", "Core categories", "Most specific"),
            selected="Core categories"
          ),
        ),
        mainPanel(
          plotlyOutput(NS(id, "museumVsEventMatrix"), height=900),
          radioButtons(
            inputId = NS(id, "museumVsEventCountOrPercentage"),
            label = "",
            choices = list(
              "Show number of events" = "count",
              "Show percentage of events" = "percentage",
              "Show rowwise percentage of museums" = "percentage_y",
              "Show columnwise percentage of events" = "percentage_x"
            )
          )
        )
      )
    ),
    hr(style=hr_style),

    fluidRow(
      h3("Events and Participants"),
      p("Within our data model, events can have senders and recipients (both actors), and can involve collections or objects."),
      p("The collections or objects involved in events can be labelled with multiple types. When viewing pairings of events and collections/objects, some events are therefore counted more than once. The table only shows collection/object types which occur in more than 3 events."),
      p("This chart shows the frequency with which different types of event involve different types of participant."),
      p("Select participant type to change the entities on the y-axis and hierarchy level to change which level in the actor types hierarchy is used."),
    ),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          NS(id, "participantType"),
          label="Participant type:",
          choices=c("Sender", "Recipient", "Collection/Object"),
          selected="Sender"
        ),
        selectInput(
          NS(id, "participantGranularity"),
          label="Actor Hierarchy level:",
          choices=c("Most general", "Core categories", "Most specific"),
          selected="Core categories"
        ),
        selectInput(
          NS(id, "eventGranularity"),
          label="Event Hierarchy level:",
          choices=c("Core categories", "Most specific"),
          selected="Core categories"
        ),
        pickerInput(
          NS(id, "eventStageInPath"),
          "Stage in path of event",
          choices=c(1,2,3,4,5,6,7),
          selected=c(1),
          options=pickerOptions(
            actionsBox=TRUE,
            size=10,
            selectedTextFormat="count > 7"
          ),
          multiple=TRUE
        ),
        p("Show only events where the collection involved originates in a museum of type:"),
        pickerInput(
            NS(id, "sizeFilter"), 
            "Size:", 
            choices=tidy_labels_size,
            selected=tidy_labels_size,
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          ),
          pickerInput(
            NS(id, "governanceFilter"), 
            "Governance:", 
            choices=tidy_labels_governance,
            selected=tidy_labels_governance,
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          ),
          pickerInput(
            NS(id, "accreditationFilter"), 
            "Accreditation:", 
            choices=tidy_labels_accreditation,
            selected=tidy_labels_accreditation,
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          ),
          pickerInput(
            NS(id, "subjectFilter"), 
            "Subject Matter:", 
            choices=tidy_labels_subject,
            selected=tidy_labels_subject,
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          ),
          pickerInput(
            NS(id, "regionFilter"), 
            "Country/Region:", 
            choices=tidy_labels_country_region,
            selected=tidy_labels_country_region,
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          ),
      ),
      mainPanel(
        plotlyOutput(NS(id, "eventsMatrix"), height=1200)
      )
    ),
    hr(style=hr_style),

    fluidRow(
      pickerInput(
        NS(id, "senderFilter"),
        label="sender: ",
        choices=NULL,
        selected=NULL,
        options=pickerOptions(
          actionsBox=TRUE, 
          size=10,
          selectedTextFormat="count > 3"
        ), 
        multiple=TRUE
      ),
      pickerInput(
        NS(id, "eventFilter"),
        label="event: ",
        choices=NULL,
        selected=NULL,
        options=pickerOptions(
          actionsBox=TRUE, 
          size=10,
          selectedTextFormat="count > 3"
        ), 
        multiple=TRUE
      ),
      pickerInput(
        NS(id, "recipientFilter"),
        label="recipient: ",
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
    pickerInput(
      NS(id, "tableSelect"),
      label="show columns:",
      choices=sequences_table_choices,
      selected=sequences_table_choices,
      options = pickerOptions(
        actionsBox = TRUE, 
        size = 10,
        selectedTextFormat = "count > 3"
      ), 
      multiple = TRUE
    ),
    downloadButton(NS(id, "downloadEventsTable"), label="Download table as CSV"),
    DTOutput(NS(id, "eventsTable"), height=900)
  )
}

eventsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$eventTypes <- renderPlot({
      event_types_hierarchy()
    }, height=900)

    output$eventTypesGlossary <- renderDT({
      event_types |>
        select(
          `Type name`=type_name,
          `Sub-type of`=sub_type_of,
          `Definition`=definition,
          `Core category?`=is_core_category,
          `Usually a change of ownership?`=change_of_ownership,
          `Usually a change of custody?`=change_of_custody,
          `Usually an end of existence?`=end_of_existence
        )
    })

    participant_type <- reactive({input$participantType})
    participant_granularity <- reactive({input$participantGranularity})
    event_granularity <- reactive({input$eventGranularity})
    event_stage_in_path <- reactive({input$eventStageInPath})
    size_filter <- reactive({
      names(tidy_labels_size)[match(input$sizeFilter, tidy_labels_size)]
    })
    governance_filter <- reactive({
      names(tidy_labels_governance)[match(input$governanceFilter, tidy_labels_governance)]
    })
    accreditation_filter <- reactive({
      names(tidy_labels_accreditation)[match(input$accreditationFilter, tidy_labels_accreditation)]
    })
    subject_filter <- reactive({
      names(tidy_labels_subject)[match(input$subjectFilter, tidy_labels_subject)]
    })
    region_filter <- reactive({
      names(tidy_labels_country_region)[match(input$regionFilter, tidy_labels_country_region)]
    })

    events_and_participants <- reactive({
      dispersal_events |>
        mutate(
          event_core_type=ifelse(is.na(event_core_type), event_type, event_core_type),
          sender_type=ifelse(is.na(sender_type), "N/A", sender_type),
          sender_core_type=ifelse(is.na(sender_core_type), sender_type, sender_core_type),
          sender_general_type=ifelse(is.na(sender_general_type), sender_core_type, sender_general_type),
          recipient_type=ifelse(is.na(recipient_type), "N/A", recipient_type),
          recipient_core_type=ifelse(is.na(recipient_core_type), recipient_type, recipient_core_type),
          recipient_general_type=ifelse(is.na(recipient_general_type), recipient_core_type, recipient_general_type)
        )
    })
    event_choices <- arrange(
      distinct(select(events_and_participants(), event_type)),
      event_type
    )$event_type
    updatePickerInput(
      inputId="eventFilter",
      choices=event_choices,
      selected=event_choices
    )
    observeEvent(participant_type(), {
      if (participant_type() == "Collection/Object") {
        shinyjs::disable("participantGranularity")
      } else {
        shinyjs::enable("participantGranularity")
      }
    })
    observeEvent(participant_granularity(), {
      req(participant_granularity)
      if (participant_granularity() == "Most general") {
        sender_choices <- arrange(
          distinct(select(events_and_participants(), sender_general_type)),
          sender_general_type
        )$sender_general_type
        updatePickerInput(
          inputId="senderFilter",
          choices=sender_choices,
          selected=sender_choices
        )
        recipient_choices <- arrange(
          distinct(select(events_and_participants(), recipient_general_type)),
          recipient_general_type
        )$recipient_general_type
        updatePickerInput(
          inputId="recipientFilter",
          choices=recipient_choices,
          selected=recipient_choices
        )
      } else if (participant_granularity() == "Core categories") {
        sender_choices <- arrange(
          distinct(select(events_and_participants(), sender_core_type)),
          sender_core_type
        )$sender_core_type
        updatePickerInput(
          inputId="senderFilter",
          choices=sender_choices,
          selected=sender_choices
        )
        recipient_choices <- arrange(
          distinct(select(events_and_participants(), recipient_core_type)),
          recipient_core_type
        )$recipient_core_type
        updatePickerInput(
          inputId="recipientFilter",
          choices=recipient_choices,
          selected=recipient_choices
        )
      } else {
        sender_choices <- arrange(
          distinct(select(events_and_participants(), sender_type)),
          sender_type
        )$sender_type
        updatePickerInput(
          inputId="senderFilter",
          choices=sender_choices,
          selected=sender_choices
        )
        recipient_choices <- arrange(
          distinct(select(events_and_participants(), recipient_type)),
          recipient_type
        )$recipient_type
        updatePickerInput(
          inputId="recipientFilter",
          choices=recipient_choices,
          selected=recipient_choices
        )
      }
    })
    observeEvent(event_granularity(), {
      req(event_granularity)
      if (event_granularity() == "Core categories") {
        event_choices <- arrange(
          distinct(select(events_and_participants(), event_core_type)),
          event_core_type
        )$event_core_type
        updatePickerInput(
          inputId="eventFilter",
          choices=event_choices,
          selected=event_choices
        )
      } else {
        event_choices <- arrange(
          distinct(select(events_and_participants(), event_type)),
          event_type
        )$event_type
        updatePickerInput(
          inputId="eventFilter",
          choices=event_choices,
          selected=event_choices
        )
      }
    })

    y_axis_type <- reactive({
      if (participant_type() == "Collection/Object") {
        return("collection")
      } else {
        return(tolower(participant_type()))
      }
    })
    y_axis_level <- reactive({
      if (y_axis_type() == "collection") {
        return("_type")
      }
      if (participant_granularity() == "Most general") {
        return("_general_type")
      } else if (participant_granularity() == "Core categories") {
        return("_core_type")
      } else {
        return("_type")
      }
    })
    event_column_name <- reactive({
      ifelse(
        event_granularity() == "Core categories",
        "event_core_type",
        "event_type"
      )
    })
    output$eventsMatrix <- renderPlotly({
      y_axis <- paste0(y_axis_type(), y_axis_level())
      title <- paste0("Frequency of ", participant_type(), " / Event Pairs")
      events_vs_participants <- events_and_participants() |>
        filter((event_stage_in_path+1) %in% event_stage_in_path()) |>
        filter(initial_museum_size %in% size_filter()) |>
        filter(initial_museum_governance %in% governance_filter() | initial_museum_governance_broad %in% governance_filter()) |>
        filter(initial_museum_accreditation %in% accreditation_filter()) |>
        filter(initial_museum_subject_matter_broad %in% subject_filter()) |>
        filter(initial_museum_region %in% region_filter() | initial_museum_country %in% region_filter())
      if (participant_type() == "Collection/Object") {
        y_axis_label <- paste0(participant_type(), " Type (Wikidata Types)")
        events_vs_participants <- events_vs_participants |>
          mutate(
            event_type = .data[[event_column_name()]],
            participant_type = str_remove_all(collection_types, "\\[|\\]"),
            participant_type = strsplit(as.character(participant_type), ", ")
          ) |>
          unnest(participant_type) |>
          mutate(
            participant_type = str_remove_all(participant_type, "^'|'$")
          ) |>
          group_by(event_type, participant_type) |>
          summarize(count=n()) |>
          ungroup() |>
          group_by(participant_type) |>
          filter(sum(count) > 3) |>
          ungroup()
      } else {
        y_axis_label <- paste0(participant_type(), " Type ", "(", participant_granularity(), ")")
        events_vs_participants <- events_vs_participants |>
          mutate(
            event_type = .data[[event_column_name()]],
            participant_type = ifelse(
              is.na(.data[[paste0(tolower(participant_type()), "_governance_broad")]]),
              .data[[y_axis]],
              paste0(.data[[y_axis]], " (", .data[[paste0(tolower(participant_type()), "_governance_broad")]], ")")
            )
          ) |>
          group_by(event_type, participant_type) |>
          summarize(count=n()) |>
          ungroup()
      }
      unique_event_types <- arrange(distinct(select(events_vs_participants, event_type)), event_type)$event_type
      unique_participant_types <- arrange(distinct(select(events_vs_participants, participant_type)), participant_type)$participant_type
      event_totals <- events_vs_participants |>
        group_by(event_type) |>
        summarize(count=sum(count)) |>
        ungroup() |>
        mutate(participant_type="All participants") |>
        select(event_type, participant_type, count)
      participant_totals <- events_vs_participants |>
        group_by(participant_type) |>
        summarize(count=sum(count)) |>
        ungroup() |>
        mutate(event_type="All events") |>
        select(event_type, participant_type, count)
      total_total <- events_vs_participants |>
        summarize(count=sum(count)) |>
        mutate(
          participant_type="All participants",
          event_type="All events"
        ) |>
        select(event_type, participant_type, count)
      events_vs_participants <- events_vs_participants |>
        rbind(event_totals) |>
        rbind(participant_totals) |>
        rbind(total_total) |>
        mutate(
          event_type=factor(event_type, levels=c("All events", unique_event_types)),
          participant_type=factor(participant_type, levels=c("All participants", unique_participant_types))
        )
      ggplot(events_vs_participants, aes(x=event_type, y=participant_type, fill=count)) +
        geom_tile(show.legend=FALSE) +
        geom_text(aes(label=count), size=5) +
        geom_hline(yintercept=1.5) +
        geom_vline(xintercept=1.5) +
        scale_fill_continuous(low="white", high="purple") +
        labs(
          title=title,
          x="Event Type",
          y=y_axis_label
        ) +
        standard_bars_theme +
        theme(
          axis.text.x=element_text(angle=45, hjust=1, vjust=1)
        )
    })

    show_sender_types <- reactive({input$senderFilter})
    show_event_types <- reactive({input$eventFilter})
    show_recipient_types <- reactive({input$recipientFilter})
    filtered_events_and_participants <- reactive({
      event_column_name <- event_column_name()
      sender_column_name <- paste0("sender", y_axis_level())
      recipient_column_name <- paste0("recipient", y_axis_level())
      events_and_participants() |>
        filter((event_stage_in_path+1) %in% event_stage_in_path()) |>
        filter(initial_museum_size %in% size_filter()) |>
        filter(initial_museum_governance %in% governance_filter() | initial_museum_governance_broad %in% governance_filter()) |>
        filter(initial_museum_accreditation %in% accreditation_filter()) |>
        filter(initial_museum_subject_matter_broad %in% subject_filter()) |>
        filter(initial_museum_region %in% region_filter() | initial_museum_country %in% region_filter()) |>
        filter(.data[[event_column_name]] %in% show_event_types()) |>
        filter(.data[[sender_column_name]] %in% show_sender_types()) |>
        filter(.data[[recipient_column_name]] %in% show_recipient_types()) |>
        mutate(
          sender_position=event_stage_in_path,
          recipient_position=event_stage_in_path + 1
        )
    })

    filter_field <- reactive({
      if (filter_field_1() == "main_subject" && specific_subject_filter() != "All") {
        return("subject_matter")
      }
      return(filter_field_1())
    })
    filter_field_1 <- reactive({
      req(input$museumsFilterField)
      if (input$museumsFilterField == "No filter") {
        return("No filter")
      }
      return(
        filter(field_names, name==input$museumsFilterField)$value[1]
      )
    })
    filter_field_label <- reactive({input$museumsFilterField})
    choices <- reactive({
      req(input$museumsChoicesField)
      if (filter_field() != "subject_matter") {
        return(
          filter(
            filter_field_choices,
            label %in% input$museumsChoicesField
            & filter_field_1() %in% field
          )$value
        )
      } else {
        return (
          filter(
            subject_filter_field_choices,
            subject_matter %in% input$museumsChoicesField
          )$subject_matter
        )
      }
    })
    specific_subject_filter <- reactive({
      req(input$subjectFilterField)
      if (input$subjectFilterField == "All") {
        return("All")
      } else {
        return(
          filter(
            filter_field_choices,
            label == input$subjectFilterField
          )$value
        )
      }
    })
    observeEvent(filter_field_1(), {
      freezeReactiveValue(input, "museumsChoicesField")
      choices <- filter_field_choices |> filter(field==filter_field_1())
      selected_choices <- choices |> filter(!value %in% by_default_ignore)
      if (filter_field_1() == "main_subject") {
        output$subjectChoices <- renderUI({
          selectInput(
            NS(id, "subjectFilterField"),
            label="Filter subject:",
            choices=c("All", choices$label),
            selected="All"
          )
        })
      } else {
        output$subjectChoices <- renderUI({})
      }
      updatePickerInput(
        inputId="museumsChoicesField",
        choices=choices$label,
        selected=selected_choices$label
      ) 
    })
    observeEvent(specific_subject_filter(), {
      if (specific_subject_filter() == "All") {
        choices <- filter_field_choices |> filter(field==filter_field_1())
        updatePickerInput(
          inputId="museumsChoicesField",
          choices=choices$label,
          selected=choices$label
        )
      } else {
        choices <- subject_filter_field_choices |> filter(main_subject==specific_subject_filter())
        updatePickerInput(
          inputId="museumsChoicesField",
          choices=choices$subject_matter,
          selected=choices$subject_matter
        )
      }
    })
    museum_vs_event_dimension_2 <- reactive({input$museumVsEventDimension2})
    event_participant_granularity <- reactive({input$eventParticipantGranularity})
    museum_vs_event_count_or_percentage <- reactive({input$museumVsEventCountOrPercentage})
    output$museumVsEventMatrix <- renderPlotly({
      museum_vs_event_matrix(
        dispersal_events,
        museum_vs_event_count_or_percentage(),
        filter_field(),
        filter_field_label(),
        choices(),
        museum_vs_event_dimension_2(),
        event_participant_granularity()
      )
    })
    
    selected_columns <- reactive({input$tableSelect})
    output$downloadEventsTable <- downloadHandler(
      filename = function() {
        paste('dispersal-events-data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(filtered_events_and_participants(), con)
      },
      contentType = "text/csv"
    )
    output$eventsTable <- renderDT({
      filtered_events_and_participants() |>
        select(all_of(selected_columns()))
    }, options=list(pageLength=100))
  })
}

event_types_hierarchy <- function() {
  # add dummy types to use as spaces between groups
  counter <- 1
  types_with_sub_types <- event_types |>
    filter(!is.na(sub_type_of)) |>
    select(type_name=sub_type_of) |>
    distinct()
  for (i in 1:nrow(types_with_sub_types)) {
    new_row_1 <- data.frame(
      type_name = as.character(counter),
      sub_type_of = types_with_sub_types$type_name[i],
      is_core_category = FALSE,
      change_of_ownership = FALSE,
      change_of_custody = FALSE,
      end_of_existence = FALSE,
      definition = "dummy"
    )
    new_row_2 <- data.frame(
      type_name = paste("z", as.character(counter)),
      sub_type_of = types_with_sub_types$type_name[i],
      is_core_category = FALSE,
      change_of_ownership = FALSE,
      change_of_custody = FALSE,
      end_of_existence = FALSE,
      definition = "dummy"
    )
    counter <- counter + 1
    event_types <- event_types |>
      rbind(new_row_1) |>
      rbind(new_row_2)
  }
 
  ownership_transfers <- event_types |>
    filter(change_of_ownership == "TRUE") |>
    select(type_name)
  custody_transfers <- event_types |>
   filter(change_of_custody == "TRUE") |>
   select(type_name)
  ends_of_existence <- event_types |>
   filter(end_of_existence == "TRUE") |>
   select(type_name)
  core_types <- event_types |>
    filter(is_core_category == "TRUE") |>
    select(type_name)
  dummy_types <- event_types |>
    filter(definition=="dummy") |>
    select(type_name)
 
  event_edges <- event_types |>
    arrange(sub_type_of, type_name) |>
    filter(sub_type_of != "") |>
    select(
      from=sub_type_of,
      to=type_name
    ) |>
    mutate(is_to_dummy = to %in% dummy_types$type_name)
  
  graph <- graph_from_data_frame(event_edges)
  V(graph)$distance_to_root <- distances(graph, v=V(graph), to=which(V(graph)$name == "event"))
  max_distance <- max(V(graph)$distance_to_root)
  parent_nodes <- sapply(V(graph), function(v) {
    parents <- neighbors(graph, v, mode = "in")
    if (length(parents) == 0) {
      return(NA) # Root node has no parent
    } else {
      return(V(graph)$name[parents[1]]) # Assuming one parent for a tree structure
    }
  })

  layout <- create_layout(graph, layout="dendrogram", circular=FALSE)
  layout$is_core_category <- layout$name %in% core_types$type_name
  # arrange nodes according to distance from root and move all core categories to the same y
  layout$y <- ifelse(layout$is_core_category, -2, layout$distance_to_root - max_distance)
  layout$parent <- parent_nodes[layout$name]
  layout$parent_y <- sapply(1:nrow(layout), function(i) {
    parent_name <- layout$parent[i]
    if (is.na(parent_name)) {
      return(NA) # Root node has no parent, so no parent y-coordinate
    } else {
      return(layout$y[layout$name == parent_name]) # Get the y-coordinate of the parent node
    }
  })
  layout$y <- ifelse(
    !is.na(layout$parent_y) & layout$y == layout$parent_y,
    layout$y + 1,
    layout$y
  )
  # add transfer types to nodes
  layout$transfer_type <- ifelse(
    layout$name %in% ownership_transfers$type_name & layout$name %in% custody_transfers$type_name,
    "Change of ownership and custody",
    ifelse(
      layout$name %in% ownership_transfers$type_name,
      "Change of ownership",
      ifelse(
        layout$name %in% custody_transfers$type_name,
        "Change of custody",
        ifelse(
          layout$name %in% ends_of_existence$type_name,
          "End of existence",
          "Non-transfer event"
        )
      )
    )
  )
  layout$is_dummy <- layout$name %in% dummy_types$type_name

  ggraph(layout) + 
    geom_edge_diagonal(
      aes(colour = ifelse(is_to_dummy, "dummy", "normal")),
      show.legend=FALSE
    ) +
    geom_node_point(
      data=layout |> filter(!is_dummy),
      aes(fill=transfer_type, colour=is_core_category),
      shape=21,
      size=4,
      stroke=2
    ) +
    geom_node_text(
      data=layout |> filter(!is_dummy),
      aes(label=name),
      size=5,
      angle=0,
      vjust="center",
      hjust="left",
      nudge_y=0.05
    ) +
    coord_flip() +
    scale_y_continuous(limits=c(-max_distance, 1)) +
    scale_fill_manual(
      values=c(
        "Change of ownership and custody"="#785EF0",
        "Change of ownership"="#648FFF",
        "Change of custody"="#FE6100",
        "End of existence"="#000000",
        "Non-transfer event"="lightgrey"
      ), 
      name="",
      na.value="black"
    ) +
    scale_colour_manual(
      values=c("TRUE"="black", "FALSE"="lightgrey"),
      labels=c("TRUE"="core categories", "FALSE"="non-core categories"),
      name=""
    ) +
    scale_edge_colour_manual(
      values=c("dummy"="white", "normal"="lightgrey")
    ) +
    labs(
      title="Hierarchy of event types involved in dispersal of museum collections"
    ) +
    type_hierarchy_theme
}

museum_vs_event_matrix <- function(events, count_or_percentage, filter_field, y_label, choices, dimension_2, dimension_2_granularity) {
  recipient <- dimension_2 %in% c("First recipient", "Last known recipient")
  if (dimension_2 == "First event") {
    events <- events |> filter(event_stage_in_path == 0)
  } else if (dimension_2 == "First recipient") {
    events <- events |>
      filter(recipient_id != "") |>
      group_by(collection_id) |>
      filter(event_stage_in_path == min(event_stage_in_path)) |>
      ungroup() |>
      mutate(
        recipient_type = ifelse(recipient_type=="actor", "unknown", recipient_type),
        recipient_core_type = ifelse(is.na(recipient_core_type), recipient_type, recipient_core_type),
        recipient_general_type = ifelse(is.na(recipient_general_type), recipient_core_type, recipient_general_type)
      )
  } else if (dimension_2 == "Last known event") {
    events <- events |>
      group_by(collection_id) |>
      filter(event_stage_in_path == max(event_stage_in_path)) |>
      ungroup()
  } else if (dimension_2 == "Last known recipient") {
    events <- events |>
      filter(!is.na(recipient_id)) |>
      group_by(collection_id) |>
      filter(event_stage_in_path == max(event_stage_in_path)) |>
      ungroup() |>
      mutate(
        recipient_type = ifelse(recipient_type=="actor", "unknown", recipient_type),
        recipient_core_type = ifelse(is.na(recipient_core_type), recipient_type, recipient_core_type),
        recipient_general_type = ifelse(is.na(recipient_general_type), recipient_core_type, recipient_general_type)
      )
  }
  if (recipient && dimension_2_granularity == "Most general") {
    x_axis <- "recipient_general_type"
    x_axis_label <- "Recipient Type (most general)"
  } else if (recipient && dimension_2_granularity == "Core categories") {
    x_axis <- "recipient_core_type"
    x_axis_label <- "Recipient Type (core category)"
  } else if (recipient && dimension_2_granularity == "Most specific") {
    x_axis <- "recipient_type"
    x_axis_label <- "Recipient Type (most specific)"
  } else if (!recipient && dimension_2_granularity == "Most general") {
    x_axis <- "event_core_type"
    x_axis_label <- "Event Type (core category)"
  } else if (!recipient && dimension_2_granularity == "Core categories") {
    x_axis <- "event_core_type"
    x_axis_label <- "Event Type (core category)"
  } else {
    x_axis <- "event_type"
    x_axis_label <- "Event Type (most specific)"
  }
  if (filter_field == "governance_main") {
    filter_field <- "governance_broad"
  } else if (filter_field == "main_subject") {
    filter_field <- "subject_matter_broad"
  }
  y_axis <- paste0("initial_museum_", filter_field)
  title <- paste(y_label, "of Initial Museum vs", dimension_2)
  number_of_museums_by_type <- events |>
    select(initial_museum_id, .data[[y_axis]]) |>
    distinct() |>
    group_by(.data[[y_axis]]) |>
    summarize(
      number_of_museums_with_type=n()
    ) |>
    ungroup()
  number_of_museums_by_event_type <- events |>
    select(initial_museum_id, .data[[x_axis]], .data[[y_axis]]) |>
    distinct() |>
    group_by(.data[[x_axis]], .data[[y_axis]]) |>
    summarize(
      number_of_museums_with_event_type=n()
    ) |>
    ungroup()
  events_summary <- events |>
    group_by(.data[[x_axis]], .data[[y_axis]]) |>
    summarize(
      count=n()
    ) |>
    ungroup() |>
    mutate(
      percentage=round(count / sum(count) * 100, 1)
    ) |>
    group_by(.data[[x_axis]]) |>
    mutate(
      percentage_x=round(count / sum(count) * 100, 1)
    ) |>
    ungroup() |>
    left_join(number_of_museums_by_type, by=y_axis) |>
    left_join(number_of_museums_by_event_type, by=c(x_axis, y_axis)) |>
    group_by(.data[[x_axis]], .data[[y_axis]]) |>
    mutate(
      percentage_y=round(number_of_museums_with_event_type / number_of_museums_with_type * 100, 1)
    ) |>
    ungroup() |>
    filter(.data[[y_axis]] %in% choices)
  ggplot(
    events_summary,
    aes(
      x=.data[[x_axis]],
      y=factor(.data[[y_axis]], museum_attribute_ordering)
    )
  ) +
    geom_tile(aes(fill=.data[[count_or_percentage]]), show.legend=FALSE) +
    geom_text(aes(label=.data[[count_or_percentage]]), size=5) +
    scale_y_discrete(labels=tidy_labels) +
    scale_fill_continuous(low="white", high="purple") +
    labs(
      title=title,
      y=y_label,
      x=x_axis_label
    ) +
    standard_bars_theme +
    theme(
      axis.text.x=element_text(angle=45, hjust=1, vjust=1)
    )
}
