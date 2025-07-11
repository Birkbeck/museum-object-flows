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
        plotlyOutput(NS(id, "eventsMatrix"), height=900)
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

    x_axis_type <- reactive({
      if (participant_type() == "Collection/Object") {
        return("collection")
      } else {
        return(tolower(participant_type()))
      }
    })
    x_axis_level <- reactive({
      if (x_axis_type() == "collection") {
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
      x_axis <- paste0(x_axis_type(), x_axis_level())
      title <- paste0("Frequency of Event / ", participant_type(), " Pairs")
      events_vs_participants <- events_and_participants() |>
        filter(initial_museum_size %in% size_filter()) |>
        filter(initial_museum_governance %in% governance_filter() | initial_museum_governance_broad %in% governance_filter()) |>
        filter(initial_museum_accreditation %in% accreditation_filter()) |>
        filter(initial_museum_subject_matter_broad %in% subject_filter()) |>
        filter(initial_museum_region %in% region_filter() | initial_museum_country %in% region_filter())
      if (participant_type() == "Collection/Object") {
        x_axis_label <- paste0(participant_type(), " Type (Wikidata Types)")
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
        x_axis_label <- paste0(participant_type(), " Type ", "(", participant_granularity(), ")")
        events_vs_participants <- events_vs_participants |>
          mutate(
            event_type = .data[[event_column_name()]],
            participant_type = ifelse(
              is.na(.data[[paste0(tolower(participant_type()), "_governance_broad")]]),
              .data[[x_axis]],
              paste0(.data[[x_axis]], " (", .data[[paste0(tolower(participant_type()), "_governance_broad")]], ")")
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
      ggplot(events_vs_participants, aes(y=event_type, x=participant_type, fill=count)) +
        geom_tile(show.legend=FALSE) +
        geom_text(aes(label=count)) +
        geom_hline(yintercept=1.5) +
        geom_vline(xintercept=1.5) +
        scale_fill_continuous(low="white", high="purple") +
        labs(
          title=title,
          y="Event Type",
          x=x_axis_label
        ) +
        theme_minimal() +
        theme(
          axis.text.x=element_text(angle=45, hjust=1, vjust=1)
        )
    })

    show_sender_types <- reactive({input$senderFilter})
    show_event_types <- reactive({input$eventFilter})
    show_recipient_types <- reactive({input$recipientFilter})
    filtered_events_and_participants <- reactive({
      event_column_name <- event_column_name()
      sender_column_name <- paste0("sender", x_axis_level())
      recipient_column_name <- paste0("recipient", x_axis_level())
      events_and_participants() |>
        filter(.data[[event_column_name]] %in% show_event_types()) |>
        filter(.data[[sender_column_name]] %in% show_sender_types()) |>
        filter(.data[[recipient_column_name]] %in% show_recipient_types()) |>
        select(
          initial_museum_id,
          initial_museum_name,
          sender_name,
          .data[[sender_column_name]],
          sender_governance,
          .data[[event_column_name]],
          recipient_name,
          recipient_governance,
          .data[[recipient_column_name]],
          collection_id,
          collection_description,
          collection_types
        )
    })
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
      filtered_events_and_participants()
    }, options=list(pageLength=100))
  })
}

event_types_hierarchy <- function() {
  ownership_transfers <- event_types |>
    clean_names() |>
    filter(change_of_ownership == "TRUE") |>
    select(type_name)
  custody_transfers <- event_types |>
   clean_names() |>
   filter(change_of_custody == "TRUE") |>
   select(type_name)
  ends_of_existence <- event_types |>
   clean_names() |>
   filter(end_of_existence == "TRUE") |>
   select(type_name)
  core_types <- event_types |>
    clean_names() |>
    filter(is_core_category == "TRUE") |>
    select(type_name)
  
  event_edges <- event_types |>
    filter(sub_type_of != "" & type_name != "closure") |>
    select(
      from=sub_type_of,
      to=type_name
    )
  
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

  ggraph(layout) + 
    geom_edge_diagonal(colour="lightgrey") +
    geom_node_point(
      aes(fill=transfer_type, colour=is_core_category),
      shape=21,
      size=4,
      stroke=2
    ) +
    geom_node_text(
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
    labs(
      title="Hierarchy of event types involved in dispersal of museum collections"
    ) +
    type_hierarchy_theme
}
