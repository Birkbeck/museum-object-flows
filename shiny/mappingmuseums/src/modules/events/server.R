source("src/modules/events/elements.R")

eventsServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$reset, {
      updateSelectInput(session=session, inputId="yAxis", selected="Sender type")
      updateSelectInput(session=session, inputId="xAxis", selected="Event type")
      updateRadioButtons(session=session, inputId="stepsOrLast", selected="Stepwise events")
      updatePickerInput(
        session=session,
        inputId="stagesInPath",
        selected=c(1)
      )
      updateRadioButtons(session=session, inputId="countOrPercentage", selected="count")
      updateSelectInput(session=session, inputId="eventGrouping", selected="Core categories")
      updateSelectInput(session=session, inputId="actorGrouping", selected="Core categories")
      updateSelectInput(session=session, inputId="museumGrouping", selected="Governance")
      updatePickerInput(
        session=session, inputId="eventTypeFilter", selected=event_core_types$label
      )
      updatePickerInput(
        session=session, inputId="collectionTypeFilter", selected=collection_types$collection_type
      )
      updatePickerInput(
        session=session, inputId="collectionStatusFilter", selected=collection_status_labels$label,
      )
      updatePickerInput(
        session=session, inputId="governanceFilter", selected=governance_broad_labels$label
      )
      updatePickerInput(
        session=session, inputId="sizeFilter", selected=size_labels$label
      )
      updatePickerInput(
        session=session, inputId="subjectFilter", selected=subject_broad_labels$label
      )
      updatePickerInput(
        session=session, inputId="subjectSpecificFilter", selected=subject_labels$label
      )
      updatePickerInput(
        session=session, inputId="regionFilter", selected=region_labels$label
      )
      updatePickerInput(
        session=session, inputId="accreditationFilter", selected=accreditation_labels$label
      )
    })

    museum_grouping <- reactive({
      req(input$museumGrouping)
      switch(
        input$museumGrouping,
        "All" = "_all",
        "Size" = "_size",
        "Governance" = "_governance_broad",
        "Accreditation" = "_accreditation",
        "Subject Matter" = "_subject_broad",
        "Country/Region" = "_region",
        "Country" = "_country"
      )
    })

    actor_grouping <- reactive({
      req(input$actorGrouping)
      switch(
        input$actorGrouping,
        "Core categories" = "_core_type",
        "Most general" = "_general_type",
        "Most specific" = "_type"
      )
    })

    actor_and_museum_grouping <- reactive({
      list(
        actor_grouping(),
        museum_grouping()
      )
    })

    sender_grouping <- reactive({
      req(input$actorGrouping)
      switch(
        input$actorGrouping,
        "Core categories" = "sender_core_type",
        "Most general" = "sender_general_type",
        "Most specific" = "sender_type"
      )
    })

    recipient_grouping <- reactive({
      req(input$actorGrouping)
      switch(
        input$actorGrouping,
        "Core categories" = "recipient_core_type",
        "Most general" = "recipient_general_type",
        "Most specific" = "recipient_type"
      )
    })

    event_grouping <- reactive({
      req(input$eventGrouping)
      switch(
        input$eventGrouping,
        "Core categories" = "event_core_type",
        "Most general" = "event_core_type",
        "Most specific" = "event_type"
      )
    })

    x_axis <- reactive({
      req(input$xAxis)
      switch(
        input$xAxis,
        "Event type" = {
          event_grouping()
        },
        "Sender type" = {
          sender_grouping()
        },
        "Recipient type" = {
          recipient_grouping()
        },
        "Collection type" = {
          "collection_type"
        },
        "Initial museum" = {
          paste0("initial_museum", museum_grouping())
        }
      )
    })

    y_axis <- reactive({
      req(input$yAxis)
      switch(
        input$yAxis,
        "Event type" = {
          event_grouping()
        },
        "Sender type" = {
          sender_grouping()
        },
        "Recipient type" = {
          recipient_grouping()
        },
        "Collection type" = {
          "collection_type"
        },
        "Initial museum" = {
          paste0("initial_museum", museum_grouping())
        }
      )
    })

    x_label <- reactive({
      req(input$xAxis)
      switch(
        input$xAxis,
        "Event type" = {
          paste0("Event type (", input$eventGrouping, ")")
        },
        "Sender type" = {
          paste0("Sender type (", input$actorGrouping, ")")
        },
        "Recipient type" = {
          paste0("Recipient type (", input$actorGrouping, ")")
        },
        "Collection type" = {
          "Collection or object type"
        },
        "Initial museum" = {
          paste0("Initial museum (", input$museumGrouping, ")")
        }
      )
    })

    y_label <- reactive({
      req(input$yAxis)
      switch(
        input$yAxis,
        "Event type" = {
          paste0("Event type (", input$eventGrouping, ")")
        },
        "Sender type" = {
          paste0("Sender type (", input$actorGrouping, ")")
        },
        "Recipient type" = {
          paste0("Recipient type (", input$actorGrouping, ")")
        },
        "Collection type" = {
          "Collection or object type"
        },
        "Initial museum" = {
          paste0("Initial museum (", input$museumGrouping, ")")
        }
      )
    })

    only_show_last_event <- reactive({
      input$stepsOrLast == "Last known event"
    })
    observeEvent(only_show_last_event(), {
      req(input$stepsOrLast)
      if (only_show_last_event()) {
        updatePickerInput(
          session=session,
          inputId="stagesInPath",
          choices=NA,
          selected=NA
        )
      } else {
        updatePickerInput(
          session=session,
          inputId="stagesInPath",
          choices=c(1,2,3,4,5,6,7),
          selected=c(1)
        )
      }
    })
    stages_in_path <- reactive({input$stagesInPath})

    count_or_percentage <- reactive({input$countOrPercentage})

    observeEvent(event_grouping(), {
      event_type_choices <- unique(
        select(dispersal_events, .data[[event_grouping()]])
      )[[event_grouping()]]
      updatePickerInput(
        session=session,
        inputId="eventTypeFilter",
        choices=event_type_choices,
        selected=event_type_choices,
      )
    })

    observeEvent(actor_and_museum_grouping(), {
      # assemble type choices for senders and recipients (actor type & museum attribute)
      if (museum_grouping() == "_all") {
        sender_museum_type_choices = c()
        recipient_museum_type_choices = c()
      } else {
        sender_museum_type <- paste0("sender", museum_grouping())
        sender_museum_types <- dispersal_events |>
          filter(!is.na(.data[[sender_museum_type]])) |>
          select(.data[[sender_museum_type]]) |>
          unique() |>
          mutate(
            sender_museum_type=paste0("museum (", .data[[sender_museum_type]], ")")
          )
        sender_museum_type_choices <- sender_museum_types$sender_museum_type
        recipient_museum_type <- paste0("recipient", museum_grouping())
        recipient_museum_types <- dispersal_events |>
          filter(!is.na(.data[[recipient_museum_type]])) |>
          select(.data[[recipient_museum_type]]) |>
          unique() |>
          mutate(
            recipient_museum_type=paste0("museum (", .data[[recipient_museum_type]], ")")
          )
        recipient_museum_type_choices <- recipient_museum_types$recipient_museum_type
      }

      sender_type <- paste0("sender", actor_grouping())
      sender_type_choices <- unique(
        select(dispersal_events, .data[[sender_type]])
      )[[sender_type]]
      sender_choices <- c(sender_museum_type_choices, sender_type_choices)
      updatePickerInput(
        session=session,
        inputId="senderTypeFilter",
        choices=sender_choices,
        selected=sender_choices,
      )

      recipient_type <- paste0("recipient", actor_grouping())
      recipient_type_choices <- unique(
        select(dispersal_events, .data[[recipient_type]])
      )[[recipient_type]]
      recipient_choices <- c(recipient_museum_type_choices, recipient_type_choices)
      updatePickerInput(
        session=session,
        inputId="recipientTypeFilter",
        choices=recipient_choices,
        selected=recipient_choices,
      )
    })

    event_filter_choices <- reactive({ input$eventTypeFilter })
    sender_filter_choices <- reactive({ input$senderTypeFilter })
    recipient_filter_choices <- reactive({ input$recipientTypeFilter })
    collection_type_filter_choices <- reactive({ input$collectionTypeFilter })
    collection_status_filter_choices <- reactive({ input$collectionStatusFilter })
    size_filter_choices <- reactive({ input$sizeFilter })
    governance_filter_choices <- reactive({ input$governanceFilter })
    subject_filter_choices <- reactive({ input$subjectFilter })
    specific_subject_filter_choices <- reactive({ input$subjectSpecificFilter })
    region_filter_choices <- reactive({ input$regionFilter })
    accreditation_filter_choices <- reactive({ input$accreditationFilter })

    observeEvent(subject_filter_choices(), {
      freezeReactiveValue(input, "subjectSpecificFilter")
      specific_subjects <- subject_labels_map |>
        filter(subject_broad %in% subject_filter_choices())
      updatePickerInput(
        session=session,
        inputId="subjectSpecificFilter",
        choices=specific_subjects$subject,
        selected=specific_subjects$subject,
      )
    })

    filtered_events_and_participants <- reactive({
      filter_events(
        dispersal_events,
        event_grouping=event_grouping(),
        sender_grouping=sender_grouping(),
        museum_grouping=museum_grouping(),
        recipient_grouping=recipient_grouping(),
        only_show_last_event=only_show_last_event(),
        stages_in_path=stages_in_path(),
        event_filter=event_filter_choices(),
        sender_filter=sender_filter_choices(),
        recipient_filter=recipient_filter_choices(),
        collection_type_filter=collection_type_filter_choices(),
        collection_status_filter=collection_status_filter_choices(),
        size_filter=size_filter_choices(),
        governance_filter=governance_filter_choices(),
        subject_broad_filter=subject_filter_choices(),
        subject_specific_filter=specific_subject_filter_choices(),
        region_filter=region_filter_choices(),
        accreditation_filter=accreditation_filter_choices()
      )
    })

    events_summary <- reactive({
      summarize_events(
        filtered_events_and_participants(),
        x_axis(),
        y_axis()
      )
    })

    output$mainPlot <- renderPlotly({
      event_heatmap(
        events_summary(),
        x_label(),
        y_label(),
        count_or_percentage()
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
