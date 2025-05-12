source("src/modules/events/elements.R")

eventsServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    museum_grouping <- reactive({
      req(input$museumGrouping)
      switch(
        input$museumGrouping,
        "All" = "_all",
        "Size" = "_size",
        "Governance" = "_governance_broad",
        "Accreditation" = "_accreditation",
        "Subject Matter" = "_subject_matter_broad",
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
          "collection_or_object_type"
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
          "collection_or_object_type"
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

    observeEvent(actor_grouping(), {
      sender_type <- paste0("sender", actor_grouping())
      sender_type_choices <- unique(
        select(dispersal_events, .data[[sender_type]])
      )[[sender_type]]
      updatePickerInput(
        session=session,
        inputId="senderTypeFilter",
        choices=sender_type_choices,
        selected=sender_type_choices,
      )
      recipient_type <- paste0("recipient", actor_grouping())
      recipient_type_choices <- unique(
        select(dispersal_events, .data[[recipient_type]])
      )[[recipient_type]]
      updatePickerInput(
        session=session,
        inputId="recipientTypeFilter",
        choices=recipient_type_choices,
        selected=recipient_type_choices,
      )
    })

    event_filter_choices <- reactive({input$eventTypeFilter})
    sender_filter_choices <- reactive({input$senderTypeFilter})
    recipient_filter_choices <- reactive({input$recipientTypeFilter})

    size_filter_choices <- reactive({
      filter(
        size_labels,
        tidy_label %in% input$sizeFilter
      )$internal_label
    })
    governance_filter_choices <- reactive({
      filter(
        governance_labels,
        tidy_label %in% input$governanceFilter
      )$internal_label
    })
    subject_filter_choices <- reactive({
      filter(
        subject_broad_labels,
        tidy_label %in% input$subjectFilter
      )$internal_label
    })
    specific_subject_filter_choices <- reactive({
      filter(
        subject_full_labels,
        tidy_label %in% input$subjectSpecificFilter
      )$internal_label
    })
    region_filter_choices <- reactive({
      filter(
        country_region_labels,
        tidy_label %in% input$regionFilter
      )$internal_label
    })
    accreditation_filter_choices <- reactive({
      filter(
        accreditation_labels,
        tidy_label %in% input$accreditationFilter
      )$internal_label
    })

    observeEvent(subject_filter_choices(), {
      freezeReactiveValue(input, "subjectSpecificFilter")
      specific_subjects <- subject_full_labels |>
        filter(subject_broad %in% subject_filter_choices())
      updatePickerInput(
        session=session,
        inputId="subjectSpecificFilter",
        choices=specific_subjects$tidy_label,
        selected=specific_subjects$tidy_label,
      )
    })

    filtered_events_and_participants <- reactive({
      dispersal_events |>
        filter(
          .data[[event_grouping()]] %in% event_filter_choices(),
          .data[[sender_grouping()]] %in% sender_filter_choices(),
          .data[[recipient_grouping()]] %in% recipient_filter_choices(),
          initial_museum_size %in% size_filter_choices(), 
          initial_museum_governance %in% governance_filter_choices()
          | initial_museum_governance_broad %in% governance_filter_choices(),
          initial_museum_subject_matter_broad %in% subject_filter_choices(),
          initial_museum_subject_matter %in% specific_subject_filter_choices(),
          initial_museum_region %in% region_filter_choices()
          | initial_museum_country %in% region_filter_choices(),
          initial_museum_accreditation %in% accreditation_filter_choices()
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
