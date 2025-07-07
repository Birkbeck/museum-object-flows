source("src/modules/dispersal/elements.R")

dispersalServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$reset, {
      updateRadioButtons(session=session, inputId="stepsOrFirstLast", selected="Steps in path")
      updateSliderInput(session=session, inputId="stagesInOwnershipPath", value=2)
      updateCheckboxInput(session=session, inputId="showTransactionCounts", value=FALSE)
      updateRadioButtons(session=session, inputId="countOrPercentage", selected="count")
      updateSwitchInput(session=session, inputId="firepower", value=FALSE)
      updateSelectInput(session=session, inputId="grouping", selected="Actor type (core categories)")
      updateSelectInput(session=session, inputId="groupingMuseums", selected="Governance")
      updatePickerInput(
        session=session,
        inputId="transactionTypeFilter",
        selected=c("Change of ownership", "Change of custody", "End of existence")
      )
      updatePickerInput(
        session=session,
        inputId="eventTypeUncertaintyFilter",
        selected=c("certain", "?+", "?", "?-")
      )
      updatePickerInput(
        session=session, inputId="collectionStatusFilter", selected=collection_status_labels$label,
      )
      updatePickerInput(
        session=session, inputId="startGovernanceFilter", selected="local authority"
      )
      updatePickerInput(
        session=session, inputId="startSizeFilter", selected=size_labels$label
      )
      updatePickerInput(
        session=session, inputId="startSubjectFilter", selected=subject_broad_labels$label
      )
      updatePickerInput(
        session=session, inputId="startSubjectSpecificFilter", selected=subject_full_labels$label
      )
      updatePickerInput(
        session=session, inputId="startRegionFilter", selected=region_labels$label
      )
      updatePickerInput(
        session=session, inputId="startAccreditationFilter", selected=accreditation_labels$label
      )
      filtered_museums <- get_dispersal_initial_museums(
        dispersal_events,
        include_firepower(),
        size_filter_choices(),
        governance_filter_choices(),
        subject_filter_choices(),
        specific_subject_filter_choices(),
        region_filter_choices(),
        accreditation_filter_choices()
      )
      updateVirtualSelect(
        session=session,
        inputId="initialMuseum",
        choices=filtered_museums$name,
        selected=filtered_museums$name
      )
      updatePickerInput(
        session=session,
        inputId="sequenceEnd",
        label=paste0("(", actor_grouping(), ")"),
        choices=actor_choices_table()$label,
        selected=actor_choices_table()$label
      )
      updatePickerInput(
        session=session,
        inputId="sequencePassesThrough",
        label=paste0("(", actor_grouping(), ")"),
        choices=actor_choices_table()$label,
        selected=actor_choices_table()$label
      )
    })

    grouping_field <- reactive({input$grouping})
    actor_grouping <- reactive({
      list(
        "Actor sector"="sector",
        "Actor type (core categories)"="core_type",
        "Actor type (most specific)"="type",
        "Actor region/country"="region"
      )[input$grouping]
    })
    sender_grouping <- reactive({
      paste0("sender_", actor_grouping())
    })
    recipient_grouping <- reactive({
      paste0("recipient_", actor_grouping())
    })
    initial_museum_grouping <- reactive({
      paste0("initial_museum_", actor_grouping())
    })

    museum_grouping_field <- reactive({
      if (input$groupingMuseums == "All") {
        return("all")
      } else if (input$groupingMuseums == "Size") {
        return("size")
      } else if (input$groupingMuseums == "Governance") {
        return("governance_broad")
      } else if (input$groupingMuseums == "Accreditation") {
        return("accreditation")
      } else if (input$groupingMuseums == "Subject Matter") {
        return("subject_broad")
      } else if (input$groupingMuseums == "Country/Region") {
        return("region")
      }
    })
    scatter_grouping_field <- reactive({
      paste0("initial_museum_", museum_grouping_field())
    })

    transaction_type_filter <- reactive({input$transactionTypeFilter})

    event_type_filter <- reactive({ input$eventTypeFilter })
    event_type_uncertainty_filter <- reactive({ input$eventTypeUncertaintyFilter })
    collection_status_filter <- reactive({ input$collectionStatusFilter })
    size_filter_choices <- reactive({ input$startSizeFilter })
    governance_filter_choices <- reactive({ input$startGovernanceFilter })
    subject_filter_choices <- reactive({ input$startSubjectFilter })
    specific_subject_filter_choices <- reactive({ input$startSubjectSpecificFilter })
    region_filter_choices <- reactive({ input$startRegionFilter })
    accreditation_filter_choices <- reactive({ input$startAccreditationFilter })
    include_firepower <- reactive({
      if (is.null(input$firepower)) {
        FALSE
      } else {
        input$firepower
      }
    })
    initial_museum_filters <- reactive({
      list(
        input$firepower,
        input$startSizeFilter,
        input$startGovernanceFilter,
        input$startSubjectFilter,
        input$startSubjectSpecificFilter,
        input$startRegionFilter,
        input$startAccreditationFilter
      )
    })
    grouping_filters <- reactive({
      list(
        input$grouping,
        input$groupingMuseums
      )
    })

    actor_choices_table <- reactive({
      req(input$grouping, input$groupingMuseums)
      get_actor_choices(grouping_field(), museum_grouping_field())
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
      sub_event_types <- event_types |>
        filter(!is_core_category & type_name != "event")
      if ("Change of ownership" %in% transaction_type_filter()) {
        event_type_choices <- rbind(
          event_type_choices,
          select(filter(core_event_types, change_of_ownership), type_name)
        ) |>
          rbind(
            select(
              filter(sub_event_types, change_of_ownership),
              type_name=core_type
            )
          )
      }
      if ("Change of custody" %in% transaction_type_filter()) {
        event_type_choices <- rbind(
          event_type_choices,
          select(filter(core_event_types, change_of_custody), type_name)
        ) |>
          rbind(
            select(
              filter(sub_event_types, change_of_custody),
              type_name=core_type
            )
          )
      }
      if ("End of existence" %in% transaction_type_filter()) {
        event_type_choices <- rbind(
          event_type_choices,
          select(filter(core_event_types, end_of_existence), type_name)
        ) |>
          rbind(
            select(
              filter(sub_event_types, end_of_existence),
              type_name=core_type
            )
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
      specific_subjects <- subject_labels_map |>
        filter(subject_broad %in% subject_filter_choices())
      updatePickerInput(
        session=session,
        inputId="startSubjectSpecificFilter",
        choices=specific_subjects$subject,
        selected=specific_subjects$subject,
      )
    })

    initial_museum_ids <- reactive({
      sapply(input$initialMuseum, function(text) sub(".*\\(([^()]*)\\)$", "\\1", text))
    })

    observeEvent(initial_museum_filters(), {
      freezeReactiveValue(input, "initialMuseum")
      filtered_museums <- get_dispersal_initial_museums(
        dispersal_events,
        include_firepower(),
        size_filter_choices(),
        governance_filter_choices(),
        subject_filter_choices(),
        specific_subject_filter_choices(),
        region_filter_choices(),
        accreditation_filter_choices()
      )
      updateVirtualSelect(
        session=session,
        inputId="initialMuseum",
        choices=filtered_museums$name,
        selected=filtered_museums$name
      )
    })

    observeEvent(grouping_filters(), {
      updatePickerInput(
        session=session,
        inputId="sequenceEnd",
        label=paste0("(", actor_grouping(), ")"),
        choices=actor_choices_table()$label,
        selected=actor_choices_table()$label
      )
      updatePickerInput(
        session=session,
        inputId="sequencePassesThrough",
        label=paste0("(", actor_grouping(), ")"),
        choices=actor_choices_table()$label,
        selected=actor_choices_table()$label
      )
    })

    filtered_sequences <- reactive({
      get_filtered_sequences(
        dispersal_events,
        transaction_type_filter(),
        actor_grouping(),
        museum_grouping_field(),
        event_type_filter(),
        event_type_uncertainty_filter(),
        collection_status_filter(),
        initial_museum_ids(),
        sequence_end(),
        sequence_passes_through(),
        steps_or_first_last()
      )
    })

    steps_or_first_last <- reactive({input$stepsOrFirstLast})

    observeEvent(steps_or_first_last(), {
      if (input$stepsOrFirstLast == "First and last actors") {
        shinyjs::disable("stagesInOwnershipPath")
      } else {
        shinyjs::enable("stagesInOwnershipPath")
      }
    })

    selected_columns <- reactive({input$tableSelect})

    ownershipChangesStart <- reactiveVal(1)
    ownershipChangesEnd <- reactiveVal(2)
    debouncedStagesInOwnershipPath <- debounce(
      reactive(input$stagesInOwnershipPath),
      millis=300
    )
    observeEvent(debouncedStagesInOwnershipPath(), {
      ownershipChangesEnd(debouncedStagesInOwnershipPath())
    })

    museum_grouping <- reactive({
      if (input$groupingMuseums == "All") {
        return("all")
      } else if (input$groupingMuseums == "Size") {
        return("size")
      } else if (input$groupingMuseums == "Governance") {
        return("governance_broad")
      } else if (input$groupingMuseums == "Accreditation") {
        return("accreditation")
      } else if (input$groupingMuseums == "Subject Matter") {
        return("subject_broad")
      } else if (input$groupingMuseums == "Country/Region") {
        return("region")
      }
    })

    pathways_layout <- reactive({
      get_pathways_layout(
        filtered_sequences(),
        ownershipChangesStart(),
        ownershipChangesEnd(),
        actor_grouping(),
        museum_grouping(),
        steps_or_first_last()
      )
    })
    sequences_layout <- reactive({
      get_sequences_layout(
        filtered_sequences(),
        ownershipChangesStart(),
        ownershipChangesEnd(),
        actor_grouping(),
        museum_grouping()
      )
    })
    map_layout <- reactive({
      get_map_layout(
        filtered_sequences(),
        ownershipChangesStart(),
        ownershipChangesEnd(),
        input$grouping,
        input$showTransactionCounts,
        steps_or_first_last()
      )
    })
    movements_distances <- reactive({
      get_movements_distances(
        filtered_sequences(),
        ownershipChangesStart(),
        ownershipChangesEnd(),
        scatter_grouping_field(),
        input$groupingMuseums,
        input$showTransactionCounts,
        steps_or_first_last(),
        TRUE
      )
    })

    currentMainPlot <- reactiveVal("pathways")
    # Update the current plot based on user clicks
    observeEvent(input$pathways, { currentMainPlot("pathways") })
    observeEvent(input$sequences, { currentMainPlot("sequences") })
    observeEvent(input$map, { currentMainPlot("map") })
    observeEvent(input$distances, { currentMainPlot("distances") })

    output$errorMessage <- renderUI({
      if (nrow(filtered_sequences()) == 0) {
        p("The filters returned no results. Try less specific filters")
      }
    })

    output$mainPlotOptions <- renderUI({
      if(currentMainPlot() == "distances") {
        radioButtons(
          inputId = NS(id, "countOrPercentage"),
          label = "",
          choices = list(
            "Show number of collections/objects" = "count",
            "Show percentage of collections/objects" = "percentage",
            "Show rowwise percentages" = "percentage_y",
            "Show columnwise percentages" = "percentage_x"
          )
        )
      }
    })

    output$mainPlot <- renderPlotly({
      if (currentMainPlot() == "pathways") {
        pathway_dendrogram(
          pathways_layout(),
          input$showTransactionCounts
        )
      } else if (currentMainPlot() == "sequences") {
        sequence_network(
          sequences_layout(),
          ownershipChangesStart(),
          ownershipChangesEnd(),
          input$showTransactionCounts
        )
      } else if (currentMainPlot() == "map") {
        movements_map(
          map_layout()
        )
      } else if (currentMainPlot() == "distances") {
        movements_heatmap(
          movements_distances(),
          scatter_grouping_field(),
          input$groupingMuseums,
          count_or_percentage()
        )
      }
    })

    count_or_percentage <- reactive({input$countOrPercentage})

    output$pathwaysSmall <- renderPlot({
      pathway_dendrogram_small(pathways_layout())
    })
    output$sequencesSmall <- renderPlot({
      sequence_network_small(
        sequences_layout(),
        ownershipChangesStart(),
        ownershipChangesEnd()
      )
    })
    output$mapSmall <- renderPlot({
      movements_map_small(
        map_layout()
      )
    })
    output$distancesSmall <- renderPlot({
      movements_heatmap_small(
        movements_distances(),
        scatter_grouping_field(),
        input$groupingMuseums
      )
    })

    output$downloadSequencesTable <- downloadHandler(
      filename = function() {
        paste('dispersal-sequences-data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(
          filtered_sequences() |> select(all_of(selected_columns())),
          con
        )
      },
      contentType = "text/csv"
    )

    output$pathwaysTable <- renderDT({
      pathway_table(filtered_sequences(), selected_columns())
    }, options=list(pageLength=100))

  })
}
