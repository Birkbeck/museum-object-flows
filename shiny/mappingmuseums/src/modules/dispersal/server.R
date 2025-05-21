source("src/modules/dispersal/elements.R")

dispersalServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    grouping_field <- reactive({input$grouping})

    museum_grouping_field <- reactive({
      if (input$groupingMuseums == "All museums") {
        return("all")
      } else if (input$groupingMuseums == "Size") {
        return("size")
      } else if (input$groupingMuseums == "Governance") {
        return("governance_broad")
      } else if (input$groupingMuseums == "Accreditation") {
        return("accreditation")
      } else if (input$groupingMuseums == "Subject Matter") {
        return("subject_matter_broad")
      } else if (input$groupingMuseums == "Country/Region") {
        return("region")
      }
    })
    scatter_grouping_field <- reactive({
      if (input$groupingMuseums == "All museums") {
        return("initial_museum_all")
      } else if (input$groupingMuseums == "Size") {
        return("initial_museum_size")
      } else if (input$groupingMuseums == "Governance") {
        return("initial_museum_governance_broad")
      } else if (input$groupingMuseums == "Accreditation") {
        return("initial_museum_accreditation")
      } else if (input$groupingMuseums == "Subject Matter") {
        return("initial_museum_subject_matter_broad")
      } else if (input$groupingMuseums == "Country/Region") {
        return("initial_museum_region")
      }
    })

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
          input$firepower | initial_museum_id != "mm.domus.SE513",
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

    observeEvent(grouping_filters(), {
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
      if (input$groupingMuseums == "All museums") {
        return("all")
      } else if (input$groupingMuseums == "Size") {
        return("size")
      } else if (input$groupingMuseums == "Governance") {
        return("governance_broad")
      } else if (input$groupingMuseums == "Accreditation") {
        return("accreditation")
      } else if (input$groupingMuseums == "Subject Matter") {
        return("subject_matter_broad")
      } else if (input$groupingMuseums == "Country/Region") {
        return("region")
      }
    })

    pathways_layout <- reactive({
      get_pathways_layout(
        filtered_sequences(),
        ownershipChangesStart(),
        ownershipChangesEnd(),
        input$grouping,
        museum_grouping(),
        steps_or_first_last()
      )
    })
    sequences_layout <- reactive({
      get_sequences_layout(
        filtered_sequences(),
        ownershipChangesStart(),
        ownershipChangesEnd(),
        input$grouping
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
