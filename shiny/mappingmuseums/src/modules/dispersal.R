popover_js <- HTML("
$(document).ready(function() {
  $('[data-toggle=\"popover\"]').popover({
    trigger: 'hover', // Show on hover
    container: 'body', // Attach to body to avoid layout issues
    html: true
  });
});")

dispersalUI <- function(id) {
  fluidPage(
    fluidRow(
      p("The transfer of museum collections after closure. The diagram below summarizes categorizes the pathways that collections follow when leaving a closed museum."),
      p("Nodes are labelled with the number of actors belonging to the category at that time step. The width of lines is proportional to the possible number of objects involved in transfers."),
      p("Use the options to alter the way that museums and other actors are grouped together and to filter transactions according to type or collection source/destination."),
      p("Find out more about actor and event types in the actor and event types tab."),
      p("The table below the diagram provides details of the depicted transfers."),
      ),
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          width=3,
          tagList(
            h3("Filter Transactions"),
            tagList(
              tags$span(
                tags$strong("Group Actors by: "),
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
                tags$strong("Group Museums by: "),
                tags$i(
                  class = "fa fa-info-circle", # Font Awesome info icon
                  style = "color: #007bff; cursor: pointer;", # Style for visibility
                  `data-toggle` = "popover", # Bootstrap popover attribute
                  `data-placement` = "right", # Position above the icon
                  title = "Group museums by",
                  `data-content` = "<p>Select how to group and display museums on the diagram.</p><p>Museums from the Mapping Museums Database are always grouped according to governance.</p><p>Other actors can be grouped according to:</p><p><strong>Actor sector:</strong> The sector of the economy (<i>public</i>, <i>private</i>, <i>third</i>, <i>etc.</i>) that they belong to.</p><p><strong>Most specific actor type:</strong> The most specific actor type that actors are known to belong to.</p><p><strong>Core category actor type:</strong> The core category that they belong to. Refer to the actor types hierarchy to see which types are included as core categories.</p><p><strong>Most general actor type:</strong> The most general actor type that actors are known to belong to (<i>organisation</i>, <i>individual</i>, or <i>historic house</i>).</p>"
                )
              ),
              tags$script(popover_js),
              selectInput(
                NS(id, "groupingMuseums"),
                label="",
                choices=c("All museums", field_names$name),
                selected="Governance"
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

            tagList(
              tags$span(
                tags$strong("Display: "),
                tags$i(
                  class = "fa fa-info-circle",
                  style = "color: #007bff; cursor: pointer;",
                  `data-toggle` = "popover",
                  `data-placement` = "right",
                  title = "Display steps or first and last actors",
                  `data-content` = "<p><strong>Steps in path:</strong> View intermediate actors in the sequences of ownership and/or custody changes</p><p><strong>First and last actors:</strong> View only the initial museum and the last known actor in the sequence.</p>"
                )
              ),
              tags$script(popover_js),
              radioButtons(
                NS(id, "stepsOrFirstLast"),
                label="",
                choices=c("Steps in path", "First and last actors"),
                selected="Steps in path",
                inline=TRUE
              ),
              ),
            tagList(
              tags$span(
                tags$strong("Steps in path: "),
                tags$i(
                  class = "fa fa-info-circle",
                  style = "color: #007bff; cursor: pointer;",
                  `data-toggle` = "popover",
                  `data-placement` = "right",
                  title = "Steps in path",
                  `data-content` = "<p>Select the start and end point of sequences. Step 1 shows the initial museums where collections originated.</p><p>Use the slider to increase the number of steps away from the museum shown on the diagram.</p>"
                )
              ),
              tags$script(popover_js),
              sliderInput(
                NS(id, "stagesInOwnershipPath"),
                label="",
                value=2,
                min=1,
                max=7,
                step=1,
                ticks=FALSE,
                width="50%"
              )
            )
          )
        ),
        mainPanel(
          plotlyOutput(NS(id, "pathwaysNetwork"), width="100%", height="850px"),
          img(src='actor-sector-key.png', align="left", width="150px"),
          fluidRow(
            p("Click on one of the small charts below to see it enlarged in the main panel above.")
          ),
          fluidRow(
            column(
              3,
              style=card_style,
              plotOutput(
                NS(id, "pathwaysSmall"),
                width=small_chart_size_px,
                height=small_chart_size_px,
                click=NS(id, "pathways")
              )
            ),
            column(
              3,
              style=card_style,
              plotOutput(
                NS(id, "sequencesSmall"),
                width=small_chart_size_px,
                height=small_chart_size_px,
                click=NS(id, "sequences")
              )
            ),
            column(
              3,
              style=card_style,
              plotOutput(
                NS(id, "mapSmall"),
                width=small_chart_size_px,
                height=small_chart_size_px,
                click=NS(id, "map")
              )
            ),
            column(
              3,
              style=card_style,
              plotOutput(
                NS(id, "distancesSmall"),
                width=small_chart_size_px,
                height=small_chart_size_px,
                click=NS(id, "distances")
              )
            )
          )
        )
      ),
    ),
    fluidRow(
      h3("Sequences Data"),
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
      downloadButton(NS(id, "downloadSequencesTable"), label="Download table as CSV")
    ),
    fluidRow(
      DTOutput(NS(id, "pathwaysTable"))
    )
  )


}

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

    output$pathwaysNetwork <- renderPlotly({
      pathway_dendrogram(
        pathways_layout(),
        input$showTransactionCounts
      )
    })

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

get_actor_choices <- function(grouping_dimension, museum_grouping_dimension) {
  grouping_dimension <- list(
    "Actor Sector"="sector",
    "Actor Type (Core Categories)"="core_type",
    "Actor Type (Most General)"="general_type",
    "Actor Type (Most Specific)"="type"
  )[grouping_dimension]
  recipient_grouping_dimension <- paste0("recipient_", grouping_dimension)
  recipient_museum_grouping_dimension <- paste0("recipient_", museum_grouping_dimension)
  choices_table <- dispersal_events |>
    mutate(
      initial_museum_all="all",
      sender_all=ifelse(!is.na(sender_size), "all", NA),
      recipient_all=ifelse(!is.na(recipient_size), "all", NA),
    ) |>
    group_by(.data[[recipient_museum_grouping_dimension]], .data[[recipient_grouping_dimension]]) |>
    summarize(
      to=paste(.data[[recipient_museum_grouping_dimension]], .data[[recipient_grouping_dimension]], sep="@"),
      label=ifelse(
        !is.na(.data[[recipient_museum_grouping_dimension]]),
        paste(.data[[recipient_museum_grouping_dimension]], "museum"),
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
    museum_grouping_dimension,
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

  initial_museum_museum_grouping_dimension <- paste0("initial_museum_", museum_grouping_dimension)
  sender_museum_grouping_dimension <- paste0("sender_", museum_grouping_dimension)
  recipient_museum_grouping_dimension <- paste0("recipient_", museum_grouping_dimension)
  final_museum_filtering_dimension <- paste0("final_", museum_grouping_dimension)

  # find each event's previous event in the chain of shown events
  sequential_events <- sequential_events |>
    mutate(
      initial_museum_all="all",
      sender_all=ifelse(!is.na(sender_size), "all", NA),
      recipient_all=ifelse(!is.na(recipient_size), "all", NA),
    ) |>
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
    group_by(original_collection_id) |> # TODO: this does not account for collections that branch
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
        paste(.data[[initial_museum_museum_grouping_dimension]], .data[[initial_museum_grouping_dimension]], 1, sep="@"),
        paste(.data[[sender_museum_grouping_dimension]], .data[[sender_grouping_dimension]], sender_position, sep="@")
      ),
      to=paste(.data[[recipient_museum_grouping_dimension]], .data[[recipient_grouping_dimension]], recipient_position, sep="@"),
    )

  # find end points of events 
  events_destinations <- sequential_events |>
    group_by(event_id, ancestor_events) |>
    filter(recipient_position==max(recipient_position)) |>
    summarize(
      destination=paste(.data[[recipient_museum_grouping_dimension]], .data[[recipient_grouping_dimension]], sep="@")
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
      recipient=paste(.data[[recipient_museum_grouping_dimension]], .data[[recipient_grouping_dimension]], sep="@")
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
        from=paste(.data[[initial_museum_museum_grouping_dimension]], .data[[initial_museum_grouping_dimension]], sender_position, sep="@"),
        to=paste(.data[[recipient_museum_grouping_dimension]], .data[[recipient_grouping_dimension]], recipient_position, sep="@")
      ) |>
      ungroup()
    }
  sequential_events    
}

pathway_table <- function(sequences, selected_columns) {
  sequences |>
    select(all_of(selected_columns))
}

get_pathways_layout <- function(sequences,
                                start_position,
                                end_position,
                                grouping_dimension,
                                museum_grouping_dimension,
                                steps_or_first_last) {
  grouping_dimension_name <- list(
    "Actor Sector"="sector",
    "Actor Type (Core Categories)"="core_type",
    "Actor Type (Most General)"="general_type",
    "Actor Type (Most Specific)"="type"
  )[grouping_dimension]
  sender_grouping_dimension <- paste0("sender_", grouping_dimension_name)
  recipient_grouping_dimension <- paste0("recipient_", grouping_dimension_name)

  sender_museum_grouping_dimension <- paste0("sender_", museum_grouping_dimension)
  recipient_museum_grouping_dimension <- paste0("recipient_", museum_grouping_dimension)

 dendrogram_data <-  sequences |>
   filter(recipient_position <= end_position) |>
   select(
     event_id,
     event_stage_in_path,
     previous_shown_event,
     from,
     to,
     sender_id,
     recipient_id,
     .data[[sender_museum_grouping_dimension]],
     .data[[recipient_museum_grouping_dimension]],
     .data[[sender_grouping_dimension]],
     .data[[recipient_grouping_dimension]],
     sender_sector,
     recipient_sector,
     sender_position,
     recipient_position,
     sender_quantity,
     recipient_quantity,
     collection_estimated_size,
     collection_estimated_size_max,
     collection_estimated_size_min
   )

  build_chains <- function(data) {
    # give stages in each type of path a unique id
    data <- data |>
      mutate(
        from_label=from,
        to_label=to,
        full_from=from,
        full_to=paste(from, "->", to)
      ) |>
      arrange(event_stage_in_path)
    for (i in 1:nrow(data)) {
      prev_event <- data$previous_shown_event[i]
      if (!is.na(prev_event)) {
        prev_index <- which(data$event_id == prev_event)
        data$full_from[i] <- paste(data$full_from[prev_index], "->", data$from[i])
        data$full_to[i] <- paste(data$full_to[prev_index], "->", data$to[i])
      }
    }
    data |>
      select(-from, -to) |>
      rename(from = full_from, to = full_to)
  }

  if (steps_or_first_last == "Steps in path") {
    dendrogram_data <- build_chains(dendrogram_data)
  } else {
    dendrogram_data <- dendrogram_data |>
      mutate(
        to = paste(from, "->", to)
      )
  }

  initial_senders <- dendrogram_data |>
    filter(event_stage_in_path == 1) |>
    select(.data[[sender_museum_grouping_dimension]], .data[[sender_grouping_dimension]]) |>
    distinct()
  if(nrow(initial_senders) > 1) {
    dummy_rows <- dendrogram_data |>
      filter(event_stage_in_path == 1) |>
      mutate(
        event_id = "",
        event_stage_in_path = 0,
        previous_shown_event = "",
        to = from,
        from = "",
        recipient_id = sender_id,
        sender_id = "",
        recipient_sector = sender_sector,
        !!sym(recipient_museum_grouping_dimension) := .data[[sender_museum_grouping_dimension]],
        !!sym(sender_museum_grouping_dimension) := "dummy",
        !!sym(recipient_grouping_dimension) := .data[[sender_grouping_dimension]],
        !!sym(sender_grouping_dimension) := "",
        recipient_position = sender_position,
        sender_position = 0,
        recipient_quantity = sender_quantity,
        sender_quantity = "1"
      )
    dendrogram_data <- bind_rows(dendrogram_data, dummy_rows)
  }

  from_nodes_counts <- dendrogram_data |>
    mutate(
      id=from,
      museum_grouping_dimension=.data[[sender_museum_grouping_dimension]],
      grouping_dimension=.data[[sender_grouping_dimension]],
      position=sender_position
    ) |>
    select(sender_id, sender_quantity, id, museum_grouping_dimension, grouping_dimension, position, sender_sector) |>
    distinct() |>
    mutate(
      sender_count=ifelse(sender_quantity=="many",2,as.numeric(sender_quantity))
    ) |>
    group_by(id, museum_grouping_dimension, grouping_dimension, position) |>
    summarize(
      from_count=sum(sender_count),
      from_count_suffix=ifelse("many" %in% sender_quantity, "+", ""),
      from_count_label=paste0(from_count, from_count_suffix),
      public_instances=sum(ifelse(sender_sector=="public", sender_count, 0)),
      university_instances=sum(ifelse(sender_sector=="university", sender_count, 0)),
      third_instances=sum(ifelse(sender_sector=="third", sender_count, 0)),
      private_instances=sum(ifelse(sender_sector=="private", sender_count, 0)),
      hybrid_instances=sum(ifelse(sender_sector=="hybrid", sender_count, 0)),
      public_proportion = public_instances / from_count,
      university_proportion = university_instances / from_count,
      third_proportion = third_instances / from_count,
      private_proportion = private_instances / from_count,
      hybrid_proportion = hybrid_instances / from_count,
      from_sector = case_when(
        public_proportion == 1 ~ "public",
        university_proportion == 1 ~ "university",
        third_proportion == 1 ~ "third",
        private_proportion == 1 ~ "private",
        hybrid_proportion == 1 ~ "hybrid",
        public_proportion >= 0.5 ~ "mostly public",
        university_proportion >= 0.5 ~ "mostly university",
        third_proportion >= 0.5 ~ "mostly third",
        private_proportion >= 0.5 ~ "mostly private",
        hybrid_proportion >= 0.5 ~ "mostly hybrid",
        TRUE ~ "unknown"
      )
    ) |>
    ungroup()

  to_nodes_counts <- dendrogram_data |>
    mutate(
      id=to,
      museum_grouping_dimension=.data[[recipient_museum_grouping_dimension]],
      grouping_dimension=.data[[recipient_grouping_dimension]],
      position=recipient_position,
    ) |>
    select(recipient_id, recipient_quantity, id, museum_grouping_dimension, grouping_dimension, position, recipient_sector) |>
    distinct() |>
    mutate(
      recipient_count=ifelse(recipient_quantity=="many",2,as.numeric(recipient_quantity))
    ) |>
    group_by(id, museum_grouping_dimension, grouping_dimension, position) |>
    summarize(
      to_count=sum(recipient_count),
      to_count_suffix=ifelse("many" %in% recipient_quantity, "+", ""),
      to_count_label=paste0(to_count, to_count_suffix),
      public_instances=sum(ifelse(recipient_sector=="public", recipient_count, 0)),
      university_instances=sum(ifelse(recipient_sector=="university", recipient_count, 0)),
      third_instances=sum(ifelse(recipient_sector=="third", recipient_count, 0)),
      private_instances=sum(ifelse(recipient_sector=="private", recipient_count, 0)),
      hybrid_instances=sum(ifelse(recipient_sector=="hybrid", recipient_count, 0)),
      public_proportion = public_instances / to_count,
      university_proportion = university_instances / to_count,
      third_proportion = third_instances / to_count,
      private_proportion = private_instances / to_count,
      hybrid_proportion = hybrid_instances / to_count,
      to_sector = case_when(
        public_proportion == 1 ~ "public",
        university_proportion == 1 ~ "university",
        third_proportion == 1 ~ "third",
        private_proportion == 1 ~ "private",
        hybrid_proportion == 1 ~ "hybrid",
        public_proportion >= 0.5 ~ "mostly public",
        university_proportion >= 0.5 ~ "mostly university",
        third_proportion >= 0.5 ~ "mostly third",
        private_proportion >= 0.5 ~ "mostly private",
        hybrid_proportion >= 0.5 ~ "mostly hybrid",
        TRUE ~ "unknown"
      )
    ) |>
    ungroup()

  node_counts <- from_nodes_counts |>
    full_join(to_nodes_counts, by=c("id", "museum_grouping_dimension", "grouping_dimension", "position")) |>
    mutate(
      count = ifelse(
        is.na(to_count),
        from_count,
        ifelse(
          is.na(from_count), 
          to_count,
          ifelse(
            from_count > to_count,
            from_count,
            to_count
          )
        )
      ),
      count_label = ifelse(
        is.na(to_count),
        from_count_label,
        ifelse(
          is.na(from_count), 
          to_count_label,
          ifelse(
            from_count > to_count,
            from_count_label,
            to_count_label
          )
        )
      ),
      sector_label = ifelse(
        is.na(to_count),
        from_sector,
        ifelse(
          is.na(from_count), 
          to_sector,
          ifelse(
            from_count > to_count,
            from_sector,
            to_sector
          )
        )
      )
    ) |>
    filter(position >= start_position & position <= end_position)

  if (grouping_dimension_name == "sector") {
    node_counts <- node_counts |>
      mutate(
        name=paste(museum_grouping_dimension, grouping_dimension, sep="@"),
        label = ifelse(
          !is.na(museum_grouping_dimension), 
          paste(gsub("_", " ", museum_grouping_dimension), "museum"),
          paste(grouping_dimension, "sector")
        )
      )
  } else {
    node_counts <- node_counts |>
      mutate(
        name=paste(museum_grouping_dimension, grouping_dimension, sep="@"),
        label = ifelse(
          !is.na(museum_grouping_dimension), 
          paste(gsub("_", " ", museum_grouping_dimension), "museum"),
          grouping_dimension
        )
      )
  }

  count_edges <- dendrogram_data |>
    select(from, to, .data[[sender_grouping_dimension]], .data[[sender_museum_grouping_dimension]], .data[[recipient_grouping_dimension]], .data[[recipient_museum_grouping_dimension]]) |>
    group_by(from, to, .data[[sender_grouping_dimension]], .data[[sender_museum_grouping_dimension]], .data[[recipient_grouping_dimension]], .data[[recipient_museum_grouping_dimension]]) |>
    summarize(count = n()) |>
    ungroup() |>
    mutate(
      label=count
    )
  size_edges <- dendrogram_data |>
    mutate(
      collection_estimated_size = ifelse(is.na(collection_estimated_size), 1, collection_estimated_size)
    ) |>
    select(from, to, .data[[sender_grouping_dimension]], .data[[sender_museum_grouping_dimension]], .data[[recipient_grouping_dimension]], .data[[recipient_museum_grouping_dimension]], collection_estimated_size) |>
    group_by(from, to, .data[[sender_grouping_dimension]], .data[[sender_museum_grouping_dimension]], .data[[recipient_grouping_dimension]], .data[[recipient_museum_grouping_dimension]]) |>
    summarize(count = sum(collection_estimated_size)) |>
    ungroup() |>
    mutate(
      label=""
    )
  max_size_edges <- dendrogram_data |>
    mutate(
      collection_estimated_size = ifelse(is.na(collection_estimated_size_max), 1, collection_estimated_size_max)
    ) |>
    select(from, to, .data[[sender_grouping_dimension]], .data[[sender_museum_grouping_dimension]], .data[[recipient_grouping_dimension]], .data[[recipient_museum_grouping_dimension]], collection_estimated_size) |>
    group_by(from, to, .data[[sender_grouping_dimension]], .data[[sender_museum_grouping_dimension]], .data[[recipient_grouping_dimension]], .data[[recipient_museum_grouping_dimension]]) |>
    summarize(count = sum(collection_estimated_size)) |>
    ungroup() |>
    mutate(
      label=""
    )
  min_size_edges <- dendrogram_data |>
    mutate(
      collection_estimated_size = ifelse(is.na(collection_estimated_size_min), 1, collection_estimated_size_min)
    ) |>
    select(from, to, .data[[sender_grouping_dimension]], .data[[sender_museum_grouping_dimension]], .data[[recipient_grouping_dimension]], .data[[recipient_museum_grouping_dimension]], collection_estimated_size) |>
    group_by(from, to, .data[[sender_grouping_dimension]], .data[[sender_museum_grouping_dimension]], .data[[recipient_grouping_dimension]], .data[[recipient_museum_grouping_dimension]]) |>
    summarize(count = sum(collection_estimated_size)) |>
    ungroup() |>
    mutate(
      label=""
    )
  edges <- rbind(
    count_edges,
    min_size_edges |> mutate(count = count * 0.25),
    min_size_edges |> mutate(count = count * 0.5),
    min_size_edges |> mutate(count = count * 0.75),
    min_size_edges,
    size_edges |> mutate(count = count * 0.25),
    size_edges |> mutate(count = count * 0.5),
    size_edges |> mutate(count = count * 0.75),
    size_edges,
    max_size_edges |> mutate(count = count * 0.25),
    max_size_edges |> mutate(count = count * 0.5),
    max_size_edges |> mutate(count = count * 0.75),
    max_size_edges
  ) |>
    left_join(node_counts |> select(from=id, from_sector_label=sector_label), by="from")

  dendrogram_graph <- graph_from_data_frame(count_edges)
  dendrogram_layout <- create_layout(dendrogram_graph, layout="dendrogram", circular=TRUE) |>
    mutate(id=name) |>
    select(id, x, y)

  node_counts <- node_counts |> left_join(dendrogram_layout, by="id")
  start_positions <- node_counts |>
    select(from=id, x, y)
  end_positions <- node_counts |>
    select(to=id, xend=x, yend=y)
  edges <- edges |>
    left_join(start_positions, by="from") |>
    left_join(end_positions, by="to") |>
    mutate(
      label_position_x=(x + xend) / 2,
      label_position_y=(y + yend) / 2
    )

  list("nodes"=node_counts, "edges"=edges)
}

pathway_dendrogram <- function(layout, show_transaction_counts) {
  node_counts <- layout$nodes
  edges <- layout$edges
  theta <- seq(pi/8, 2*pi, length.out=16)
  xo <- diff(range(node_counts$x)) / 500
  yo <- diff(range(node_counts$y)) / 500
  label_shadows <- node_counts |>
    mutate(y=y-0.06) |>
    crossing(theta=theta) |>
    mutate(
      x_offset = x + cos(theta) * xo,
      y_offset = y + sin(theta) * yo
    )
  transaction_pathways_plot <- ggplot(node_counts, aes(x=x, y=y)) +
    geom_segment(
      data=edges,
      aes(
        x=x,
        y=y,
        xend=xend,
        yend=yend,
        linewidth=count,
        colour=from_sector_label,
      ),
      alpha=0.1,
      arrow=arrow(ends="last", length=unit(0.2, "inches"))
    ) +
    geom_point(
      aes(
        label=label,
        fill=sector_label,
        size=count
      ),
      pch=21,
      colour="black",
      alpha=0.7
    ) +
    geom_text(
      aes(label=count_label)
    ) +
    geom_text(
      data=label_shadows,
      aes(x=x_offset, y=y_offset,label=label),
      size=5,
      colour="white"
    ) +
    geom_text(
      data=node_counts,
      aes(y=y-0.06,label=label),
      size=5,
    ) +
    scale_x_continuous(expand=c(0.1, 0)) +
    scale_y_continuous(expand=c(0.1, 0)) +
    scale_size_continuous(range=c(5, 20)) +
    scale_linewidth(range=c(1,10)) +
    public_private_fill_scale +
    public_private_colour_scale +
    labs(
      title="Pathways Taken by Museum Collections"
    ) +
    network_theme +
    theme(
      axis.title=element_text(size=0),
      axis.text=element_text(size=0),
      legend.position="None"
    )
  if (show_transaction_counts) {
    transaction_pathways_plot <- transaction_pathways_plot +
      geom_text(
        data=edges,
        aes(
          x=label_position_x,
          y=label_position_y,
          label=label,
          size=4
        )
      )
  }
  transaction_pathways_plot |>
    ggplotly(tooltip=c("label", "count")) |>
    layout(
      showlegend=FALSE,
      xaxis=list(title="", zeroline=FALSE, showticklabels=FALSE),
      yaxis=list(title="", zeroline=FALSE, showticklabels=FALSE)
    ) 
}

pathway_dendrogram_small <- function(layout) {
  node_counts <- layout$nodes
  edges <- layout$edges
  ggplot(node_counts, aes(x=x, y=y)) +
    geom_segment(
      data=edges,
      aes(
        x=x,
        y=y,
        xend=xend,
        yend=yend,
        linewidth=count,
        colour=from_sector_label,
      ),
      alpha=0.1,
      arrow=arrow(ends="last", length=unit(0.2, "inches"))
    ) +
    geom_point(
      aes(
        label=label,
        fill=sector_label,
        size=count
      ),
      pch=21,
      colour="black",
      alpha=0.7
    ) +
    geom_text(
      aes(label=count_label)
    ) +
    scale_x_continuous(expand=c(0.1, 0)) +
    scale_y_continuous(expand=c(0.1, 0)) +
    scale_size_continuous(range=c(5, 20)) +
    scale_linewidth(range=c(1,10)) +
    public_private_fill_scale +
    public_private_colour_scale +
    labs(
      title="Pathways Taken by Collections"
    ) +
    network_theme +
    theme(
      plot.title = element_text(size=14),
      legend.position="None",
      axis.title = element_blank(),
      axis.text = element_blank()
    )
}

get_sequences_layout <- function(sequences,
                                 start_position,
                                 end_position,
                                 grouping_dimension) {
  grouping_dimension <- list(
    "Actor Sector"="sector",
    "Actor Type (Core Categories)"="core_type",
    "Actor Type (Most General)"="general_type",
    "Actor Type (Most Specific)"="type"
  )[grouping_dimension]
  sender_grouping_dimension <- paste0("sender_", grouping_dimension)
  recipient_grouping_dimension <- paste0("recipient_", grouping_dimension)

  from_nodes_counts <- sequences |>
    mutate(
      id=from,
      governance_broad=sender_governance_broad,
      grouping_dimension=.data[[sender_grouping_dimension]],
      position=sender_position
    ) |>
    select(sender_id, sender_quantity, id, governance_broad, grouping_dimension, position, sender_sector) |>
    distinct() |>
    mutate(
      sender_count=ifelse(sender_quantity=="many",2,as.numeric(sender_quantity))
    ) |>
    group_by(id, governance_broad, grouping_dimension, position) |>
    summarize(
      from_count=sum(sender_count),
      from_count_suffix=ifelse("many" %in% sender_quantity, "+", ""),
      from_count_label=paste0(from_count, from_count_suffix),
      public_instances=sum(ifelse(sender_sector=="public", sender_count, 0)),
      university_instances=sum(ifelse(sender_sector=="university", sender_count, 0)),
      third_instances=sum(ifelse(sender_sector=="third", sender_count, 0)),
      private_instances=sum(ifelse(sender_sector=="private", sender_count, 0)),
      hybrid_instances=sum(ifelse(sender_sector=="hybrid", sender_count, 0)),
      public_proportion = public_instances / from_count,
      university_proportion = university_instances / from_count,
      third_proportion = third_instances / from_count,
      private_proportion = private_instances / from_count,
      hybrid_proportion = hybrid_instances / from_count,
      from_sector = case_when(
        public_proportion == 1 ~ "public",
        university_proportion == 1 ~ "university",
        third_proportion == 1 ~ "third",
        private_proportion == 1 ~ "private",
        hybrid_proportion == 1 ~ "hybrid",
        public_proportion >= 0.5 ~ "mostly public",
        university_proportion >= 0.5 ~ "mostly university",
        third_proportion >= 0.5 ~ "mostly third",
        private_proportion >= 0.5 ~ "mostly private",
        hybrid_proportion >= 0.5 ~ "mostly hybrid",
        TRUE ~ "unknown"
      )
    ) |>
    ungroup()

  to_nodes_counts <- sequences |>
    mutate(
      id=to,
      governance_broad=recipient_governance_broad,
      grouping_dimension=.data[[recipient_grouping_dimension]],
      position=recipient_position,
    ) |>
    select(recipient_id, recipient_quantity, id, governance_broad, grouping_dimension, position, recipient_sector) |>
    distinct() |>
    mutate(
      recipient_count=ifelse(recipient_quantity=="many",2,as.numeric(recipient_quantity))
    ) |>
    group_by(id, governance_broad, grouping_dimension, position) |>
    summarize(
      to_count=sum(recipient_count),
      to_count_suffix=ifelse("many" %in% recipient_quantity, "+", ""),
      to_count_label=paste0(to_count, to_count_suffix),
      public_instances=sum(ifelse(recipient_sector=="public", recipient_count, 0)),
      university_instances=sum(ifelse(recipient_sector=="university", recipient_count, 0)),
      third_instances=sum(ifelse(recipient_sector=="third", recipient_count, 0)),
      private_instances=sum(ifelse(recipient_sector=="private", recipient_count, 0)),
      hybrid_instances=sum(ifelse(recipient_sector=="hybrid", recipient_count, 0)),
      public_proportion = public_instances / to_count,
      university_proportion = university_instances / to_count,
      third_proportion = third_instances / to_count,
      private_proportion = private_instances / to_count,
      hybrid_proportion = hybrid_instances / to_count,
      to_sector = case_when(
        public_proportion == 1 ~ "public",
        university_proportion == 1 ~ "university",
        third_proportion == 1 ~ "third",
        private_proportion == 1 ~ "private",
        hybrid_proportion == 1 ~ "hybrid",
        public_proportion >= 0.5 ~ "mostly public",
        university_proportion >= 0.5 ~ "mostly university",
        third_proportion >= 0.5 ~ "mostly third",
        private_proportion >= 0.5 ~ "mostly private",
        hybrid_proportion >= 0.5 ~ "mostly hybrid",
        TRUE ~ "unknown"
      )
    ) |>
    ungroup()

  node_counts <- from_nodes_counts |>
    full_join(to_nodes_counts, by=c("id", "governance_broad", "grouping_dimension", "position")) |>
    mutate(
      count = ifelse(
        is.na(to_count),
        from_count,
        ifelse(
          is.na(from_count), 
          to_count,
          ifelse(
            from_count > to_count,
            from_count,
            to_count
          )
        )
      ),
      count_label = ifelse(
        is.na(to_count),
        from_count_label,
        ifelse(
          is.na(from_count), 
          to_count_label,
          ifelse(
            from_count > to_count,
            from_count_label,
            to_count_label
          )
        )
      ),
      name = paste(governance_broad, grouping_dimension, sep="@"),
      sector_label = ifelse(
        is.na(to_count),
        from_sector,
        ifelse(
          is.na(from_count), 
          to_sector,
          ifelse(
            from_count > to_count,
            from_sector,
            to_sector
          )
        )
      )
    ) |>
    filter(position >= start_position & position <= end_position)

  name_mapping <- node_counts |>
    select(name) |>
    distinct() |>
    mutate(
      name_numeric = as.numeric(factor(name, sector_type_ordering[sector_type_ordering %in% node_counts$name]))
    )
  
  if (grouping_dimension == "sector") {
    name_mapping <- name_mapping |> 
      mutate(
        governance = sapply(strsplit(name, "@"), `[`, 1),
        sector = sapply(strsplit(name, "@"), `[`, 2),
        label = ifelse(
          governance != "NA", 
          paste(governance, "museum"), 
          paste("Other", sector, "sector")
        )
      ) |> 
      select(-governance, -sector)
  } else {
    name_mapping <- name_mapping |> 
      mutate(
        governance = sapply(strsplit(name, "@"), `[`, 1),
        actor_type = sapply(strsplit(name, "@"), `[`, 2),
        label = ifelse(
          governance != "NA", 
          paste(governance, "museum"), 
          actor_type
        )
      ) |> 
      select(-governance, -actor_type)
  }
  
  node_counts <- node_counts |>
    left_join(name_mapping, by = "name")
  
  count_edges <- sequences |>
    select(from, to, .data[[sender_grouping_dimension]], sender_governance_broad, .data[[recipient_grouping_dimension]], recipient_governance_broad) |>
    group_by(from, to, .data[[sender_grouping_dimension]], sender_governance_broad, .data[[recipient_grouping_dimension]], recipient_governance_broad) |>
    summarize(count = n()) |>
    ungroup() |>
    mutate(
      label=count
    )

  size_edges <- sequences |>
    mutate(
      collection_estimated_size = ifelse(is.na(collection_estimated_size), 1, collection_estimated_size)
    ) |>
    select(from, to, .data[[sender_grouping_dimension]], sender_governance_broad, .data[[recipient_grouping_dimension]], recipient_governance_broad, collection_estimated_size) |>
    group_by(from, to, .data[[sender_grouping_dimension]], sender_governance_broad, .data[[recipient_grouping_dimension]], recipient_governance_broad) |>
    summarize(count = sum(collection_estimated_size)) |>
    ungroup() |>
    mutate(
      label=""
    )
  
  max_size_edges <- sequences |>
    mutate(
      collection_estimated_size = ifelse(is.na(collection_estimated_size_max), 1, collection_estimated_size_max)
    ) |>
    select(from, to, .data[[sender_grouping_dimension]], sender_governance_broad, .data[[recipient_grouping_dimension]], recipient_governance_broad, collection_estimated_size) |>
    group_by(from, to, .data[[sender_grouping_dimension]], sender_governance_broad, .data[[recipient_grouping_dimension]], recipient_governance_broad) |>
    summarize(count = sum(collection_estimated_size)) |>
    ungroup() |>
    mutate(
      label=""
    )
  
  min_size_edges <- sequences |>
    mutate(
      collection_estimated_size = ifelse(is.na(collection_estimated_size_min), 1, collection_estimated_size_min)
    ) |>
    select(from, to, .data[[sender_grouping_dimension]], sender_governance_broad, .data[[recipient_grouping_dimension]], recipient_governance_broad, collection_estimated_size) |>
    group_by(from, to, .data[[sender_grouping_dimension]], sender_governance_broad, .data[[recipient_grouping_dimension]], recipient_governance_broad) |>
    summarize(count = sum(collection_estimated_size)) |>
    ungroup() |>
    mutate(
      label=""
    )
  
  edges <- rbind(
    count_edges,
    min_size_edges |> mutate(count = count * 0.25),
    min_size_edges |> mutate(count = count * 0.5),
    min_size_edges |> mutate(count = count * 0.75),
    min_size_edges,
    size_edges |> mutate(count = count * 0.25),
    size_edges |> mutate(count = count * 0.5),
    size_edges |> mutate(count = count * 0.75),
    size_edges,
    max_size_edges |> mutate(count = count * 0.25),
    max_size_edges |> mutate(count = count * 0.5),
    max_size_edges |> mutate(count = count * 0.75),
    max_size_edges
  ) |>
    mutate(
      from_name=paste(sender_governance_broad, .data[[sender_grouping_dimension]], sep="@"),
      to_name=paste(recipient_governance_broad, .data[[recipient_grouping_dimension]], sep="@"),
      label=ifelse(!is.na(label), as.character(label), ""),
      from_position=as.numeric(sapply(str_split(from, "@"), function(x) if(length(x) > 2) x[3] else NA)),
      to_position=as.numeric(sapply(str_split(to, "@"), function(x) if(length(x) > 2) x[3] else NA)),
    ) |>
    filter(from_position >= start_position & to_position <= end_position) |>
    left_join(name_mapping |> select(-label), by = c("from_name" = "name")) |>
    rename(from_name_numeric = name_numeric) |>
    left_join(name_mapping |> select(-label), by = c("to_name" = "name")) |>
    rename(to_name_numeric = name_numeric) |>
    mutate(
      # Calculate direction vector
      dx = to_name_numeric - from_name_numeric,
      dy = to_position - from_position,
      
      # Normalize the direction vector to unit length
      length = sqrt(dx^2 + dy^2),
      ux = dx / length,
      uy = dy / length,
      
      # Calculate perpendicular vectors for control points
      perp_x = -uy,  # Perpendicular vector x component
      perp_y = ux,   # Perpendicular vector y component
      
      # Control points
      control1_x = from_name_numeric + (dx / 3) + (perp_x * 0.5),  # Shift control point 1 perpendicular to the line
      control1_y = from_position + (dy / 3) + (perp_y * 0.5),
      
      control2_x = from_name_numeric + 2 * (dx / 3) - (perp_x * 0.5),  # Shift control point 2 in the opposite perpendicular direction
      control2_y = from_position + 2 * (dy / 3) - (perp_y * 0.5)
    ) |>
    rowwise() |>
    mutate(
      random_offset = runif(n(), min=-0.1, max=0.1),
      gradient = (to_position - from_position) / (to_name_numeric - from_name_numeric),
      label_position_x = mean(c(from_name_numeric, to_name_numeric)),
      label_position_x = ifelse(
          gradient == Inf,
          label_position_x,
          label_position_x + random_offset * (1 / gradient)
      ),
      label_position_y = mean(c(from_position, to_position)) + random_offset,
    ) |>
    left_join(node_counts |> select(from=id, from_sector_label=sector_label), by="from")


  edges_bezier <- edges %>%
    select(from_name_numeric, from_position, control1_x, control1_y, control2_x, control2_y, to_name_numeric, to_position, count, .data[[sender_grouping_dimension]], label) %>%
    pivot_longer(cols = c(from_name_numeric, control1_x, control2_x, to_name_numeric), names_to = "point", values_to = "x") %>%
    pivot_longer(cols = c(from_position, control1_y, control2_y, to_position), names_to = "point_y", values_to = "y") %>%
    filter(
      (point == "from_name_numeric" & point_y == "from_position") |
        (point == "control1_x" & point_y == "control1_y") |
        (point == "control2_x" & point_y == "control2_y") |
        (point == "to_name_numeric" & point_y == "to_position")
    ) %>%
    arrange(.data[[sender_grouping_dimension]], count, label) %>%
    group_by(interaction(count, .data[[sender_grouping_dimension]], label)) %>%
    filter(n() == 4) 

  list("nodes"=node_counts, "edges"=edges)
}

sequence_network <- function(layout, show_transaction_counts) {
  node_counts <- layout$nodes
  edges <- layout$edges
  transaction_sequence_plot <- ggplot(node_counts, aes(x=name_numeric, y=position)) +
    geom_segment(
      data=edges,
      aes(
        x=from_name_numeric,
        y=from_position,
        xend=to_name_numeric,
        yend=to_position,
        linewidth=count,
        colour=from_sector_label,
      ),
      alpha=0.1
    ) +
    # geom_bezier(
    #   data=edges_bezier,
    #   aes(x=x, y=y, group=interaction(count, sender_core_type, label), linewidth=count, colour=sender_core_type),
    #   alpha=0.1
    # ) +
    geom_point(
      aes(
        fill=sector_label,
        size=count
      ),
      pch=21,
      colour="black",
      alpha=0.7
    ) +
    geom_text(aes(label=count_label), size=5) +
    coord_flip() +
    scale_x_continuous(
      name="Actor",
      breaks=name_mapping$name_numeric,
      labels=str_replace_all(name_mapping$label, "_", " "),
      sec.axis=dup_axis(name="Actor")
    ) +
    scale_y_continuous(breaks=start_position:end_position) +
    scale_size_continuous(range=c(5, 20)) +
    scale_linewidth(range=c(1,10)) +
    public_private_fill_scale +
    public_private_colour_scale +
    labs(
      title="Sequential Transactions of Museum Collections",
      x="Actor",
      y="Stage in path"
    ) +
    guides(
      fill=guide_legend(override.aes = list(size = 12)),
      colour="none",
      size="none",
      linewidth="none"
    ) +
    network_theme +
    theme(
      legend.position="None"
    )
  
  if (show_transaction_counts) {
    transaction_sequence_plot <- transaction_sequence_plot +
      geom_text(
        data=edges,
        aes(
          x=label_position_x,
          y=label_position_y,
          label=label,
          size=4
        )
      )
  }
  
  transaction_sequence_plot
}
  
sequence_network_small <- function(layout, start_position, end_position) {
  node_counts <- layout$nodes
  edges <- layout$edges
  ggplot(node_counts, aes(x=name_numeric, y=position)) +
    geom_segment(
      data=edges,
      aes(
        x=from_name_numeric,
        y=from_position,
        xend=to_name_numeric,
        yend=to_position,
        linewidth=count,
        colour=from_sector_label,
      ),
      alpha=0.1
    ) +
    geom_point(
      aes(
        fill=sector_label,
        size=count
      ),
      pch=21,
      colour="black",
      alpha=0.7
    ) +
    geom_text(aes(label=count_label), size=5) +
    coord_flip() +
    scale_size_continuous(range=c(5, 20)) +
    scale_linewidth(range=c(1,10)) +
    public_private_fill_scale +
    public_private_colour_scale +
    labs(
      title="Sequential Transactions of Collections",
      x="Actor",
      y="Stage in path"
    ) +
    guides(
      fill=guide_legend(override.aes = list(size = 12)),
      colour="none",
      size="none",
      linewidth="none"
    ) +
    network_theme +
    theme(
      plot.title = element_text(size=14),
      legend.position="None",
      axis.title = element_text(size=14),
      axis.text = element_blank()
    )
}
