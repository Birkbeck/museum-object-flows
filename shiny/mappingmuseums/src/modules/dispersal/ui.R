dispersalUI <- function(id) {
  fluidPage(

    text_box("DISPERSAL-TOP"),

    sidebarLayout(
      sidebarPanel(
        width=3,
        style = sidebar_style,

        div(
          style = "text-align: right;",
          actionButton(NS(id, "reset"), "Reset options")
        ),

        form_subtitle("View", tooltip_view),

        form_item(
          "Display",
          tooltip_steps_or_first_last,
          radioButtons(
            NS(id, "stepsOrFirstLast"),
            label="",
            choices=c("Steps in path", "First and last actors"),
            selected="Steps in path",
            inline=TRUE
          )
        ),

        form_item(
          "Steps in path",
          tooltip_steps_in_path,
          sliderInput(
            NS(id, "stagesInOwnershipPath"),
            label="",
            value=2,
            min=1,
            max=6,
            step=1,
            ticks=FALSE,
            width="50%"
          )
        ),

        checkboxInput(
          NS(id, "showTransactionCounts"),
          label="Show Transaction Counts",
          value=FALSE
        ),

        uiOutput(NS(id, "mainPlotOptions")),

        form_item(
          "!! Firepower",
          tooltip_include_firepower,
          switchInput(
            NS(id, "firepower"),
            value=FALSE
          )
        ),

        form_item(
          "Actors - level of detail",
          tooltip_group_actors_by,
          selectInput(
            NS(id, "grouping"),
            label="",
            choices=c(
              "Actor sector",
              "Actor type (core categories)",
              "Actor type (most specific)",
              "Actor region/country"
            ),
            selected="Actor type (core categories)"
          )
        ),
            
        form_item(
          "Museums attribute",
          tooltip_group_actors_by,
          selectInput(
            NS(id, "groupingMuseums"),
            label="",
            choices=field_names$name,
            selected="Governance"
          )
        ),

        form_subtitle("Filter", tooltip_filter),

        form_item(
          "Show transfer types",
          tooltip_transaction_types,
          pickerInput(
            NS(id, "transactionTypeFilter"), 
            label="",
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

        form_item(
          "Show event types",
          tooltip_event_types,
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

        form_item(
          "Show events with uncertainty level",
          tooltip_event_uncertainty,
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

        form_item(
           "Collection status",
           tooltip_collection_status,
           pickerInput(
             NS(id, "collectionStatusFilter"), 
             "", 
             choices=collection_status_labels$label,
             selected=collection_status_labels$label,
             options=pickerOptions(
               actionsBox=TRUE, 
               size=10,
               selectedTextFormat="count > 3"
             ), 
             multiple=TRUE
           )   
        ),
            
        form_item(
          "Initial museum",
          tooltip_initial_museum,
          virtualSelectInput(
            NS(id, "initialMuseum"),
            "",
            choices=NULL,
            selected=NULL,
            multiple=TRUE,
            disableSelectAll=FALSE,
            search=TRUE
          )
        ),

        form_item(
           "Initial museum governance",
           tooltip_museum_governance,
           pickerInput(
             NS(id, "startGovernanceFilter"), 
             "",
             choices=governance_broad_labels$label,
             selected=c("local authority"),
             options=pickerOptions(
               actionsBox=TRUE, 
               size=10,
               selectedTextFormat="count > 3"
             ), 
             multiple=TRUE
           ) 
        ),

        form_item(
          "Initial museum size",
          tooltip_museum_size,
          pickerInput(
            NS(id, "startSizeFilter"), 
            "",
            choices=size_labels$label,
            selected=size_labels$label,
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          ) 
        ),

        form_item(
         "Initial museum subject",
         tooltip_museum_subject,
         pickerInput(
           NS(id, "startSubjectFilter"), 
           "",
           choices=subject_broad_labels$label,
           selected=subject_broad_labels$label,
           options=pickerOptions(
             actionsBox=TRUE, 
             size=10,
             selectedTextFormat="count > 3"
           ), 
           multiple=TRUE
         )  
        ),
            
        form_item(
          "Initial museum subject (specific)",
          tooltip_museum_subject_specific,
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

        form_item(
         "Initial museum location",
         tooltip_museum_country_region,
         pickerInput(
           NS(id, "startRegionFilter"), 
           "",
           choices=region_labels$label,
           selected=region_labels$label,
           options=pickerOptions(
             actionsBox=TRUE, 
             size=10,
             selectedTextFormat="count > 3"
           ), 
           multiple=TRUE
         )   
        ),

        form_item(
          "Initial museum accreditation",
          tooltip_museum_accreditation,
          pickerInput(
            NS(id, "startAccreditationFilter"), 
            "",
            choices=accreditation_labels$label,
            selected=accreditation_labels$label,
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          )   
        ),

        form_item(
          "Final destination",
          tooltip_final_destination,
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

        form_item(
          "Passes through",
          tooltip_passes_through,
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
        )

      ),

      mainPanel(
        uiOutput(NS(id, "errorMessage")),
        plotlyOutput(NS(id, "mainPlot"), width="100%", height="850px"),
        fluidRow(
          img(src='actor-sector-key.png', align="left", width="450px")
        ),
        fluidRow(
          text_box("DISPERSAL-BOTTOM Click on one of the small charts below to see it enlarged in the main panel above.")
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

    fluidRow(
      h3("Sequences Data"),
      virtualSelectInput(
        NS(id, "tableSelect"),
        label="show columns:",
        choices=sequences_table_choices,
        selected=sequences_table_selected,
        multiple=TRUE,
        disableSelectAll=FALSE,
        search=TRUE
      ), 
      downloadButton(NS(id, "downloadSequencesTable"), label="Download table as CSV")
    ),

    fluidRow(
      DTOutput(NS(id, "pathwaysTable"))
    )

  )
}
