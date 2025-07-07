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
          "<p><strong>Steps in path:</strong> View intermediate actors in the sequences of ownership and/or custody changes</p><p><strong>First and last actors:</strong> View only the initial museum and the last known actor in the sequence.</p>",
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
          "<p>Select the start and end point of sequences. Step 1 shows the initial museums where collections originated.</p><p>Use the slider to increase the number of steps away from the museum shown on the diagram.</p>",
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
          "Transactions involving collections originating from Firepower are automatically excluded from the diagrams. Switch on in order to include them.",
          switchInput(
            NS(id, "firepower"),
            value=FALSE
          )
        ),

        form_item(
          "Group actors by",
          "<p>Select how to group and display actors on the diagram.</p><p>Museums from the Mapping Museums Database are always grouped according to governance.</p><p>Other actors can be grouped according to:</p><p><strong>Actor sector:</strong> The sector of the economy (<i>public</i>, <i>private</i>, <i>third</i>, <i>etc.</i>) that they belong to.</p><p><strong>Most specific actor type:</strong> The most specific actor type that actors are known to belong to.</p><p><strong>Core category actor type:</strong> The core category that they belong to. Refer to the actor types hierarchy to see which types are included as core categories.</p><p><strong>Most general actor type:</strong> The most general actor type that actors are known to belong to (<i>organisation</i>, <i>individual</i>, or <i>historic house</i>).</p>",
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
          "Group museums by",
          "<p>Select how to group and display museums on the diagram.</p><p>Museums from the Mapping Museums Database are always grouped according to governance.</p><p>Other actors can be grouped according to:</p><p><strong>Actor sector:</strong> The sector of the economy (<i>public</i>, <i>private</i>, <i>third</i>, <i>etc.</i>) that they belong to.</p><p><strong>Most specific actor type:</strong> The most specific actor type that actors are known to belong to.</p><p><strong>Core category actor type:</strong> The core category that they belong to. Refer to the actor types hierarchy to see which types are included as core categories.</p><p><strong>Most general actor type:</strong> The most general actor type that actors are known to belong to (<i>organisation</i>, <i>individual</i>, or <i>historic house</i>).</p>",
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
          "<p>Select which transactions should appear on the diagram.</p><p>Most <strong>changes of ownership</strong> are also changes of custody, but occasionally an item is sold without being sent to its new owner.</p><p><strong>Changes of custody</strong> include a wider range of movements than changes of ownership (e.g. <i>loaned</i> and <i>stored</i>).</p><p><strong>End of existence</strong> is represented as a transfer to no recipient.</p>",
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
          "<p>Select which (core category) event types should appear on the diagram.</p><p>Refer to the events tab to see which events belong to each core category and for a definition of each event type.</p>",
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
          "<p>Filter for events with certain or uncertain types.</p><p><strong>Certain: </strong>Events where the type of event is certain.</p><p><strong>?+: </strong>Events where the type of event is highly likely.</p><p><strong?: </strong>Events where the type of event is probable.</p><p><strong>?-: Events where the type of event is possible.</p>",
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
           "Status of collection involved in events",
           "<p><strong>Items from a museum's collection: </strong>Items that originally formed part of a now closed museum's collection</p><p><strong>Items loaned to a museum: </strong>Loaned items that were in the custody of a museum when it closed</p><p><strong>Items for handling: </strong>Items that belonged to a now closed museum, but that were not part of the official collection and could be handled for educational purposes.</p><p><strong>Other items: </strong>Items that belonged to a now closed museum, but that were not museum objects. For example display cases and other furniture.</p>",
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
          "<p>The initial closed museums from which the depicted sequences begin.</p><p>This field updates with a list of museums according to the filters below.</p><p>It is possible to search for and select an individual museum so that only collection transfers starting at that museum are shown in the diagram.</p>",
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
           "<p>The governance structure of the museum</p>",
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
          "<p>The size of the initial museum. Museum sizes are based on approximate annual visitor numbers:</p><p><strong>Small: </strong>0 - 10,000 annual visitors.</p><p><strong>Medium: </strong>10,000 - 50,000 annual visitors</p><p><strong>Large: </strong>50,000 - 1,000,000 annual visitors.</p><p><strong>Huge: </strong>More than 1,000,000 annual visitors.</p>",
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
         "<p>The subject matter of the initial museum.</p>",
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
           "<p>Specific categories of museum subject matter.</p>",
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
         "Initial museum country/region",
         "<p>Where in the United Kingdom the museum is located.</p>",
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
           "<p>Whether or not the museum was accredited at the time of closure.</p>",
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
          "<p>The final actor in the sequence of transfers. The values in this field update according to how actors are grouped on the diagram.</p>",
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
          "<p>Filter sequences that only pass through specified actors at some point in the sequence of transfers. The values in this field update according to how actors are grouped on the diagram.</p>",
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
        img(src='actor-sector-key.png', align="left", width="150px"),
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
