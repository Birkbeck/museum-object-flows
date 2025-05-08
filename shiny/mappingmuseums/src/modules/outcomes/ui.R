outcomesUI <- function(id) {
  fluidPage(
    fluidRow(
      p("What do museums do with their collections when they close? See below for a summary of what initially happens to collections, who initially receives them and how this varies across different types of museum."),
      p("See the table at the bottom of this page for details of each museum and the outcomes of its closure."),
      p("Outcome categories were determined by clustering museums according to the approximate proportion of their collections involved in events of each event type or received by actors of each actor type."),
    ),
    sidebarLayout(
      sidebarPanel(width=3,
        h3("Filter Outcomes"),

        tagList(
          tags$span(
            tags$strong("Outcome type: "),
            tags$i(
              class = "fa fa-info-circle", # Font Awesome info icon
              style = "color: #007bff; cursor: pointer;", # Style for visibility
              `data-toggle` = "popover", # Bootstrap popover attribute
              `data-placement` = "right", # Position above the icon
              title = "Outcome type",
              `data-content` = "<p>Select how outcomes should be displayed on the diagrams.</p><p><strong>Main event:</strong> Show outcomes of closure in terms of events</p><p><strong>Main actor:</strong> Show outcomes of closure in terms of recipients</p>"
            )
          ),
          tags$script(popover_js),
          selectInput(
            NS(id, "outcomeType"),
            label="",
            choices=c("Outcome event type", "Outcome recipient type", "Outcome destination type"),
            selected="Outcome event type"
          )
        ),

        tagList(
          tags$span(
            tags$strong("Show only outcomes: "),
            tags$i(
              class = "fa fa-info-circle", # Font Awesome info icon
              style = "color: #007bff; cursor: pointer;", # Style for visibility
              `data-toggle` = "popover", # Bootstrap popover attribute
              `data-placement` = "right", # Position above the icon
              title = "Show only outcomes",
              `data-content` = "<p>Select which outcomes should appear in the visualizations. Removing some outcomes could improve the readability of charts.</p>"
            )
          ),
          tags$script(popover_js),
          pickerInput(
            NS(id, "outcomeFilter"),
            label="",
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
            tags$strong("Group museums by: "),
            tags$i(
              class = "fa fa-info-circle", # Font Awesome info icon
              style = "color: #007bff; cursor: pointer;", # Style for visibility
              `data-toggle` = "popover", # Bootstrap popover attribute
              `data-placement` = "right", # Position above the icon
              title = "Group museums by",
              `data-content` = "<p>For the 2-dimensional heatmap.</p><p>Select which museum attribute to show on the <i>x</i>-axis.</p>"
            )
          ),
          tags$script(popover_js),
          selectInput(
            NS(id, "museumGrouping"),
            label="",
            choices=c(
              field_names$name,
              "Core reason for closure",
              "Outcome event type",
              "Outcome recipient type",
              "Outcome destination type"
            ),
            selected="Governance"
          )
        ),


        h3("Filter Museums"),
        p("Show only the outcomes of closure for certain types of museum."),

        tagList(
          tags$span(
            tags$strong("Museum governance: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Museum governance",
              `data-content` = "<p>The governance structure of the museum</p>"
            )
          ),
          tags$script(popover_js),
          pickerInput(
            NS(id, "governanceFilter"), 
            "",
            choices=governance_labels$tidy_label,
            selected=governance_labels$tidy_label,
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
            tags$strong("Museum size: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Museum size",
              `data-content` = "<p>The size of the museum. Museum sizes are based on approximate annual visitor numbers:</p><p><strong>Small: </strong>0 - 10,000 annual visitors.</p><p><strong>Medium: </strong>10,000 - 50,000 annual visitors</p><p><strong>Large: </strong>50,000 - 1,000,000 annual visitors.</p><p><strong>Huge: </strong>More than 1,000,000 annual visitors.</p>"
            )
          ),
          tags$script(popover_js),
          pickerInput(
            NS(id, "sizeFilter"), 
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
            tags$strong("Museum subject: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Museum subject",
              `data-content` = "<p>The subject matter of the museum.</p>"
            )
          ),
          tags$script(popover_js),
          pickerInput(
            NS(id, "subjectFilter"), 
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
            tags$strong("Museum subject (specific): "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Museum subject (specific)",
              `data-content` = "<p>Specific categories of museum subject matter.</p>"
            )
          ),
          tags$script(popover_js),
          pickerInput(
            NS(id, "subjectSpecificFilter"), 
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
            tags$strong("Museum country/region: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Museum country or region",
              `data-content` = "<p>Where in the United Kingdom the museum is located.</p>"
            )
          ),
          tags$script(popover_js),
          pickerInput(
            NS(id, "regionFilter"), 
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
            tags$strong("Museum accreditation: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Museum accreditation",
              `data-content` = "<p>Whether or not the museum was accredited at the time of closure.</p>"
            )
          ),
          tags$script(popover_js),
          pickerInput(
            NS(id, "accreditationFilter"), 
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
        div(uiOutput(NS(id, "mainPlotOptions")))
      ),
      mainPanel(
        div(uiOutput(NS(id, "mainPlot")), style = "height: 1200px; width: 100%;"),
        div(uiOutput(NS(id, "mainPlotExplanation")), style = "margin-top: 20px;"),
        fluidRow(
          p("Click on one of the small charts below to see it enlarged in the main panel above.")
        ),
        fluidRow(
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "outcomesBarChartSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "outcomesBarChart")
            ),
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "outcomesHeatmapSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "outcomesHeatmap")
            ),
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "outcomesLineChartSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "outcomesLineChart")
            )
          )
        )
      )
    ),
    fluidRow(
      h3("Outcomes of Museum Closure"),
      downloadButton(NS(id, "downloadOutcomesTable"), label="Download table as CSV")
    ),
    fluidRow(
      DTOutput(NS(id, "closureOutcomesTable"))
    )
  )
}
