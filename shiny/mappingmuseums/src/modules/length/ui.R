lengthUI <- function(id) {
  fluidPage(
    fluidRow(
      p("How long does it take for museums to close? See the charts below to see how length of closure varies by museum type and over time."),
      p("See the table at the bottom of this page for details of each museum and how long its closure took.")
    ),
    sidebarLayout(
      sidebarPanel(
        width=3,
        style = "height: 90vh; overflow-y: auto;",

        h3("View"),

        div(uiOutput(NS(id, "mainPlotOptions"))),

        h3("Grouping"),

        tagList(
          tags$span(
            tags$strong("Group museums by:"),
            tags$i(
              class = "fa fa-info-circle", # Font Awesome info icon
              style = "color: #007bff; cursor: pointer;", # Style for visibility
              `data-toggle` = "popover", # Bootstrap popover attribute
              `data-placement` = "right", # Position above the icon
              title = "Group museums by",
              `data-content` = "<p>Select which attribute museums should be grouped by.</p>"
            )
          ),
          tags$script(popover_js),
          selectInput(
            NS(id, "museumGrouping"),
            label="",
            choices=c(field_names$name),
            selected="All"
          )
        ),

        h3("Filters"),

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

        tagList(
          tags$span(
            tags$strong("Example museum: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Example museum",
              `data-content` = "<p>Select a museum to display its closure timelines.</p>"
            )
          ),
          tags$script(popover_js),
          virtualSelectInput(
            NS(id, "exampleMuseum"),
            "",
            choices=NULL,
            selected=NULL,
            multiple=FALSE,
            disableSelectAll=FALSE,
            search=TRUE
          )
        )
      ),
      mainPanel(
        plotlyOutput(NS(id, "mainPlot"), height="720px", width="100%"),
        uiOutput(NS(id, "mainPlotExplanation")),
        fluidRow(
          p("Click on one of the small charts below to see it enlarged in the main panel above.")
        ),
        fluidRow(
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "lengthTileChartSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "lengthTileChart")
            ),
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "lengthLineChartSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "lengthLineChart")
            ),
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "lengthScatterSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "lengthScatter")
            ),
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "exampleTimelinesSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "exampleTimelines")
            )
          )
        )
      )
    ),
    fluidRow(
      h3("Lengths of Closure"),
      downloadButton(NS(id, "downloadLengthsTable"), label="Download table as CSV")
    ),
    fluidRow(
      DTOutput(NS(id, "closureLengthsTable"))
    )
  )
}
