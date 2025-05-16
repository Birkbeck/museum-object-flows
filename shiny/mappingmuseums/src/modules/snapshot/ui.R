snapshotUI <- function(id) {
  fluidPage(

    fluidRow(
      p(
        "The distribution of different types of museum across the UK in a single year or during a selected time period."
      ),
      p(
        "If viewing museums in a single year, charts display the number of museums open at the end of that year. If viewing museums during a time period, charts display the number of museums open for at least some of the period."
      ),
      p(
        "The numbers in the charts show the estimated number of museums. Estimates take into account uncertain opening and closure dates."
      ),
    ),

    sidebarLayout(
      sidebarPanel(
        width=3,
        style = "height: 90vh; overflow-y: auto;",

        h3("View"),

        tagList(
          tags$span(
            tags$strong("Single year or range of years: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Single year or range of years",
              `data-content` = "<p><strong>Single year:</strong> Select to view the museums that were open at the end of the specified year.</p><p><strong>Range of years:</strong> Select to view the museums that were open during at least part of the specified range of years.</p>"
            )
          ),
          tags$script(popover_js),
          radioButtons(
            NS(id, "yearOrRange"),
            label="",
            choices=c("Single year", "Range of years"),
            selected="Single year",
            inline=TRUE
          ),
        ),


        uiOutput(NS(id, "yearSlider")),

        tagList(
          tags$span(
            tags$strong("Main Axis: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Main Axis",
              `data-content` = "<p>Select which attribute to group museums by</p>"
            )
          ),
          tags$script(popover_js),
          selectInput(
            NS(id, "mainAxis"),
            label="",
            choices=c("No filter", field_names$name),
            selected="Governance"
          )
        ),

        tagList(
          tags$span(
            tags$strong("Secondary Axis: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Secondary Axis",
              `data-content` = "<p>Select a second museum attribute to group museums by on the horizontal axis of the heatmap.</p>"
            )
          ),
          tags$script(popover_js),
          selectInput(
            NS(id, "secondAxis"),
            label="",
            choices=field_names$name,
            selected="Country/Region"
          )
        ),

        uiOutput(NS(id, "mainPlotOptions")),

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
        )
      ),

      mainPanel(
        plotlyOutput(NS(id, "mainPlot"), height="800px", width="100%"),
        uiOutput(NS(id, "mainPlotExplanation")),
        fluidRow(
          p("Click on one of the small charts below to see it enlarged in the main panel above.")
        ),
        fluidRow(
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "museumMapSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "museumMap")
            ),
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "museumCountsSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "museumCounts")
            ),
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "museumHeatmapSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "museumHeatmap")
            )
          )
        )
      )
    ),
    fluidRow(
      h3("Museums Open During Period"),
      downloadButton(NS(id, "downloadSnapshotTable"), label="Download table as CSV"),
      DTOutput(NS(id, "openMuseumsTable"))
    )
  )
}
