reasonsUI <- function(id) {
  fluidPage(

    text_box("REASONS-TOP"),

    sidebarLayout(
      sidebarPanel(
        width=3,
        style = sidebar_style,

        div(
          style = "text-align: right;",
          actionButton(NS(id, "reset"), "Reset options")
        ),

        form_subtitle("View", tooltip_view),

        div(uiOutput(NS(id, "mainPlotOptions"))),

        form_item(
          "Museums attribute (for heatmaps only)",
          tooltip_secondary_attribute,
          disabled(
            selectInput(
              NS(id, "museumGrouping"),
              label="",
              choices=field_names$name,
              selected="Governance"
            )
          )
        ),

        form_item(
          "Reasons - level of detail",
          tooltip_reason_type_level,
          selectInput(
            NS(id, "reasonLevel"),
            label="",
            choices=c("Core categories", "Core categories and their sub-categories", "Most specific"),
            selected="Core categories"
          )
        ),

        form_subtitle("Filter", tooltip_filter),

        form_item(
          "Reason core category",
          tooltip_reason_filter,
          pickerInput(
            NS(id, "reasonFilter"),
            label="",
            choices=reason_core_labels$label,
            selected=reason_core_labels$label,
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          ) 
        ),

        form_item(
          "Museum governance",
          tooltip_museum_governance,
          pickerInput(
            NS(id, "governanceFilter"), 
            "",
            choices=governance_broad_labels$label,
            selected=governance_broad_labels$label,
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          ) 
        ),
        
        form_item(
          "Museum size",
          tooltip_museum_size,
          pickerInput(
            NS(id, "sizeFilter"), 
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
          "Museum subject",
          tooltip_museum_subject,
          pickerInput(
            NS(id, "subjectFilter"), 
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
          "Museum subject (specific)",
          tooltip_museum_subject_specific,
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
        
        form_item(
          "Museum location",
          tooltip_museum_country_region,
          pickerInput(
            NS(id, "regionFilter"), 
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
          "Museum accreditation",
          tooltip_museum_accreditation,
          pickerInput(
            NS(id, "accreditationFilter"), 
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
        )
      ),

      mainPanel(
        uiOutput(NS(id, "errorMessage")),
        div(uiOutput(NS(id, "mainPlot")), style = "height: 1200px; width: 100%;"),
        div(uiOutput(NS(id, "mainPlotExplanation")), style = "margin-top: 20px;"),
        fluidRow(
          text_box("REASONS-BOTTOM Click on one of the small charts below to see it enlarged in the main panel above.")
        ),
        fluidRow(
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "reasonsBarChartSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "reasonsBarChart")
            ),
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "reasonsHeatmapSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "reasonsHeatmap")
            ),
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "reasonsLineChartSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "reasonsLineChart")
            )
          )
        )
      )
    ),
    fluidRow(
      h3("Reasons for Closure"),
      downloadButton(NS(id, "downloadReasonsTable"), label="Download table as CSV")
    ),
    fluidRow(
      DTOutput(NS(id, "closureReasonsTable"))
    )
  )
}
