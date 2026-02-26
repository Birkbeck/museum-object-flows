outcomesUI <- function(id) {
  fluidPage(

    text_box(top_outcomes),

    sidebarLayout(
      sidebarPanel(
        width=3,
        style = sidebar_style,

        div(class="scroll-hint", "▼ Scroll for more options"),

        div(
          style = "text-align: right;",
          actionButton(NS(id, "reset"), "Reset options")
        ),

        form_subtitle("View", tooltip_view),

        div(uiOutput(NS(id, "mainPlotOptions"))),

        form_item(
          "Main axis",
          tooltip_main_attribute_outcomes,
          radioButtons(
            NS(id, "outcomeType"),
            label="",
            choices=c(
              "Collection outcome",
              "Collection recipient",
              "Collection recipient count",
              "Collection recipient share",
              "Collection destination"
            ),
            selected="Collection outcome"
          )
        ),

        shinyjs::hidden(
          tags$div(
            id=NS(id, "museumGroupingFormItem"),
            form_item(
              "Museums attribute",
              tooltip_secondary_attribute,
              radioButtons(
                NS(id, "museumGrouping"),
                label="",
                choices=c(
                  field_names()$name,
                  "Core reason for closure",
                  "Collection outcome",
                  "Collection recipient",
                  "Collection recipient count",
                  "Collection recipient share",
                  "Collection destination"
                ),
                selected="Governance"
              )
            )
          )
        ),

        form_subtitle("Filter", tooltip_filter),

        form_item(
          "Museum accreditation",
          tooltip_museum_accreditation,
          pickerInput(
            NS(id, "accreditationFilter"), 
            "",
            choices=accreditation_labels()$label,
            selected=accreditation_labels()$label,
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
            choices=governance_broad_labels()$label,
            selected=governance_broad_labels()$label,
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
            choices=size_labels()$label,
            selected=size_labels()$label,
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
            choices=subject_broad_labels()$label,
            selected=subject_broad_labels()$label,
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
            choices=region_labels()$label,
            selected=region_labels()$label,
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
        div(
          withSpinner(uiOutput(NS(id, "mainPlot"))),
          style = "height: 1200px; width: 100%;"
        ),
        fluidRow(
          column(
            3,
            style=card_style,
            withSpinner(
              plotOutput(
                NS(id, "outcomesBarChartSmall"),
                width=small_chart_size_px,
                height=small_chart_size_px,
                click=NS(id, "outcomesBarChart")
              )
            )
          ),
          column(
            3,
            style=card_style,
            withSpinner(
              plotOutput(
                NS(id, "outcomesHeatmapSmall"),
                width=small_chart_size_px,
                height=small_chart_size_px,
                click=NS(id, "outcomesHeatmap")
              )
            )
          ),
          column(
            3,
            style=card_style,
            withSpinner(
              plotOutput(
                NS(id, "outcomesLineChartSmall"),
                width=small_chart_size_px,
                height=small_chart_size_px,
                click=NS(id, "outcomesLineChart")
              )
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
