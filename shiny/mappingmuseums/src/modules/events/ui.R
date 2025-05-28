eventsUI <- function(id) {
  fluidPage(

    text_box("EVENTS-TOP"),
    
    sidebarLayout(
      sidebarPanel(
        width=3,
        style = "height: 90vh; overflow-y: auto;",

        div(
          style = "text-align: right;",
          actionButton(NS(id, "reset"), "Reset options")
        ),

        h3("View"),

        tagList(
          tags$span(
            tags$strong("Main axis: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Main axis",
              `data-content` = "<p>Select which part of the event to view on the y-axis</p>"
            )
          ),
          tags$script(popover_js),
          selectInput(
            NS(id, "yAxis"),
            label="",
            choices=c("Event type", "Sender type", "Recipient type", "Collection type", "Initial museum"),
            selected="Sender type"
          )
        ),
        
        tagList(
          tags$span(
            tags$strong("Secondary axis: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Secondary axis",
              `data-content` = "<p>Select which part of the event to view on the x-axis</p>"
            )
          ),
          tags$script(popover_js),
          selectInput(
            NS(id, "xAxis"),
            label="",
            choices=c("Event type", "Sender type", "Recipient type", "Collection type", "Initial museum"),
            selected="Event type"
          )
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
            NS(id, "stepsOrLast"),
            label="",
            choices=c("Stepwise events", "Last known event"),
            selected="Stepwise events",
            inline=TRUE
          )
        ),
        tagList(
          tags$span(
            tags$strong("Stepwise events: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Stepwise events",
              `data-content` = "<p>Select the start and end point of sequences. Step 1 shows the initial museums where collections originated.</p><p>Use the slider to increase the number of steps away from the museum shown on the diagram.</p>"
            )
          ),
          tags$script(popover_js),
          pickerInput(
            NS(id, "stagesInPath"),
            "",
            choices=c(),
            selected=c(),
            options=pickerOptions(
              actionsBox=TRUE,
              size=10,
              selectedTextFormat="count > 7"
            ),
            multiple=TRUE
          )
        ),

        tagList(
          tags$span(
            tags$strong("Counts or percentages: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Display counts or percentages",
              `data-content` = "
<p><strong>Number of events:</strong> The number of events with the event/participant types</p>
<p><strong>Percentage of events:</strong> The percentage of all events with the event/participant types.</p>
<p><strong>Rowwise percentages:</strong> The percentage of all events with each y-axis attribute that have each x-axis attribute</p>
<p><strong>Columnwise percentages:</strong> The percentage of all events with each x-axis attribute that have each y-axis attribute</p>
"
            )
          ),
          tags$script(popover_js),
          radioButtons(
            inputId = NS(id, "countOrPercentage"),
            label = "",
            choices = list(
              "Show number of events" = "count",
              "Show percentage of events" = "percentage",
              "Show rowwise percentages" = "percentage_rowwise",
              "Show columnwise percentages" = "percentage_columnwise"
            )
          )
        ),

        h3("Grouping"),

        tagList(
          tags$span(
            tags$strong("Group events by: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Event Hierarchy Level",
              `data-content` = "<p>Select which level of the event hierarchy should be used to classify events.</p>"
            )
          ),
          tags$script(popover_js),
          selectInput(
            NS(id, "eventGrouping"),
            label="",
            choices=c("Most general", "Core categories", "Most specific"),
            selected="Core categories"
          )
        ),

        tagList(
          tags$span(
            tags$strong("Group actors by: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Actor Hierarchy Level",
              `data-content` = "<p>Select which level of the actor hierarchy should be used to classify actors.</p>"
            )
          ),
          tags$script(popover_js),
          selectInput(
            NS(id, "actorGrouping"),
            label="",
            choices=c("Most general", "Core categories", "Most specific"),
            selected="Core categories"
          )
        ),
        
        tagList(
          tags$span(
            tags$strong("Group museums by: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Group museums",
              `data-content` = "<p>Select which attribute museums should be grouped by</p>"
            )
          ),
          tags$script(popover_js),
          selectInput(
            NS(id, "museumGrouping"),
            label="",
            choices=field_names$name,
            selected="Governance"
          )
        ),
        
        h3("Filters"),
        
        tagList(
          tags$span(
            tags$strong("Event type: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Event type",
              `data-content` = "<p>Show only events of a specified type.</p>"
            )
          ),
          tags$script(popover_js),
          pickerInput(
            NS(id, "eventTypeFilter"), 
            "",
            choices=c(),
            selected=c(),
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
            tags$strong("Sender type: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Sender type",
              `data-content` = "<p>Show only events with a sender of a specified type.</p>"
            )
          ),
          tags$script(popover_js),
          pickerInput(
            NS(id, "senderTypeFilter"), 
            "",
            choices=c(),
            selected=c(),
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
            tags$strong("Recipient type: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Recipient type",
              `data-content` = "<p>Show only events with a recipient of a specified type.</p>"
            )
          ),
          tags$script(popover_js),
          pickerInput(
            NS(id, "recipientTypeFilter"), 
            "",
            choices=c(),
            selected=c(),
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
            tags$strong("Collection type: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Collection type",
              `data-content` = "<p>Show only events with a collection of a specified type.</p><p>Some collections have more than one type. Any collection which has at least one of the types selected in this filter will contribute to the chart.</p>"
            )
          ),
          tags$script(popover_js),
          pickerInput(
            NS(id, "collectionTypeFilter"), 
            "",
            choices=collection_types$collection_type,
            selected=collection_types$collection_type,
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
            tags$strong("Collection status: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Collection status",
              `data-content` = "<p>Show only events with a collection of a specified status.</p>"
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
        
        tagList(
          tags$span(
            tags$strong("Initial museum governance: "),
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
            choices=filter(governance_labels, internal_label != "Independent")$tidy_label,
            selected=filter(governance_labels, internal_label != "Independent")$tidy_label,
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
            tags$strong("Initial museum subject: "),
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
            tags$strong("Initial museum subject (specific): "),
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
            tags$strong("Initial museum country/region: "),
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
            choices=filter(country_region_labels, internal_label != "England")$tidy_label,
            selected=filter(country_region_labels, internal_label != "England")$tidy_label,
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
        plotlyOutput(NS(id, "mainPlot"), width="100%", height="1200px"),
        div(uiOutput(NS(id, "mainPlotExplanation")), style = "margin-top: 20px;")
      )
    ),
    
    fluidRow(
      h3("Events Involving Museum Collections"),
      pickerInput(
        NS(id, "tableSelect"),
        label="show columns:",
        choices=events_table_choices,
        selected=events_table_selected,
        options = pickerOptions(
          actionsBox = TRUE, 
          size = 10,
          selectedTextFormat = "count > 3"
        ), 
        multiple = TRUE
      ),
      downloadButton(NS(id, "downloadEventsTable"), label="Download table as CSV")
    ),
    fluidRow(
      DTOutput(NS(id, "eventsTable"))
    )
  )
}
