library(DT)
library(plotly)
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(htmlwidgets)
library(janitor)
library(ggplot2)
library(igraph)
library(ggraph)
library(ggrepel)
library(tidyverse)
library(readr)
library(shinycssloaders)

source("src/labels.R")
source("src/texts.R")
source("src/mapping_museums_tables.R")
source("src/themes.R")
source("src/calculate_outcomes.R")
source("src/calculate_closure_lengths.R")
source("src/load_data.R")
source("src/ui_elements.R")

source("src/modules/home/ui.R")
source("src/modules/home/server.R")

source("src/modules/about/ui.R")
source("src/modules/about/server.R")

source("src/modules/glossary/ui.R")
source("src/modules/glossary/server.R")

source("src/modules/snapshot/ui.R")
source("src/modules/snapshot/server.R")

source("src/modules/changes/ui.R")
source("src/modules/changes/server.R")

source("src/modules/reasons/ui.R")
source("src/modules/reasons/server.R")

source("src/modules/length/ui.R")
source("src/modules/length/server.R")

source("src/modules/outcomes/ui.R")
source("src/modules/outcomes/server.R")

source("src/modules/events/ui.R")
source("src/modules/events/server.R")

source("src/modules/dispersal/ui.R")
source("src/modules/dispersal/server.R")

source("src/modules/data/ui.R")
source("src/modules/data/server.R")

PRODUCTION <- as.logical(Sys.getenv("PRODUCTION"))

user_base <- readRDS("users.rds")

function(input, output, session) {
  
  # call login module supplying data frame, 
  # user and password cols and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- reactiveVal(FALSE)
  observeEvent(input$logout, {
    logout_init(TRUE)
    session$reload()
  })
  
  output$user_table <- renderTable({
    # use req to only render results when credentials()$user_auth is TRUE
    req(credentials()$user_auth)
    credentials()$info
  })

  observeEvent(credentials()$user_auth, {
    if (!PRODUCTION || credentials()$user_auth) {
      output$appContent <- renderUI({
        fluidPage(
          useShinyjs(),
          actionButton("logout", "Logout"),
          titlePanel(generate_title("Mapping Museums & Museum Closure")),
          tags$head(
            tags$style(type="text/css", ".nav-tabs {font-size: 16px}")
          ),
          tabsetPanel(
            tabPanel(
              tags$span("Home", title="Go back to the home page"),
              homeUI("home")
            ),
            tabPanel(
              tags$span("About", title="About the project"),
              aboutUI("about")
            ),
            tabPanel(
              tags$span("Taxonomies", title="Definitions of key terms and type hierarchies"),
              glossaryUI("glossary"),
            ),
            tabPanel(
              tags$span("Mapping Museums", title=""),
              tabsetPanel(
                tabPanel(
                  tags$span("Sector Snapshot", title="Data on museums open in a chosen time period"),
                  snapshotUI("snapshot")
                ),
                tabPanel(
                  tags$span("Sector Changes", title="Changes in museum numbers over a chosen time period"),
                  changesUI("changes")
                )
              )
            ),
            tabPanel(
              tags$span("Museum Closure", title=""),
              tabsetPanel(
                tabPanel(
                  tags$span("Reasons for Closure", title="Reasons why museums have closed"),
                  reasonsUI("reasons")
                ),
                tabPanel(
                  tags$span("Outcomes of Closure", title="What museums do after closure"),
                  outcomesUI("outcomes")
                )
              )
            ),
            tabPanel(
              tags$span("Collection Disposal", title=""),
              tabsetPanel(
                tabPanel(
                  tags$span("Events after closure", title="What happens after closure"),
                  eventsUI("events")
                ),
                tabPanel(
                  tags$span("Object destinations", title="The flow of collections away from closed museums"),
                  dispersalUI("dispersal")
                ),
                tabPanel(
                  tags$span("Length of dispersal period", title="How long it takes for museums to close"),
                  lengthUI("length")
                ),
                tabPanel(
                  tags$span("About the data", title="An introduction to data concerning collection dispersal"),
                  dataUI("data")
                )
              )
            )
          )
        )
      })
    } else {
      output$app_content <- renderUI({
        h3("Please log in.")
      })
    }
  })
  
  observeEvent(credentials()$user_auth, {
    if (!PRODUCTION | credentials()$user_auth) {
      
      small_chart_size <- 300
      x_labels <- reactive({c(
        "start_total"=paste("Open Museums at start of", input$year_range[1]),
        "end_total"=paste("Open Museums at end of", input$year_range[2]),
        "openings"=paste0("New Museum Openings ", input$year_range[1], "-", input$year_range[2]),
        "closures"=paste0("Museum Closures ", input$year_range[1], "-", input$year_range[2]),
        "change"=paste0("Change in Museum Numbers ", input$year_range[1], "-", input$year_range[2]),
        "change_pc"=paste0("Percentage Change in Museums ", input$year_range[1], "-", input$year_range[2])
      )})
      y_labels <- c(
        "No filter"="All Museums",
        "size"="Museum Size",
        "governance"="Museum Governance",
        "accreditation"="Museum Accreditation",
        "main_subject"="Subject Matter",
        "region"="Country/Region"
      )
      
      homeServer("home")
      glossaryServer("glossary")

      # mapping museums
      snapshotServer("snapshot")
      changesServer("changes")

      # museum closure
      reasonsServer("reasons")
      outcomesServer("outcomes")

      # collection dispersal
      eventsServer("events")
      dispersalServer("dispersal")
      lengthServer("length")
      dataServer("data")
      
    }
  })
}
