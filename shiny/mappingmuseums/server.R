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
library(tidyverse)
library(readr)

source("src/labels.R")
source("src/mapping_museums_tables.R")
source("src/themes.R")
source("src/load_data.R")

source("src/modules/dispersal_filters.R")
source("src/modules/home.R")
source("src/modules/snapshot.R")
source("src/modules/changes.R")
source("src/modules/causes.R")
source("src/modules/length.R")
source("src/modules/actors.R")
source("src/modules/events.R")
source("src/modules/pathways.R")
source("src/modules/sequences.R")
source("src/modules/movements.R")

PRODUCTION <- FALSE
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
          actionButton("logout", "Logout"),
          titlePanel(generate_title("Mapping Museums & Museum Closure")),
          tabsetPanel(
            tabPanel(
              "Home",
              homeUI("home"),
            ),
            tabPanel(
              "Sector Snapshot",
              snapshotUI("snapshot"),
            ),
            tabPanel(
              "Sector Changes",
              changesUI("changes"),
            ),
            tabPanel(
              "Reasons for Closure",
              causesUI("causes"),
            ),
            tabPanel(
              "Length of Closure",
              lengthUI("length"),
            ),
            tabPanel(
              "Dispersal: Actors",
              actorsUI("actors"),
            ),
            tabPanel(
              "Dispersal: Events",
              eventsUI("events"),
            ),
            tabPanel(
              "Dispersal: Pathways",
              pathwaysUI("pathways"),
            ),
            tabPanel(
              "Dispersal: Sequences",
              sequencesUI("sequences")
            ),
            tabPanel(
              "Dispersal: Movements",
              movementsUI("movements")
            ),
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
      snapshotServer("snapshot")
      changesServer("changes")
      causesServer("causes")
      lengthServer("length")
      actorsServer("actors")
      eventsServer("events")
      pathwaysServer("pathways")
      sequencesServer("sequences")
      movementsServer("movements")
      
    }
  })
}
