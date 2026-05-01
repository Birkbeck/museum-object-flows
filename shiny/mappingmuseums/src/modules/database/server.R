source("src/modules/database/elements.R")

databaseServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    CURRENT_YEAR <- as.numeric(format(Sys.Date(), "%Y"))

    museum_ids_file <- paste0(data_url, "museum_ids.csv")
    tfidf_file <- paste0(data_url, "tfidf.mtx")
    idf_file <- paste0(data_url, "idf.csv")
    vocab_file <- paste0(data_url, "vocab.csv")

    museum_ids <- read_csv(museum_ids_file, show_col_types = FALSE)$museum_id
    X <- readMM(tfidf_file)
    X <- as(X, "dgCMatrix")  # efficient column-compressed
    vocab <- read_csv(vocab_file, show_col_types = FALSE)$vocab
    idf <- read_csv(idf_file, show_col_types = FALSE)$idf
    term_to_col <- setNames(seq_along(vocab), vocab)

    free_text_search <- reactive({input$freeText})
    accreditation_filter <- reactive({input$accreditationFilter})
    governance_filter <- reactive({input$governanceFilter})
    size_filter <- reactive({input$sizeFilter})
    subject_filter <- reactive({input$subjectFilter})
    subject_specific_filter <- reactive({input$subjectSpecificFilter})
    country_filter <- reactive({input$countryFilter})
    region_filter <- reactive({input$regionFilter})
    lad_filter <- reactive({input$ladFilter})
    address_substring_filter <- reactive({
      gsub("[[:punct:]]", "", input$addressFilter)
    })

    existence_or_open_close <- reactive({input$existenceOrOpenClose})
    # determine opening and closing filters
    opening_range_is_certain <- reactive({
      if (existence_or_open_close() == "Museums that were open in time period") {
        input$existedCertainty == "definitely"
      } else {
        input$openingCertainty == "definitely"
      }
    })
    opening_range_start <- reactive({
      if (existence_or_open_close() == "Museums that were open in time period") {
        "pre-1960"
      } else {
        input$openingStart
      }
    })
    opening_range_end <- reactive({
      if (existence_or_open_close() == "Museums that were open in time period") {
        if (input$existedInclusivity == "inclusive") {
          input$existedEnd
        } else {
          if (input$existedEnd == "pre-1960") {
            1958
          } else {
            as.numeric(input$existedEnd) - 1
          }
        }
      } else {
        input$openingEnd
      }
    })
    opening_range_is_inclusive <- reactive({
      if (existence_or_open_close() == "Museums that were open in time period") {
        TRUE
      } else {
        input$openingInclusivity == "inclusive"
      }
    })
    closing_range_is_certain <- reactive({
      if (existence_or_open_close() == "Museums that were open in time period") {
        input$existedCertainty == "definitely"
      } else {
        input$closingCertainty == "definitely"
      }
    })
    closing_range_start <- reactive({
      if (existence_or_open_close() == "Museums that were open in time period") {
        if (input$existedInclusivity == "inclusive") {
          input$existedStart
        } else {
          as.numeric(input$existedStart) + 1
        }
      } else {
        input$closingStart
      }
    })
    closing_range_end <- reactive({
      if (existence_or_open_close() == "Museums that were open in time period") {
        "never"
      } else {
        input$closingEnd
      }
    })
    closing_range_is_inclusive <- reactive({
      if (existence_or_open_close() == "Museums that were open in time period") {
        TRUE
      } else {
        input$closingInclusivity == "inclusive"
      }
    })

    observeEvent(subject_filter(), {
      freezeReactiveValue(input, "subjectSpecificFilter")
      specific_subjects <- subject_labels_map() |>
        filter(subject_broad %in% subject_filter())
      updatePickerInput(
        session=session,
        inputId="subjectSpecificFilter",
        choices=specific_subjects$subject,
        selected=specific_subjects$subject,
      )
    })

    observeEvent(existence_or_open_close(), {
      if (existence_or_open_close() == "Museums that were open in time period") {
        output$timePeriodSearch <- renderUI({
          tags$div(
            style = "display: flex; align-items: flex-end; gap: 8px;",
            p("Museums that"),
            selectInput(
              NS(id, "existedCertainty"),
              "",
              choices=c("definitely", "possibly"),
              selected="possibly",
              multiple=FALSE,
              width=120
            ),
            p("were open anytime between"),
            selectInput(
              NS(id, "existedStart"),
              "",
              choices=c("pre-1960", seq(1960, CURRENT_YEAR, by=1)),
              selected="1960",
              multiple=FALSE,
              width=120
            ),
            p("and"),
            selectInput(
              NS(id, "existedEnd"),
              "",
              choices=c("pre-1960", seq(1960, CURRENT_YEAR, by=1)),
              selected=CURRENT_YEAR,
              multiple=FALSE,
              width=120
            ),
            selectInput(
              NS(id, "existedInclusivity"),
              "",
              choices=c("inclusive", "exclusive"),
              selected="inclusive",
              multiple=FALSE,
              width=120
            )
          )
        })
      } else {
        output$timePeriodSearch <- renderUI({
          tagList(
            tags$div(
              style = "display: flex; align-items: flex-end; gap: 8px;",
              p("Museums that"),
              selectInput(
                NS(id, "openingCertainty"),
                "",
                choices=c("definitely", "possibly"),
                selected="possibly",
                multiple=FALSE,
                width=120
              ),
              p("opened between"),
              selectInput(
                NS(id, "openingStart"),
                "",
                choices=c("pre-1960", seq(1960, CURRENT_YEAR, by=1)),
                selected="pre-1960",
                multiple=FALSE,
                width=120
              ),
              p("and"),
              selectInput(
                NS(id, "openingEnd"),
                "",
                choices=c("pre-1960", seq(1960, CURRENT_YEAR, by=1)),
                selected=CURRENT_YEAR,
                multiple=FALSE,
                width=120
              ),
              selectInput(
                NS(id, "openingInclusivity"),
                "",
                choices=c("inclusive", "exclusive"),
                selected="inclusive",
                multiple=FALSE,
                width=120
              )
            ),
            tags$div(
              style = "display: flex; align-items: flex-end; gap: 8px;",
              p("Museums that"),
              selectInput(
                NS(id, "closingCertainty"),
                "",
                choices=c("definitely", "possibly"),
                selected="possibly",
                multiple=FALSE,
                width=120
              ),
              p("closed between"),
              selectInput(
                NS(id, "closingStart"),
                "",
                choices=c("never", "pre-1960", seq(1960, CURRENT_YEAR, by=1)),
                selected="pre-1960",
                multiple=FALSE,
                width=120
              ),
              p("and"),
              selectInput(
                NS(id, "closingEnd"),
                "",
                choices=c("never", "pre-1960", seq(1960, CURRENT_YEAR, by=1)),
                selected="never",
                multiple=FALSE,
                width=120
              ),
              selectInput(
                NS(id, "closingInclusivity"),
                "",
                choices=c("inclusive", "exclusive"),
                selected="inclusive",
                multiple=FALSE,
                width=120
              )
            )
          )
        })
      }
    })

    observeEvent(input$reset, {
      updatePickerInput(
        session, "accreditationFilter", selected=accreditation_labels()$label
      )
      updatePickerInput(
        session, "governanceFilter", selected=governance_broad_labels()$label
      )
      updatePickerInput(
        session, "sizeFilter", selected=size_labels()$label
      )
      updatePickerInput(
        session, "subjectFilter", selected=subject_broad_labels()$label
      )
      updatePickerInput(
        session, "subjectSpecificFilter", selected=subject_labels()$label
      )
      updatePickerInput(
        session, "countryFilter", selected=country_labels()$label
      )
      updatePickerInput(
        session, "regionFilter", selected=region_labels()$label
      )
      updateSelectInput(
        session, "ladFilter", selected=lad_labels()$label
      )
      updateTextInput(
        session, "addressFilter", value = ""
      )
      updateRadioButtons(
        session, "existenceOrOpenClose", selected="Museums that were open in time period"
      )
      updateSelectInput(
        session, "existedCertainty", selected="possibly"
      )
      updateSelectInput(
        session, "existedStart", selected="pre-1960"
      )
      updateSelectInput(
        session, "existedEnd", selected=CURRENT_YEAR
      )
      updateSelectInput(
        session, "existedInclusivity", selected="inclusive"
      )
    })

    filtered_museums <- reactive({
      query <- trimws(free_text_search())
      if (nchar(query) < 3) {
        museum_scores <- tibble::tibble(museum_id = museum_ids, score = 1) |> unique()
      } else {
        museum_scores <- score_query(
          free_text_search(),
          museums_search_table(),
          X,
          museum_ids,
          term_to_col,
          idf
        ) |> filter(score > 0.05)
      }
      museums_including_crown_dependencies() |>
        left_join(museum_scores, by="museum_id") |>
        filter(
          score > 0,
          accreditation %in% accreditation_filter(),
          governance_broad %in% governance_filter(),
          size %in% size_filter(),
          subject_broad %in% subject_filter(),
          subject %in% subject_specific_filter(),
          country %in% country_filter(),
          region %in% region_filter(),
          lad %in% lad_filter(),
          (
            grepl(
              address_substring_filter(),
              place,
              ignore.case=TRUE
            )
            | address_substring_filter() == ""
          )
        ) |>
        filter_by_year(
          "opened",
          opening_range_start(),
          opening_range_end(),
          opening_range_is_certain(),
          opening_range_is_inclusive()
        ) |>
        filter_by_year(
          "closed",
          closing_range_start(),
          closing_range_end(),
          closing_range_is_certain(),
          closing_range_is_inclusive()
        ) |>
        arrange(desc(score))
    })

    search_results_columns <- reactive({
      mm_db_choices[mm_db_choices %in% input$tableSelect]
    })

    output$download <- downloadHandler(
      filename = function() {
        paste('mapping-museums-search-results', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(
          filtered_museums() |> select(all_of(search_results_columns())),
          con
        )
      },
      contentType = "text/csv"
    )

    output$searchTable <- renderDT({
      filtered_museums() |>
        mutate(accreditation_number=as.character(accreditation_number)) |>
        select(all_of(search_results_columns()))
    }, options=list(pageLength=100, dom="liptlip"))
    # l = page length menu
    # i = info text
    # p = pagination
    # t = table
    # f = search box

  })
}
