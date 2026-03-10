make_char_wb_ngrams <- function(text, ngram_range = c(3L, 5L)) {
  text <- tolower(text)
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)

  if (!nzchar(text)) {
    return(character(0))
  }

  words <- unlist(strsplit(text, " ", fixed = TRUE), use.names = FALSE)
  words <- words[nzchar(words)]

  if (!length(words)) {
    return(character(0))
  }

  min_n <- as.integer(ngram_range[1])
  max_n <- as.integer(ngram_range[2])

  grams <- character(0)

  for (word in words) {
    padded <- paste0(" ", word, " ")
    n_chars <- nchar(padded, type = "chars")

    for (n in seq.int(min_n, max_n)) {
      if (n > n_chars) next
      starts <- seq_len(n_chars - n + 1L)
      grams <- c(
        grams,
        substring(padded, starts, starts + n - 1L)
      )
    }
  }

  grams
}

make_query_vec <- function(query, term_to_col, idf, ncol_X, ngram_range = c(3L, 5L)) {
  grams <- make_char_wb_ngrams(query, ngram_range = ngram_range)

  cols <- term_to_col[grams]
  cols <- cols[!is.na(cols)]

  if (!length(cols)) {
    return(
      Matrix::sparseVector(
        i = integer(0),
        x = numeric(0),
        length = ncol_X
      )
    )
  }

  tab <- table(cols)
  j <- as.integer(names(tab))
  tf <- as.numeric(tab)

  x <- tf * idf[j]

  norm <- sqrt(sum(x^2))
  if (norm > 0) {
    x <- x / norm
  }

  Matrix::sparseVector(i = j, x = x, length = ncol_X)
}

score_query <- function(query, X, museum_ids, term_to_col, idf, ngram_range = c(3L, 5L)) {
  qv <- make_query_vec(
    query = query,
    term_to_col = term_to_col,
    idf = idf,
    ncol_X = ncol(X),
    ngram_range = ngram_range
  )

  if (length(qv@x) == 0) {
    return(
      tibble::tibble(
        museum_id = museum_ids,
        score = 0
      )
    )
  }

  s <- as.numeric(X %*% qv)

  tibble::tibble(
    museum_id = museum_ids,
    score = s
  ) |>
    dplyr::arrange(dplyr::desc(score))
}

filter_by_year <- function(df, event_type, start, end, certain, inclusive) {
  convert_to_truncated_timescale <- function(year) {
    ifelse(year < 1960, 1959, year)
  }
  if (start == "pre-1960") {
    start <- 1959
  } else if (start == "never") {
    start <- 9999
  } else {
    start <- as.numeric(start)
  }
  if (end == "pre-1960") {
    end <- 1959
  } else if (end == "never") {
    end <- 9999
  } else {
    end <- as.numeric(end)
  }
  if (inclusive) {
    ordering_operator <- `<=`
  } else {
    ordering_operator <- `<`
  }
  if (certain) {
    combination_operator <- `&`
  } else {
    combination_operator <- `|`
  }
  df |>
    mutate(
      yo1=convert_to_truncated_timescale(.data[[paste("year", event_type, "1", sep="_")]]),
      yo2=convert_to_truncated_timescale(.data[[paste("year", event_type, "2", sep="_")]]),
    ) |>
    filter(
      combination_operator(
        ordering_operator(start, yo1) & ordering_operator(yo1, end),
        ordering_operator(start, yo2) & ordering_operator(yo2, end)
      )
    ) |>
    select(-yo1, -yo2)
} 
