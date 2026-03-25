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

score_query <- function(
  query,
  museums,
  X,
  museum_ids,
  term_to_col,
  idf,
  ngram_range = c(3L, 5L)
) {

  qv <- make_query_vec(
    query = query,
    term_to_col = term_to_col,
    idf = idf,
    ncol_X = ncol(X),
    ngram_range = ngram_range
  )

  if (length(qv@x) == 0 && length(terms) == 0) {
    return(
      tibble::tibble(
        museum_id = museum_ids,
        proportion_of_terms_in_name = 0,
        proportion_of_terms_in_blob = 0,
        cosine_similarity = 0,
        score = 0
      )
    )
  }

  if (length(qv@x) == 0) {
    cosine_similarity <- rep(0, length(museum_ids))
  } else {
    cosine_similarity <- as.numeric(X %*% qv)
  }

  m <- museums |>
    select(museum_id, search_blob, museum_name, alternative_name) |>
    unique() |>
    mutate(
      name=tolower(paste(museum_name, ifelse(is.na(alternative_name), "", alternative_name)))
    )
  search_blob <- m$search_blob
  name <- m$name
  q <- tolower(query)
  terms <- unlist(strsplit(gsub("[^a-z0-9 ]+", " ", q), "\\s+"))
  terms <- terms[nzchar(terms)]

  if (length(terms) == 0) {
    proportion_of_terms_in_name <- rep(0, length(name))
    proportion_of_terms_in_blob <- rep(0, length(search_blob))
  } else {
    name_term_match_matrix <- vapply(
      terms,
      function(term) grepl(term, name, fixed = TRUE),
      logical(length(name))
    )

    blob_term_match_matrix <- vapply(
      terms,
      function(term) grepl(term, search_blob, fixed = TRUE),
      logical(length(search_blob))
    )

    if (is.null(dim(name_term_match_matrix))) {
      proportion_of_terms_in_name <- as.numeric(name_term_match_matrix)
    } else {
      proportion_of_terms_in_name <- rowMeans(name_term_match_matrix)
    }

    if (is.null(dim(blob_term_match_matrix))) {
      proportion_of_terms_in_blob <- as.numeric(blob_term_match_matrix)
    } else {
      proportion_of_terms_in_blob <- rowMeans(blob_term_match_matrix)
    }
  }

  table_of_cosine_similarities <- tibble::tibble(
    museum_id = museum_ids,
    cosine_similarity = cosine_similarity
  ) |>
    unique()

  tibble::tibble(
    museum_id = m$museum_id,
    proportion_of_terms_in_name,
    proportion_of_terms_in_blob
  ) |>
    left_join(
      table_of_cosine_similarities,
      by="museum_id"
    ) |>
    mutate(
      score =
        0.3 * proportion_of_terms_in_name +
        0.3 * proportion_of_terms_in_blob +
        0.4 * cosine_similarity
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
