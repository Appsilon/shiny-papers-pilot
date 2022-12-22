box::use(
  dplyr[
    `%>%`,
    filter,
    first,
    group_by,
    mutate,
    n,
    n_distinct,
    select,
    summarise,
    ungroup,
  ],
  rlang[
    `!!`,
    parse_expr,
  ],
  sf[
    st_as_sf,
    st_simplify,
    st_transform,
  ],
)

#' @title
#' @description
#'
#' @param studies
#'
#' @export
summarise_studies <- function(studies) {
  studies <- studies %>%
    group_by(country, mechanism, mechanism_internal) %>%
    summarise(
      flag = first(flag),
      n_mpas = n_distinct(name),
      n_studies = n_distinct(ID),
      n_votes = n(),
      n_positive = sum(direction == "positive"),
      n_negative = sum(direction == "negative"),
      n_neutral = sum(direction == "neutral"),
      n_ambiguous = sum(direction == "ambiguous"),
      country = paste0(unique(country), collapse = ", "),
      climate = paste0(unique(climate), collapse = ", "),
      continent = paste0(unique(continent), collapse = ", "),
      ocean = paste0(unique(ocean), collapse = ", "),
      ecosystem = paste0(unique(ecosystem), collapse = ", "),
      .groups = "keep"
    ) %>%
    ungroup()

  return(studies)
}

#' @title
#' @description
#'
#' @param studies
#' @param mechanism
#'
#' @export
summarise_mechanism <- function(studies, mechanism) {
  mechanism_sel <- mechanism
  ns <- studies %>%
    filter(mechanism_internal == mechanism_sel) %>%
    summarise(
      mechanism = first(mechanism),
      mechanism_internal = first(mechanism_internal),
      n_studies = sum(n_studies),
      n_votes = sum(n_votes),
      n_positive = sum(n_positive),
      n_negative = sum(n_negative),
      n_neutral = sum(n_neutral),
      n_ambiguous = sum(n_ambiguous)
    ) %>%
    mutate(
      votes = (n_positive - n_negative)/(n_votes)
    )

  return(ns)
}

#' @title
#' @description
#'
#' @param file
#'
#' @export
read_csv_wrap <- function(file) {
  read.csv(file = file, colClasses = "character", na.strings = "")
}

#' @title
#' @description
#'
#' @param pathway_list
#'
#' @export
clean_pathway <- function(pathway_list) {
  # collect info
  pathway_file_in <- pathway_list$file_in
  pathway_file <- pathway_list$file
  pathway_fill <- pathway_list$label
  pathway_filter <- pathway_list$filter
  pathway_file_in <- pathway_list$file_in

  # filter papers that are participating
  df <- read_csv_wrap(file = pathway_file_in) %>%
    filter(!is.na(direction))

  # filter pathway
  if (!is.null(pathway_filter))
    df <- filter(.data = df, !!parse_expr(pathway_filter))

  # fill pathway
  if (!is.null(pathway_fill))
    df <- mutate(.data = df, mechanism = pathway_fill)

  # try to save the file
  tryCatch(
    expr = {
      write.csv(x = df, file = pathway_file)
      return(pathway_file)
    },
    error = function(e) e
  )
}
