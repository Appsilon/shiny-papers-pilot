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
      n_studies = n_distinct(ID2),
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
      votes = (n_positive - n_negative)/(n_positive + n_negative)
    )

  return(ns)
}
