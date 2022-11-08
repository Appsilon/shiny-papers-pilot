box::use(
  dplyr[
    `%>%`,
    first,
    group_by,
    n,
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
summarise_mpas <- function(studies) {
  studies <- studies %>%
    group_by(ID, name, mechanism, mechanism_internal) %>%
    summarise(
      flag = first(flag),
      n_studies = n(),
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