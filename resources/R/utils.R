# packages and funcitons
box::use(
  glue[...],
  htmltools[...],
  stringr[...],
  yaml[...]
)

#' @title
#' @description
#'
#' @param consts
#' @param name
#' @param mechanism
#' @param n_studies
#' @param n_positive
#' @param n_negative
#' @param n_neutral
#' @param n_ambiguous
#' @param country
#' @param flag
#' @param continent
#' @param ocean
#' @param climate
#' @param ecosystem
#'
#' @export
create_tooltip <- function(
    consts, name, mechanism,
    n_studies, n_positive, n_negative, n_neutral, n_ambiguous,
    country, flag, continent, ocean, climate, ecosystem
) {
  # get the metadata
  metadata <- consts$pathways[[unique(mechanism)]]

  # tooltip template
  tooltip_html <- glue(
    "
    <!-- title: flag, country and MPA name -->
    <h3 class = 'popup-title'>
      <img src = {flag} class = 'flag'></img>
      {country} - <b>{name}</b>
    </h2>

    <hr>

    <!-- MPA information -->
    <div class = 'grid-four'>
      <div class = 'info-div'>
        <h4 class = 'info-title'>Continent:</h4>
      </div>
      <div class = 'info-div'>
        <h4 class = 'info-content'>{continent}</h3>
      </div>
      <div class = 'info-div'>
        <h4 class = 'info-title'>Climate:</h4>
      </div>
      <div class = 'info-div'>
        <h4 class = 'info-content'>{climate}</h3>
      </div>
      <div class = 'info-div'>
        <h4 class = 'info-title'>Ocean:</h4>
      </div>
      <div class = 'info-div'>
        <h4 class = 'info-content'>{ocean}</h3>
      </div>
      <div class = 'info-div'>
        <h4 class = 'info-title'>Ecosystem:</h4>
      </div>
      <div class = 'info-div'>
        <h4 class = 'info-content'>{ecosystem}</h3>
      </div>
    </div>

    <hr>

    <!-- mechanism title -->
    <div class = 'mechanism-div' style = 'background: {metadata$color}'>
      <h4 class = 'mechanism-title'>{metadata$label}</h4>
      <img src = '{metadata$icon}' class = 'mechanism-icon'></img>
    </div>

    <!-- mechanism information -->

    <div class = 'grid-four' style = 'margin-top: 20px'>
      <div class = 'direction-div'>
        <img src = 'https://i.ibb.co/Y2h1Jq5/positive.png' class = 'direction-icon'></img>
        <h5 class = 'direction-number'>{n_positive}</h5>
      </div>
      <div class = 'direction-div'>
        <img src = 'https://i.ibb.co/N7HFXrH/negative.png' class = 'direction-icon'></img>
        <h5 class = 'direction-number'>{n_negative}</h5>
      </div>
      <div class = 'direction-div'>
        <img src = 'https://i.ibb.co/jMy9LKs/neutral.png' class = 'direction-icon'></img>
        <h5 class = 'direction-number'>{n_neutral}</h5>
      </div>
      <div class = 'direction-div'>
        <img src = 'https://i.ibb.co/7WXs1wm/ambiguous.png' class = 'direction-icon'></img>
        <h5 class = 'direction-number'>{n_ambiguous}</h5>
      </div>
    </div>
    "
    )

  return(tooltip_html)
}

#' @title
#' @description
#'
#' @param flag
#'
#' @export
get_flag_link <- function(flag) {
  flag <- str_to_lower(string = flag)
  flag <- str_replace_all(string = flag, pattern = "\\s", replacement = "")

  url <- glue("https://www.crwflags.com/art/countries/{flag}.gif")

  return(url)
}

#' @title
#' @description
#'
#' @param val
#'
#' @export
set_line_color <- function(val) {
  case_when(
    val > 0 ~ "green",
    val == 0 ~ "grey",
    TRUE ~ "red"
  )
}

#' @title
#' @description
#'
#' @param p
#' @param ypos
#' @param symbol
#' @param size
#' @param color
#' @param lwd
#' @param lcol
#' @param frame
#'
#' @export
add_animated_marker <- function(p, ypos = 0.01, symbol = "arrow-down", size = 20,
                                color = "green", lwd = 1, lcol = "black", frame = 1) {
  p %>%
    add_trace(
    type = "scatter",
    mode = "markers",
    x = rep(constants$votes$init, 2),
    y = ypos,
    frame = frame,
    hoverinfo = "none",
    marker = list(
      symbol = symbol,
      size = size,
      color = color,
      line = list(
        width = lwd,
        color = lcol
      )
    )
  )
}
