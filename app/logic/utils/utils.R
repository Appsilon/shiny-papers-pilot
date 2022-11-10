# packages and funcitons
box::use(
  dplyr[...],
  ggplot2[...],
  ggtext[...],
  glue[...],
  htmltools[...],
  plotly[...],
  shiny[...],
  shinyjs[...],
  stats[...],
  stringr[...],
  tidyr[...],
  spsComps[...],
  yaml[...],
)

#' @title
#' @description
#'
#' @param consts
#' @param mechanism
#' @param n_mpas
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
    consts, mechanism,
    n_mpas, n_studies, n_positive, n_negative, n_neutral, n_ambiguous,
    country, flag, continent, ocean, climate, ecosystem
) {
  # get the metadata
  metadata <- consts$pathways[[unique(mechanism)]]

  # tooltip template
  tooltip_html <- glue(
    "
    <!-- title: flag and country -->
    <h3 class = 'popup-title'>
      <img src = {flag} class = 'flag'></img>
      {country}
    </h2>

    <hr>

    <!-- MPA information -->
    <div class = 'grid-four-text'>
      <div class = 'info-div'>
        <h4 class = 'info-title'>Continent:</h4>
      </div>
      <div class = 'info-div'>
        <span class = 'info-content-hover' data-hover = '{continent}'>
          <h4 class = 'info-content'>{continent}</h3>
        </span>
      </div>
      <div class = 'info-div'>
        <h4 class = 'info-title'>Climate:</h4>
      </div>
      <div class = 'info-div'>
        <span class = 'info-content-hover' data-hover = '{climate}'>
          <h4 class = 'info-content'>{climate}</h3>
        </span>
      </div>
      <div class = 'info-div'>
        <h4 class = 'info-title'>Ocean:</h4>
      </div>
      <div class = 'info-div'>
        <span class = 'info-content-hover' data-hover = '{ocean}'>
          <h4 class = 'info-content'>{ocean}</h3>
        </span>
      </div>
      <div class = 'info-div'>
        <h4 class = 'info-title'>Ecosystem:</h4>
      </div>
      <div class = 'info-div'>
        <span class = 'info-content-hover' data-hover = '{ecosystem}'>
          <h4 class = 'info-content'>{ecosystem}</h3>
        </span>
      </div>
    </div>

    <hr>

    <!-- mechanism title -->
    <div class = 'mechanism-div' style = 'background: {metadata$color}'>
      <h4 class = 'mechanism-title'>{metadata$label}</h4>
      <img src = '{metadata$icon}' class = 'mechanism-icon'></img>
    </div>

    <!-- mechanism information -->

    <div class = 'grid-four-img' style = 'margin-top: 20px'>
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
#' @param indicator
#' @param unit
#' @param unit_type
#'
#' @export
indicator_info <- function(indicator, unit, unit_type) {
  indicators <- mapply(
    indicator,
    unit,
    unit_type,
    FUN = function(x, y , z) {
      glue(
      "
      <div class = 'display-flex'>
        <h5 class = 'left-margin margin-bottom-five'><b>{x}:</b></h5>
        <h5 class = 'left-margin'>{y}</h5>
        <h5 class = 'left-margin'><i>({z})</i></h5>
      </div>
      "
      )
    }
  )

  indicators <- paste(indicators, collapse = "")

  return(indicators)
}

#' @title
#' @description
#'
#' @param mechanism
#' @param definition
#' @param indicator
#' @param unit
#' @param unit_type
#'
#' @export
mechanism_info <- function(mechanism, definition, indicator, unit, unit_type) {
  indicator_infos <- indicator_info(
    indicator = indicator,
    unit = unit,
    unit_type = unit_type
  )

  info <- glue(
  "
  <div class = 'info-tooltip'>
    <h3 class = 'margin-bottom-five'><b>{mechanism}</b></h3>
    <h5 class = 'no-margin'>{definition}</h5>
    <hr class = 'hr-margin'>
    <h3 style = 'margin-bottom: 0px'>Indicators</h3>
    {indicator_infos}
  </div>
  "
  )

  return(info)
}

#' @title
#' @description
#'
#' @param glide_id
#' @param element_id
#' @param mechanism
#' @param color
#' @param icon
#' @param definition
#' @param indicator
#' @param unit
#' @param unit_type
#' @param width
#' @param height
#'
#' @export
mechanism_card <- function(
    glide_id,
    element_id,
    mechanism,
    color,
    icon,
    definition,
    indicator,
    unit,
    unit_type,
    width = "100px",
    height = "85px"
) {
  mechanism <- toupper(x = mechanism)
  mechanism_tooltip <- mechanism_info(
    mechanism = mechanism,
    definition = definition,
    indicator = indicator,
    unit = unit,
    unit_type = unit_type
  )
  mechanism_tooltip <- gsub(pattern = "\n", replacement = "", x = mechanism_tooltip)

  out <- tags$a(
    class = "pathway-card",
    id = element_id,
    style = glue("background: {color}; width: {width}; height: {height}"),
    onclick = glue("glideSelected('{element_id}', '{glide_id}')"),
    tags$img(
      src = icon,
      class = "pathway-icon"
    ),
    bsPop(
      tag = icon(
        name = "circle-info",
        class = "pathway-info"
      ),
      content = mechanism_tooltip,
      placement = "bottom",
      html = TRUE,
      status = "info"
    ),
    tags$p(
      class = "pathway-title",
      mechanism
    )
  )

  return(out)
}

#' @title
#' @description
#'
#' @param dir < or >
#' @param float left or right
#'
#' @export
arrow_btn <- function(dir = "<", float = "left") {
  html_code <- tags$div(
    `class` = "glide__arrows",
    `data-glide-el` = "controls",
    tags$button(
      `class` = "glide__arrow glide__arrow--left glide-control",
      `data-glide-dir` = dir,
      style = glue("float: {float}"),
      dir
    )
  )

  return(html_code)
}

#' @title
#' @description
#'
#' @param content
#'
#' @export
glide <- function(content) {
  html_code <-
    absolutePanel(
      fixed = TRUE,
      draggable = TRUE,
      class = "glide pathway-grid",
      tags$div(
        class = "glide-container",
        arrow_btn(dir = "<", float = "left"),
        tags$div(
          `class` = "glide__track",
          `data-glide-el` = "track",
          tags$ul(
            class = "glide__slides",
            content
          )
        ),
        arrow_btn(dir = ">", float = "right")
      )
    )

  return(html_code)
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
  cases <- case_when(
    val > 0 ~ "green",
    val == 0 ~ "grey",
    TRUE ~ "red"
  )

  return(cases)
}

#' @title
#' @description
#'
#' @param vote_plot_id
#' @param ...
#'
#' @export
insert_vote_count <- function(vote_plot_id, ...) {
  tags$div(
    extendShinyjs(
      text = glue(
        "
        shinyjs.resetClick = function() {
          Shiny.onInputChange('.clientValue-plotly_click-{{vote_plot_id}}', 'null');
        }",
        .open = "{{",
        .close = "}}"
      ),
      functions = c("resetClick")
    ),
    ...
  )
}

#' @title
#' @description
#'
#' @param p
#' @param consts
#' @param ypos
#' @param symbol
#' @param size
#' @param color
#' @param lwd
#' @param lcol
#' @param frame
#'
#' @export
add_animated_marker <- function(
    p,
    xpos,
    ypos = 0.01,
    symbol = "arrow-down",
    size = 20,
    color = "green",
    lwd = 1,
    lcol = "black",
    frame = 1
) {
  x_label <- glue("<extra></extra>{round(abs(xpos)*100, 2)} %")
  p <- p %>%
    add_trace(
      type = "scatter",
      mode = "markers",
      x = rep(xpos, 2),
      y = ypos,
      frame = frame,
      hovertemplate = x_label,
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

  return(p)
}

#' @title
#' @description
#'
#' @param plot_id
#' @param votes
#' @param consts
#' @param first_frame
#'
#' @export
plot_vote_count <- function(plot_id, votes, consts, first_frame) {
  p <- plot_ly(source = plot_id) %>%
    add_trace(
      x = c(0, votes, 0),
      y = rep(0, 3),
      frame = first_frame,
      type = "scatter",
      mode = "lines",
      line = list(
        border = "round",
        width = 10,
        simplify = FALSE,
        color = set_line_color(votes)
      )
    ) %>%
    add_animated_marker(
      .,
      xpos = votes,
      ypos = 0,
      size = 25,
      symbol = "circle",
      color = set_line_color(votes),
      lwd = 2,
      lcol = "white",
      frame = first_frame
    ) %>%
    layout(
      yaxis = list(
        visible = FALSE,
        fixedrange = TRUE,
        range = list(-1, 1)
      ),
      xaxis = list(
        zeroline = TRUE,
        showline = FALSE,
        showgrid = TRUE,
        fixedrange = TRUE,
        showticklabels = TRUE,
        range = list(
          consts$votes$min - 0.1,
          consts$votes$max + 0.1
        ),
        ticktext = list(
          "100%\nNegative Evidence",
          "0\nEquivalent Evidence",
          "100%\nPositive Evidence"
        ),
        tickvals = list(-1, 0, 1)
      ),
      showlegend = FALSE
    ) %>%
    animation_opts(
      frame = 500,
      transition = 500,
      redraw = FALSE,
      mode = "next"
    ) %>%
    config(displayModeBar = FALSE)

  return(p)
}

#' @title
#' @description
#'
#' @param session
#' @param plot_id
#' @param votes
#' @param frame
#'
#' @export
update_vote_count <- function(session, plot_id, votes, frame) {
  plotlyProxy(
    outputId = plot_id,
    session = session,
    deferUntilFlush = FALSE
  ) %>%
    plotlyProxyInvoke(
      "animate",
      list(
        data = list(
          list(
            x = c(0, votes, 0),
            frame = frame,
            line = list(color = set_line_color(votes))
          ),
          list(
            x = rep(votes, 2),
            marker = list(color = set_line_color(votes)),
            frame = frame
          )
        ),
        traces = list(0, 1)
      )
    )
}

#' @title
#' @description
#'
#' @param mechanism
#' @param mechanism_icon
#' @param n_votes
#'
#' @export
create_header <- function(mechanism, mechanism_icon, n_votes) {
  tags$div(
    class = "absolute-panel-header",
    tags$div(
      class = "absolute-panel-fifty",
      tags$div(
        styles = "float: left;",
        tags$h2(
          "Vote-counting",
          class = "absolute-panel-text"
        ),
        tags$p(
          glue("# Votes: {n_votes}"),
          style = "margin: 0px"
        )
      )
    ),
    tags$div(
      class = "absolute-panel-fifty",
      tags$div(
        style = "display: flex; float: right; text-align: right;",
        tags$h2(
          mechanism,
          class = "absolute-panel-text"
        ),
        tags$img(
          src = mechanism_icon,
          class = "absolute-panel-img"
        )
      )
    )
  )
}

#' @title
#' @description
#'
#' @param data
#' @param consts
#'
#' @export
votes_barplot <- function(data, consts) {
  data <- data %>%
    pivot_longer(cols = n_positive:n_ambiguous, values_to = "n") %>%
    mutate(name = factor(x = name, levels = c("n_positive", "n_negative", "n_neutral", "n_ambiguous")))

  icons <- sapply(X = consts$directions, FUN = "[[", "icon")
  img_tags <- sapply(X = icons, FUN = function(x) sprintf("<img src = '%s' width = '50'/>", x))
  labels <- setNames(
    object = img_tags,
    nm = c("n_ambiguous", "n_negative", "n_neutral", "n_positive")
  )
  data$icon <- labels[data$name]

  g <- ggplot(data = data) +
    geom_bar(
      mapping = aes(x = name, y = n, fill = name),
      width = 0.4,
      stat = "identity"
    ) +
    scale_x_discrete(name = NULL, labels = data$icon, limits = rev) +
    scale_fill_manual(
      values = c(
        "n_positive" = consts$directions$positive$color,
        "n_negative" = consts$directions$negative$color,
        "n_neutral" = consts$directions$neutral$color,
        "n_ambiguous" = consts$directions$ambiguous$color
      )
    ) +
    annotate(
      geom = "text",
      x = data$name,
      y = data$n + 0.05*max(data$n),
      label = data$n
    ) +
    coord_flip() +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x  = element_blank(),
      axis.text.y = element_markdown(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

  return(g)
}
