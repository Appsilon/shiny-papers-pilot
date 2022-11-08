box::use(
  glue[
    glue
  ],
  dplyr[
    `%>%`,
    case_when,
  ],
  shiny[
    absolutePanel,
    isolate,
    moduleServer,
    NS,
    observeEvent,
    reactive,
    reactiveVal,
    tags,
  ],
  shinyjs[
    extendShinyjs,
    js,
  ],
  plotly[
    add_trace,
    animation_opts,
    config,
    event_data,
    layout,
    plotlyOutput,
    plotlyProxy,
    plotlyProxyInvoke,
    plot_ly,
    renderPlotly,
  ]
)

box::use(
  utils = ./utils/utils
)

plot_id <- "vote_plot"

#' export
ui <- function(id, consts) {
  # namespace
  ns <- NS(id)
  vote_plot_id <- ns(plot_id)

  absolutePanel(
    id = "controls",
    class = "panel panel-default",
    fixed = TRUE,
    draggable = TRUE,

    top = "auto",
    bottom = 10,
    left = 0,
    right = 0,

    width = "fit-content",
    height = "fit-content",

    tags$h2("Vote-counting"),

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

      tags$div(
        plotlyOutput(outputId = vote_plot_id, height = "150px", width = "40vw")
      )
    )
  )
}

server <- function(id, consts) {
  moduleServer(id, function(input, output, session) {
    # environment
    ns <- session$ns
    vote_plot_id <- ns(plot_id)

    # To allow smooth animation between data updates.
    frame_n <- reactiveVal(1)
    output[[plot_id]] <- renderPlotly({
      first_frame <- isolate(frame_n())

      plot_ly(source = plot_id) %>%
        add_trace(
          x = rep(consts$votes$init, 3), y = rep(0, 3), frame = first_frame,
          type = "scatter",
          mode = "lines",
          hoverinfo = "none",
          line = list(border = "round", width = 10, simplify = FALSE, color = "gray")
        ) %>%
        utils$add_animated_marker(
          .,
          consts = consts,
          ypos = 0,
          size = 25,
          symbol = "circle",
          color = "gray",
          lwd = 2,
          lcol = "white",
          frame = first_frame
        ) %>%
        layout(
          yaxis = list(visible = FALSE, fixedrange = TRUE, range = list(-1, 1)),
          xaxis = list(
            zeroline = TRUE,
            showline = FALSE,
            showticklabels = FALSE,
            showgrid = TRUE,
            fixedrange = TRUE,
            range = list(consts$votes$min - 0.1, consts$votes$max + 0.1)
          ),
          showlegend = FALSE
        ) %>%
        animation_opts(frame = 500, transition = 500, redraw = FALSE, mode = "next") %>%
        config(displayModeBar = FALSE)
    })

    vote_plot_event <- reactive(
      event_data(
        event = "plotly_click",
        source = plot_id
      )
    )
    observeEvent(vote_plot_event(), {
      js$resetClick()
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    observeEvent(session$userData$pathway, {
      frame_n(frame_n() + 1)

      votes <- 0.85

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
                frame = frame_n(),
                line = list(color = utils$set_line_color(votes))
              ),
              list(
                x = rep(votes, 2),
                frame = frame_n()
              )
            ),
            traces = list(0, 1)
          )
        )
    })
  })
}
