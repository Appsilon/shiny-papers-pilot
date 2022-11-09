box::use(
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
    js,
  ],
  plotly[
    event_data,
    plotlyOutput,
    renderPlotly,
  ],
  stats[
    runif,
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

    utils$insert_vote_count(
      vote_plot_id = vote_plot_id,
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

      utils$plot_vote_count(
        plot_id = plot_id,
        consts = consts,
        first_frame = first_frame
      )
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

    observeEvent(session$userData$pathway(), {
      frame_n(frame_n() + 1)
      votes <- runif(1, -1, 1)

      utils$update_vote_count(
        session = session,
        plot_id = plot_id,
        votes = votes,
        frame = frame_n()
      )
    }, ignoreInit = TRUE)
  })
}
