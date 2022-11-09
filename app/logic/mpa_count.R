box::use(
  glue[
    glue
  ],
  shiny[
    absolutePanel,
    eventReactive,
    htmlOutput,
    isolate,
    moduleServer,
    NS,
    observeEvent,
    reactive,
    reactiveVal,
    renderUI,
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
  utils = ./utils/utils,
  utils_data = ./utils/utils_data,
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

    htmlOutput(outputId = ns("header")),

    tags$div(
      style = "border: solid 1px grey; border-radius: 10px; padding: 5px; margin-top: 15px",
      class = "vote-count-plot",
      plotlyOutput(outputId = vote_plot_id, height = "150px", width = "40vw")
    )
  )
}

server <- function(id, studies, consts) {
  moduleServer(id, function(input, output, session) {
    # environment
    ns <- session$ns
    vote_plot_id <- ns(plot_id)

    # Get necessary statistics
    statistics <- eventReactive(session$userData$pathway(), {
      ns <- utils_data$summarise_mechanism(
        studies = studies,
        mechanism = session$userData$pathway()
      )

      return(ns)
    })
    # To allow smooth animation between data updates.
    frame_n <- reactiveVal(0)
    observeEvent(statistics(), {
      # managing frames
      frame_n(frame_n() + 1)
      frame <- frame_n()

      statistics_df <- statistics()
      mechanism_internal <- statistics_df$mechanism_internal
      mechanism <- statistics_df$mechanism
      mechanism_meta <- consts$pathways[[mechanism_internal]]

      # Header
      output$header <- renderUI({
        html_code <- tags$div(
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
                glue("# Votes: {statistics_df$n_votes}"),
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
                src = mechanism_meta$icon,
                class = "absolute-panel-img"
              )
            )
          )
        )

        return(html_code)
      })

      if (frame == 1) {
        output[[plot_id]] <- renderPlotly({
          utils$plot_vote_count(
            plot_id = plot_id,
            votes = statistics_df$votes,
            consts = consts,
            first_frame = frame
          )
        })
      } else {
        utils$update_vote_count(
          session = session,
          plot_id = plot_id,
          votes = statistics_df$votes,
          frame = frame_n()
        )
      }
    }, ignoreNULL = TRUE)
  })
}
