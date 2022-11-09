box::use(
  glue[
    glue
  ],
  plotly[
    event_data,
    plotlyOutput,
    renderPlotly,
  ],
  shiny[
    absolutePanel,
    eventReactive,
    htmlOutput,
    icon,
    isolate,
    moduleServer,
    NS,
    observeEvent,
    plotOutput,
    reactive,
    reactiveVal,
    renderPlot,
    renderUI,
    tags,
  ],
  shinyjs[
    js,
  ],
  spsComps[
    bsPopover,
    bsPop,
  ],
)

box::use(
  utils = ./utils/utils,
  utils_data = ./utils/utils_data,
)

plot_id <- "vote_plot"
subplot_id <- "vote_subplot"

#' export
ui <- function(id, consts) {
  # namespace
  ns <- NS(id)
  vote_plot_id <- ns(plot_id)
  vote_subplot_id <- ns(subplot_id)

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

    tags$div(
      htmlOutput(outputId = ns("header")),
      tags$div(
        class = "vote-count-plot",
        tags$div(
          style = "position: relative",
          tags$div(
            id = ns("vote_subplot_div"),
            class = "vote-subplot-div",
            icon(
              name = "close",
              class = "vote-subplot-icon",
              onclick = glue("toggleSubplot('{ns('vote_subplot_div')}')")
            ),
            tags$h2("Vote counting", style = "margin: 0px"),
            tags$p("Some explanation here...", style = "margin: 0px 5px 20px 5px;"),
            tags$div(
              style = "padding: 10px; border: solid 1px grey; border-radius: 10px;",
              plotOutput(outputId = vote_subplot_id, width = 400, height = 200)
            )
          ),
          tags$h5(
            icon("plus-circle"),
            "See all votes",
            class = "vote-count-show-more",
            onclick = glue("toggleSubplot('{ns('vote_subplot_div')}')")
          )
        ),
        plotlyOutput(outputId = vote_plot_id, height = "150px", width = "40vw")
      )
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
        html_code <- utils$create_header(
          mechanism = mechanism,
          mechanism_icon = mechanism_meta$icon,
          n_votes = statistics_df$n_votes
        )

        return(html_code)
      })

      # Plot
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

      # Subplot
      output[[subplot_id]] <- renderPlot({
        utils$votes_barplot(data = statistics(), consts = consts)
      }, bg = "transparent")
    }, ignoreNULL = TRUE)
  })
}
