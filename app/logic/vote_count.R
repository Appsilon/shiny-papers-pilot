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
  shinycssloaders[
    withSpinner,
  ],
  shinyjs[
    js,
    hide,
    hidden,
    toggle,
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
      tags$div(
        # header
        tags$div(
          style = "width: 40vw;",
          tags$div(
            class = "collapse-div",
            icon(
              id = "collapse_button",
              name = "minus",
              `data-toggle` = "collapse",
              `data-target` = "#demo",
              class = "collapse-button",
              onclick = "updateCollapseIcon('collapse_button')"
            )
          ),
          htmlOutput(outputId = ns("header"))
        ),
        tags$div(
          id = "demo",
          class = "vote-count-plot collapse in",
          tags$div(
            style = "position: relative",
            # tooltip
            hidden(
              tags$div(
                id = ns("vote_subplot_div"),
                class = "vote-subplot-div",
                icon(
                  id = ns("vote_subplot_update_close"),
                  name = "close",
                  class = "action-button vote-subplot-icon"
                ),
                tags$h2("Vote counting", class = "popup-title"),
                tags$p(consts$infos$vote_count, class = "popup-subtitle"),
                tags$div(
                  class = "vote-count-plot",
                  withSpinner(
                    ui_element = plotOutput(outputId = vote_subplot_id, width = 400, height = 200) ,
                    type = 5
                  )
                )
              )
            ),
            # tooltip
            tags$h5(
              id = ns("vote_subplot_update_toggle"),
              icon("plus-circle"),
              "See all votes",
              class = "action-button vote-count-show-more"
            )
          ),
          plotlyOutput(outputId = vote_plot_id, height = "150px", width = "40vw")
        )
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

    # toogle popup
    observeEvent(input$vote_subplot_update_toggle, {
      toggle(id = "vote_subplot_div")
    })

    # close popup
    observeEvent(input$vote_subplot_update_close, {
      hide(id = "vote_subplot_div")
    })
  })
}
