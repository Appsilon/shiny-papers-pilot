library(shiny)
library(plotly)
library(dplyr)
library(shinyjs)
source("app/logic/utils/utils.R")

constants <- read_yaml(file = "app/static/constants/constants.yml")

ui <- fluidPage(
  useShinyjs(),
  extendShinyjs(
    text = "shinyjs.resetClick = function() {
      Shiny.onInputChange('.clientValue-plotly_click-vote_plot', 'null');
    }",
    functions = c("resetClick")
    ),
  # TODO: this input is a simplification of incoming vote data. We will not use
  # a widget just the pathway data.
  div(
    style = "display: grid; gap: 20px;",
    actionButton("rand_data", "Incoming data :)"),
    div(
      style = "border: 3px gray solid; border-radius: 5px",
      plotlyOutput(height = "150px", "vote_plot")
    ),
    verbatimTextOutput("clickevent")
  )
)

server <- function(input, output, session) {
  # To allow smooth animation between data updates.
  frame_n <- reactiveVal(1)
  output$clickevent <- renderPrint({
    event_data("plotly_click", source = "vote_plot")
  })
  output$vote_plot <- renderPlotly({
    first_frame <- isolate(frame_n())

    plot_ly(source = "vote_plot") %>%
      add_trace(
        x = rep(constants$votes$init, 3), y = rep(0, 3), frame = first_frame,
        type = "scatter",
        mode = "lines",
        hoverinfo = "none",
        line = list(border = "round", width = 10, simplify = FALSE, color = "gray")
      ) %>%
      add_animated_marker(
        .,
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
          range = list(constants$votes$min - 0.1, constants$votes$max + 0.1)
        ),
        showlegend = FALSE
      ) %>%
      animation_opts(frame = 500, transition = 500, redraw = FALSE, mode = "next") %>%
      config(displayModeBar = FALSE)
  })

  vote_plot_event <- reactive(event_data("plotly_click", source = "vote_plot"))
  observeEvent(vote_plot_event(), {
      shinyjs::js$resetClick()
    if (vote_plot_event()$curveNumber == 1) {
      showNotification("vote thingy was clicked")
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(input$rand_data, {
    frame_n(frame_n() + 1)
    votes <- round(runif(1, min = constants$votes$min, max = constants$votes$max), 2)

    plotlyProxy("vote_plot", session = session, deferUntilFlush = FALSE) %>%
      plotlyProxyInvoke(
        "animate",
        list(
          data = list(
            list(
              x = c(0, votes, 0),
              frame = frame_n(),
              line = list(color = set_line_color(votes))
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
}

shinyApp(ui, server)
