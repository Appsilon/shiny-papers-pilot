# packages and functions
box::use(
  shiny[
    fluidPage,
    HTML,
    includeCSS,
    includeScript,
    renderText,
    shinyApp,
    tags,
    textOutput,
  ],
  yaml[read_yaml]
)

box::use(
  utils = ./utils,
)

# get the data
constants <- read_yaml(file = "constants/constants.yml")
studies <- readRDS(file = "data/preprocessing/studies.RDS")

# simple shiny app to validate the ideas. we will exclude it later
mecanisms <- lapply(X = constants$pathways, FUN = function(x) {
  utils$mechanism_card(
    glide_id = 'glide',
    element_id = x$id,
    mechanism = x$label,
    color = x$color,
    icon = x$icon
  )
})

ui <- fluidPage(
  # glidejs
  tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/Glide.js/3.0.2/css/glide.core.css"),
  tags$script(src = "https://cdn.jsdelivr.net/npm/@glidejs/glide"),

  # personal css
  includeCSS(path = "www/styles.css"),
  includeScript(path = "www/utils.js"),

  # ui
  utils$glide(content = mecanisms),
  textOutput(outputId = "text")
)


server <- function(input, output) {
  observeEvent(input$glide, {
    output$text <- renderText({
      input$glide
    })
  })
}

shinyApp(ui = ui, server = server)
