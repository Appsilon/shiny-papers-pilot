# packages and functions
box::use(
  glue[
    glue
  ],
  shiny[
    fluidPage,
    moduleServer,
    NS,
    observeEvent,
  ],
)

box::use(
  utils = ./utils/utils,
)

#' @export
ui <- function(id, consts) {
  # namespace
  ns <- NS(id)

  # simple shiny app to validate the ideas. we will exclude it later
  mecanisms <- lapply(X = consts$pathways, FUN = function(x) {
    utils$mechanism_card(
      glide_id = ns("glide"),
      element_id = x$id,
      mechanism = x$label,
      color = x$color,
      icon = x$icon,
      definition = x$definition,
      indicator = x$indicator,
      unit = x$unit,
      unit_type = x$unit_type
    )
  })

  # ui
  utils$glide(content = mecanisms)
}

#' @export
server <- function(id, consts) {
  moduleServer(id, function(input, output, session) {
    # environment
    ns <- session$ns

    # select the first pathway
    shinyjs::runjs(code = glue("glideSelected('{consts$pathways$carbon_sequestration$id}', '{ns('glide')}')"))

    # update selected pathway
    observeEvent(input$glide, {
      session$userData$pathway(input$glide)
    })
  })
}
