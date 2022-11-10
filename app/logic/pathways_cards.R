# packages and functions
box::use(
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
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # update selected pathway
    observeEvent(input$glide, {
      session$userData$pathway(input$glide)
    })
  })
}
