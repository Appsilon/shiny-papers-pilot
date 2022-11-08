box::use(
  shiny[
    fluidPage,
    includeCSS,
    includeScript,
    moduleServer,
    NS,
    reactiveVal,
    tags,
  ],
  yaml[
    read_yaml,
  ],
)

box::use(
  map = logic/mpa_map,
  pathways = logic/pathways_cards,
  utils_data = logic/utils/utils_data,
)

# get the data
constants <- read_yaml(file = "app/static/constants/constants.yml")
shp <- readRDS(file = "app/static/data/preprocessing/mpas_shp.RDS")
studies <- readRDS(file = "app/static/data/preprocessing/studies.RDS")
mpas <- utils_data$summarise_mpas(studies)

#' @export
ui <- function(id) {
  # namespace
  ns <- NS(id)

  # ui
  fluidPage(
    # glidejs
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/Glide.js/3.0.2/css/glide.core.css"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/@glidejs/glide"),

    # personal css
    includeCSS(path = "app/styles/styles.css"),
    includeScript(path = "app/js/utils.js"),

    # ui
    tags$div(
      class = "app-conteiner",
      pathways$ui(id = ns("pathways"), consts = constants),
      map$ui(id = ns("map"))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # sessionData
    session$userData$pathway <- reactiveVal("carbon_sequestration")

    # modules
    pathways$server(id = "pathways")
    map$server(id = "map", mpas = mpas, shp = shp, consts = constants)
  })
}
