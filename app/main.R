box::use(
  sf[
    st_as_sf
  ],
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
mpas_shp <- readRDS(file = "app/static/data/preprocessing/mpas_shp.RDS")
countries_shp <- readRDS(file = "app/static/data/preprocessing/countries_shp.RDS")
mpas_shp <- st_as_sf(x = mpas_shp)
countries_shp <- st_as_sf(x = countries_shp)
studies <- readRDS(file = "app/static/data/preprocessing/studies.RDS")
studies <- utils_data$summarise_studies(studies)

#' @export
ui <- function(id) {
  # namespace
  ns <- NS(id)

  # ui
  fluidPage(
    # glidejs
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/Glide.js/3.0.2/css/glide.core.css"
    ),
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
    map$server(id = "map", studies = studies, shp = countries_shp, consts = constants)
  })
}
