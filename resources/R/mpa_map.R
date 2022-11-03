# packages and functions
box::use(
  dplyr[
    `%>%`,
    filter,
    first,
    group_by,
    left_join,
    mutate,
    n,
    select,
    summarise,
    ungroup,
  ],
  glue[
    glue,
  ],
  leaflet[
    addProviderTiles,
    addPolygons,
    leaflet,
    leafletOutput,
    renderLeaflet,
  ],
  sf[
    st_as_sf,
    st_simplify,
    st_transform,
  ],
  shiny[
    fluidPage,
    includeCSS,
    selectInput,
    shinyApp,
  ],
  yaml[read_yaml],
)

box::use(
  utils = ./utils
)

# get the data
constants <- read_yaml(file = "constants/constants.yml")
shp <- readRDS(file = "data/preprocessing/mpas_shp.RDS")
studies <- readRDS(file = "data/preprocessing/studies.RDS")

# summarise important variables
mpas <- studies %>%
  group_by(ID, name, mechanism, mechanism_internal) %>%
  summarise(
    flag = first(flag),
    n_studies = n(),
    n_positive = sum(direction == "positive"),
    n_negative = sum(direction == "negative"),
    n_neutral = sum(direction == "neutral"),
    n_ambiguous = sum(direction == "ambiguous"),
    country = paste0(unique(country), collapse = ", "),
    climate = paste0(unique(climate), collapse = ", "),
    continent = paste0(unique(continent), collapse = ", "),
    ocean = paste0(unique(ocean), collapse = ", "),
    ecosystem = paste0(unique(ecosystem), collapse = ", "),
    .groups = "keep"
  ) %>%
  ungroup()

shp <- shp %>%
  st_as_sf() %>%
  select(-name) %>%
  st_transform(crs = 4087) %>%
  st_simplify(dTolerance = 1000) %>%
  st_transform(crs = 4326)

# simple shiny app to validate the ideas. we will exclude it later
ui <- fluidPage(
  includeCSS(path = "www/styles.css"),
  mechanism <- selectInput(
    inputId = "mechanism",
    label = "Mechanism",
    choices = sort(unique(mpas$mechanism)),
    selected = "Agency"
  ),
  leafletOutput(outputId = "map", height = "100vh")
)

server <- function(input, output) {
  output$map <- renderLeaflet({

    # get the current mechanism
    mechanism_sel <- input$mechanism

    # summarise data for the specifc mechanism
    mpas <- mpas %>%
      filter(mechanism == mechanism_sel) %>%
      left_join(y = shp, by = "ID") %>%
      st_as_sf() %>%
      mutate(
        label = utils$create_tooltip(
          consts = constants,
          name = name,
          mechanism = mechanism_internal,
          n_studies = n_studies,
          n_positive = n_positive,
          n_negative = n_negative,
          n_neutral = n_neutral,
          n_ambiguous = n_ambiguous,
          country = country,
          flag = flag,
          continent = continent,
          ocean = ocean,
          climate = climate,
          ecosystem = ecosystem
        ),
        prop = n_positive/(n_studies - n_ambiguous)
      )

    # draw the map
    mechanism_internal <- unique(mpas$mechanism_internal)
    base_color <- constants$pathways[[mechanism_internal]]$color
    pal <- leaflet::colorNumeric(
      palette = c("#FFFFFFFF", base_color),
      domain = mpas$prop
    )

    leaflet(mpas) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(
        color = ~pal(prop),
        weight = 0,
        fillOpacity = 0.3,
        popup = ~label
      )
  })
}

shinyApp(ui = ui, server = server)
