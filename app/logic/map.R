# packages and functions
box::use(
  dplyr[
    `%>%`,
    filter,
    if_else,
    left_join,
    mutate,
  ],
  leaflet[
    addProviderTiles,
    addPolygons,
    leaflet,
    leafletOutput,
    renderLeaflet,
    setMaxBounds,
  ],
  sf[
    st_as_sf,
  ],
  shiny[
    moduleServer,
    NS,
  ],
)

box::use(
  utils = ./utils/utils
)

#' @export
ui <- function(id) {
  # namespace
  ns <- NS(id)

  # map
  leafletOutput(outputId = ns("map"), height = "100vh")
}

#' @export
server <- function(id, studies, shp, consts) {
  moduleServer(id, function(input, output, session) {
    output$map <- renderLeaflet({
      # get the current mechanism
      mechanism_sel <- session$userData$pathway()

      # removing entries without MPA
      if (!mechanism_sel %in% studies$mechanism_internal) return(NULL)

      # summarise data for the specifc mechanism
      studies <- studies %>%
        filter(mechanism_internal == mechanism_sel) %>%
        left_join(y = shp, by = "country") %>%
        st_as_sf() %>%
        mutate(
          label = utils$create_tooltip(
            consts = consts,
            # name = name,
            mechanism = mechanism_internal,
            n_mpas = n_mpas,
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
          prop = if_else(
            condition = n_studies != n_ambiguous,
            true = n_positive / (n_studies - n_ambiguous),
            false = 0
          )
        )

      # draw the map
      base_color <- consts$pathways[[mechanism_sel]]$color
      pal <- leaflet::colorNumeric(
        palette = c("#FFFFFFFF", base_color),
        domain = studies$prop
      )

      leaflet(data = studies) %>%
        addProviderTiles(provider = "Esri.WorldGrayCanvas") %>%
        addPolygons(
          color = ~pal(prop),
          weight = 0,
          fillOpacity = 0.3,
          popup = ~label
        ) %>%
        setMaxBounds(
          lng1 = -90,
          lat1 = -180,
          lng2 = 90,
          lat2 = 180
        )
    })
  })
}
