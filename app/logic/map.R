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
    addLegend,
    addProviderTiles,
    addPolygons,
    colorNumeric,
    leaflet,
    leafletOutput,
    leafletProxy,
    providerTileOptions,
    renderLeaflet,
  ],
  sf[
    st_as_sf,
  ],
  shiny[
    eventReactive,
    moduleServer,
    NS,
    observeEvent,
    reactiveVal,
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
    # leaflet objects
    leaflet_objects <- eventReactive(session$userData$pathway(), {
      # get the current mechanism
      mechanism_sel <- session$userData$pathway()

      # removing entries without MPA
      if (!mechanism_sel %in% studies$mechanism_internal) {
        leafletProxy(
          mapId = "map",
          session = session,
          data = NULL
        ) %>%
          leaflet::clearShapes()

        return(NULL)
      }

      # summarise data for the specifc mechanism
      studies <- studies %>%
        filter(mechanism_internal == mechanism_sel) %>%
        left_join(y = shp, by = "country") %>%
        st_as_sf() %>%
        mutate(
          label = utils$create_tooltip(
            consts = consts,
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
          votes = if_else(
            condition = n_studies != n_ambiguous,
            true = (n_positive - n_negative) / (n_votes),
            false = 0
          )
        )

      # draw the map
      base_color <- consts$pathways[[mechanism_sel]]$color
      pal <- colorNumeric(
        palette = c("#DC3220", "#005AB5"),
        domain = studies$votes
      )

      out <- list(studies = studies, pal = pal)
      return(out)
    })

    frame_n <- reactiveVal(0)
    observeEvent(leaflet_objects(), {
      # collect objects
      objs <- leaflet_objects()
      studies <- objs$studies
      pal <- objs$pal

      # managing frames
      frame_n(frame_n() + 1)
      frame <- frame_n()

      if (frame == 1) {
        # draw the map
        output$map <- renderLeaflet({
          leaflet(data = studies) %>%
            leaflet::setMaxBounds(. ,-180, -90, 180, 90) %>%
            addProviderTiles(
              provider = "Esri.WorldGrayCanvas",
              options = providerTileOptions(
                minZoom = 3,
                maxZoom = 10,
                bounds = list(
                  list(-90, -180),
                  list(90, 180)
                ),
                noWrap = TRUE,
                updateWhenZooming = FALSE,
                updateWhenIdle = FALSE
              )
            ) %>%
            addPolygons(
              color = ~pal(votes),
              weight = 0,
              fillOpacity = 0.3,
              popup = ~label
            ) %>%
            addLegend(
              position = "bottomleft",
              title = "Evidence",
              bins = 1:-1,
              values = 1:-1,
              colors = pal(1:-1),
              labels = c("Positive", "Neutral", "Negative"),
              opacity = 1
            )
        })
      } else {
        # update the map
        leafletProxy(
          mapId = "map",
          session = session,
          data = studies
        ) %>%
          leaflet::clearShapes() %>%
          addPolygons(
            color = ~pal(votes),
            weight = 0,
            fillOpacity = 0.3,
            popup = ~label
          )
      }
    })
  })
}
