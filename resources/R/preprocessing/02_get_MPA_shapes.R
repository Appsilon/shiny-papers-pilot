# packages
library(dplyr)
library(sf)
library(stringr)

# get MPAs in use in our paper
studies <- readRDS(file = "data/preprocessing/studies.RDS")

# read shape files and filter by our MPAs
# shape files can be downloaded at https://www.protectedplanet.net/en
shp1 <- st_read(dsn = "data/shp/WDPA_WDOECM_oct2022_Public_all_shp/polygons/1/WDPA_WDOECM_Oct2022_Public_all_shp-polygons.shp")
shp2 <- st_read(dsn = "data/shp/WDPA_WDOECM_oct2022_Public_all_shp/polygons/2/WDPA_WDOECM_Oct2022_Public_all_shp-polygons.shp")
shp3 <- st_read(dsn = "data/shp/WDPA_WDOECM_oct2022_Public_all_shp/polygons/3/WDPA_WDOECM_Oct2022_Public_all_shp-polygons.shp")

shp <- bind_rows(shp1, shp2, shp3)
rm("shp1", "shp2", "shp3")
gc(reset = TRUE)

# TODO: It is completely wrong. It is just to keep coding
studies <- studies %>%
  select(name, ID) %>%
  unique()

shp <- shp %>%
  arrange(desc(GIS_AREA)) %>%
  mutate(ID = 1:n())

studies <- studies %>%
  left_join(shp, by = "ID")

# save it
saveRDS(object = studies, file = "data/preprocessing/mpas_shp.RDS")
