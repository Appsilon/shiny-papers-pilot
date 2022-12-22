# packages
library(dplyr)
library(sf)
library(stringr)
library(spData)

# get MPAs in use in our paper
studies <- readRDS(file = "app/static/data/preprocessing/studies.RDS")

# Get authors' shp files for all geometries in the article
shp_mitigation <- st_read(dsn = "app/static/data/originals/locations_mitigation.shp") %>%
  rename(MPA = "location", nb_studies = "nb.studies")
shp_adaptation <- st_read(dsn = "app/static/data/originals/locations_adaptation.shp") %>%
  rename(mecanism = "Mecanism", ID = "id")

shp_all <- bind_rows(shp_mitigation, shp_adaptation)
shp_all %>% as_tibble() %>% count(MPA, mecanism) %>% filter(n > 1)

# read shape files and filter by our MPAs
# shape files can be downloaded at https://www.protectedplanet.net/en
#
# shp <- bind_rows(shp1, shp2, shp3)
# rm("shp1", "shp2", "shp3")
# gc(reset = TRUE)


# TODO: Using country instead of MPA
studies <- studies %>%
  # filter(!is.na(name) & name != "mix" & name != "") %>%
  # select(name, ID) %>%
  select(country) %>%
  unique()

# shp <- shp %>%
#   arrange(desc(GIS_AREA)) %>%
#   mutate(ID = 1:n()) # nolint

# based on countries
countries <- studies %>%
  left_join(world %>% select(-continent), by = c("country" = "name_long"))

# mpas <- studies %>%
#   select(-country, -name) %>%
#   left_join(shp, by = "ID") %>%
#   st_as_sf() %>%
#   st_transform(crs = 4087) %>%
#   st_simplify(dTolerance = 1000) %>%
#   st_transform(crs = 4326)

# save it
# saveRDS(object = mpas, file = "app/static/data/preprocessing/mpas_shp.RDS")
saveRDS(object = countries, file = "app/static/data/preprocessing/countries_shp.RDS")
