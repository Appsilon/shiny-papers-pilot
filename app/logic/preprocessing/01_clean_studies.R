# packages
library(dplyr)
library(stringr)

source("app/logic/utils/utils.R")

# get the data
# TODO: my guess is that we have a different file with more complete info:
# all_studies.csv
constants <- read_yaml(file = "app/static/constants/constants.yml")
studies <- read.csv("app/static/data/originals/data_s2.csv", colClasses = "character")
mechanisms <- sapply(X = constants$pathways, FUN = "[[", "label")
mechanisms_df <- data.frame(
  mechanism = mechanisms,
  mechanism_internal = names(mechanisms)
)

# summarize important variables
# TODO: check what "mix" means for MPAname...are these studies spanning several
# MPAs? If so, do we know which MPAs?
# For now, we might go by assuming that each study is about 1 MPA only.
studies <- studies %>%
  filter(!is.na(MPAname) & MPAname != "mix" & MPAname != "") %>%
  mutate(
    country = str_to_title(country),
    climate = str_to_title(climate),
    continent = str_to_title(continent),
    ocean = str_to_title(ocean),
    ecosystem = str_to_title(ecosystem),
    mechanism = str_to_title(mechanism),
    country = case_when(
      country == "Bengladesh" ~ "Bangladesh",
      country == "Great Britain" ~ "United Kingdom",
      country == "Hawaii" ~ "United States",
      country == "Italia" ~ "Italy",
      country == "Netherland" ~ "Netherlands",
      country == "Phillipines" ~ "Philippines",
      country == "Puerto Rico" ~ "PR",
      country == "Saint Lucia" ~ "St Lucia",
      country == "Timor Leste" ~ "East Timor",
      country == "Tobago" ~ "Trinidad and Tobago",
      country == "Usa" ~ "United States",
      TRUE ~ country
    ),
    flag = case_when(
      country == "Guam" ~ "GM",
      country == "Northern Ireland" ~ "UK",
      country == "Saint Martin" ~ "St Maarten",
      country == "Solomon Islands" ~ "Solomon",
      country == "Taiwan" ~ "TW",
      country == "Trinidad and Tobago" ~ "Trinidad",
      country == "Turks And Caicos" ~ "Turks Caicos",
      country == "United Kingdom" ~ "UK",
      TRUE ~ country
    ),
    flag = get_flag_link(flag = flag),
    direction = case_when(
      direction == "neutral " ~ "neutral",
      direction == "positive " ~ "positive",
      TRUE ~ direction
    ),
    ID = as.numeric(as.factor(x = MPAname))
  ) %>%
  left_join(y = mechanisms_df, by = "mechanism") %>%
  rename(name = MPAname) %>%
  filter(!is.na(mechanism_internal))

# TODO: It seems that "Phenotypic plasticity" and "Connectivity" mechanisms are not on the data
# But there are 2 mechanism in the data that are absent in the paper: "Recovery" and "Resistance".

# save data
saveRDS(object = studies, file = "app/static/data/preprocessing/studies.RDS")
