# packages
library(dplyr)
library(stringr)

source("app/logic/utils/utils.R")
source("app/logic/utils/utils_data.R")

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

# treating pathways independently
studies <- lapply(X = constants$pathways, FUN = clean_pathway)

# combine files
studies <- lapply(X = studies, FUN = read_csv_wrap) %>%
  bind_rows()

# summarize important variables
studies <- studies %>%
  mutate(
    MPAname = case_when(
      MPAname == "mix" ~ NA_character_,
      MPAname == "" ~ NA_character_,
      TRUE ~ MPAname
    ),
    country = str_to_title(country),
    climate = str_to_title(climate),
    continent = str_to_title(continent),
    ocean = str_to_title(ocean),
    ecosystem = str_to_title(ecosystem),
    mechanism = str_to_title(mechanism),
    country = case_when(
      country == "Alaska" ~ "United States",
      country == "Bengladesh" ~ "Bangladesh",
      country == "Great Britain" ~ "United Kingdom",
      country == "Guinea Bissau" ~ "United Kingdom",
      country == "Hawaii" ~ "United States",
      country == "Italia" ~ "Italy",
      country == "Netherland" ~ "Netherlands",
      country == "Phillipines" ~ "Philippines",
      country == "Puerto Rico" ~ "PR",
      country == "Saint Lucia" ~ "St Lucia",
      country == "Taiwai" ~ "Taiwan",
      country == "Timor Leste" ~ "Timor-Leste",
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
      country == "Timor-Leste" ~ "East Timor",
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
    ID_study = ID,
    ID = as.numeric(as.factor(x = MPAname))
  ) %>%
  left_join(y = mechanisms_df, by = "mechanism") %>%
  rename(name = MPAname) %>%
  filter(!is.na(mechanism_internal))

# save data
saveRDS(
  object = studies,
  file = "app/static/data/preprocessing/studies.RDS"
)
