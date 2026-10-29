library(tidyverse)
library(readxl)
library(janitor)

read_excel("Raw Data/DATA.xlsx", sheet = "Waste") |>
  clean_names() |> 
  mutate(
    reported_collection_segregation = reported_collection_segregation |> 
      as_factor()
    ) |> 
  saveRDS(file = "Data/predictors.rds")

read_excel("Raw Data/DATA.xlsx", sheet = "Disease") |> 
  clean_names() |> 
  select_if(\(x) length(unique(x)) > 5) |> 
  saveRDS(file = "Data/observations.rds")
