library(tidyverse)
library(readxl)
library(janitor)

read_excel("Raw Data/DATA.xlsx", sheet = "Waste") |>
  clean_names() |> 
  mutate(collection_segregation = as_factor(collection_segregation)) |> 
  saveRDS(file = "Data/predictors.rds")

read_excel("Raw Data/DATA.xlsx", sheet = "Disease") |> 
  clean_names() |> 
  select_if(\(x) length(unique(x)) > 5) |> 
  saveRDS(file = "Data/observations.rds")
