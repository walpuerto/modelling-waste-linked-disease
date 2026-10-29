library("randomForest")

predictors <- readRDS("Data/predictors.rds")
observations <- readRDS("Data/observations.RDS")

set.seed(1000)

sample_random_indexes <- function() {
  sample.int(nrow(predictors), 0.7 * nrow(predictors))
}

training_indexes <- tibble(
  dengue = sample_random_indexes(),
  gastroenteritis = sample_random_indexes(),
  bronchial_asthma = sample_random_indexes(),
  dermatitis = sample_random_indexes(),
  chronic_obstructive_pulmonary_disease = sample_random_indexes(),
  pulmonary_hypertension = sample_random_indexes()
)

generate_model <- function(disease_name) {
  training_x <- predictors |> 
    select(-name_of_place) |> 
    slice(training_indexes[[disease_name]])
  
  training_y <- observations |> 
    slice(training_indexes[[disease_name]]) |> 
    pull(any_of(disease_name))
  
  randomForest(training_x, y = training_y)
}

save_model <- function(model, disease_name) {
  model |>
    saveRDS(paste0("Models/Full Models/", disease_name, "_model.rds"))
}

generate_and_save_model <- function(disease_name) {
  generate_model(disease_name) |>
    save_model(disease_name)
}

generate_and_save_model("dengue")

generate_and_save_model("gastroenteritis")

generate_and_save_model("bronchial_asthma")

generate_and_save_model("dermatitis")

generate_and_save_model("chronic_obstructive_pulmonary_disease")

generate_and_save_model("pulmonary_hypertension")

get_inverse_indexes <- function(training_indices) {
  (1:nrow(predictors))[-training_indices]
}

testing_indexes <- training_indexes |> 
  map(get_inverse_indexes) |> 
  as_tibble()

generate_testing_set <- function(disease_name) {
  x <- predictors
  
  y <- observations |> 
    select(name_of_place, any_of(disease_name)) |> 
  
  left_join(x, y, by = join_by(name_of_place)) |> 
    slice(testing_indexes[[disease_name]])
} 

save_testing_set <- function(testing_set, disease_name) {
  testing_set |> 
    saveRDS(
      file = paste0("Models/Testing Sets/", disease_name, "_testing_set.rds")
      )
}

generate_and_save_testing_set <- function(disease_name) {
  generate_testing_set(disease_name) |> 
    save_testing_set(disease_name)
}

generate_and_save_testing_set("dengue")

generate_and_save_testing_set("gastroenteritis")

generate_and_save_testing_set("bronchial_asthma")

generate_and_save_testing_set("dermatitis")

generate_and_save_testing_set("chronic_obstructive_pulmonary_disease")

generate_and_save_testing_set("pulmonary_hypertension")
