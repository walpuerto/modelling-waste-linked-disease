predictor_columns <- predictors |> 
  select(-name_of_place)

reduced_sets <- predictor_columns |> 
  names() |> 
  map(\(x) select(predictor_columns, -any_of(x)))

names(reduced_sets) <- predictor_columns |> 
  names()

generate_reduced_model <- function(predictor_name, disease_name) {
  training_x <- reduced_sets[[predictor_name]] |>
    slice(training_indexes[[disease_name]])
  
  training_y <- observations |> 
    slice(training_indexes[[disease_name]]) |> 
    pull(any_of(disease_name))
  
  randomForest(training_x, y = training_y)
}

generate_all_reduced_models <- function(disease_name) {
  reduced_models <- predictor_columns |> 
    names() |> 
    map(\(x) generate_reduced_model(x, disease_name))
  
  names(reduced_models) <- predictor_columns |> 
    names()
  
  reduced_models
}

generate_all_reduced_models_and_save <- function(disease_name) {
  file_name <- paste0(
    "Models/Reduced Models/",
    disease_name,
    "_reduced_models.rds")
  
  disease_name |>
    generate_all_reduced_models() |> 
    saveRDS(file = file_name)
}

generate_all_reduced_models_and_save("dengue")

generate_all_reduced_models_and_save("gastroenteritis")

generate_all_reduced_models_and_save("bronchial_asthma")

generate_all_reduced_models_and_save("dermatitis")

generate_all_reduced_models_and_save("chronic_obstructive_pulmonary_disease")

generate_all_reduced_models_and_save("pulmonary_hypertension")
