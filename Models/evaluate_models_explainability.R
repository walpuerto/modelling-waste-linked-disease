predict_reduced_model <- function(variable_name, disease_name) {
  testing_x <- get_testing_x(disease_name) |> 
    select(-any_of(variable_name))
  
  readRDS(
    paste0("Models/Reduced Models/", disease_name, "_reduced_models.rds")
  ) |>
    _[[variable_name]] |> 
    predict(testing_x)
}

predict_all_reduced_models <- function(disease_name) {
  predictors |> 
    select(-name_of_place) |> 
    names() |> 
    map(\(x) predict_reduced_model(x, disease_name))
}

percent_inc_mse <- function(full_prediction, reduced_prediction, observed) {
  full_mse <- mse(full_prediction, observed)
  
  reduced_mse <- mse(reduced_prediction, observed)
  
  (reduced_mse - full_mse) * 100 / full_mse
}

rge <- function(full_prediction, reduced_prediction, observed) {
  rg(reduced_prediction, full_prediction)
}

evaluate_explainability <- function(disease_name, statistic) {
  observed <- get_testing_y(disease_name)
  
  full_prediction <- predict_model(disease_name)
  
  predict_all_reduced_models(disease_name) |> 
    map_dbl(\(x) statistic(full_prediction, x, observed))
}

generate_explainability_tibble <- function(disease_name) {
  tibble(
    predictor_name = names(select(predictors, -name_of_place)),
    percent_inc_mse = evaluate_explainability(disease_name, percent_inc_mse),
    rge = evaluate_explainability(disease_name, rge)
  )
}

model_explainabilities <- disease_names |> 
  map(generate_explainability_tibble)

names(model_explainabilities) <- disease_names
  
model_explainabilities |> 
  write_rds(file = "results/model_explainabilities.rds")
