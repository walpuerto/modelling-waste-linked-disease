convert_some_to_all <- function(factor) {
  ifelse(
    factor == "SOME AREAS",
    "ALL AREAS",
    ifelse(
      factor == "ALL AREAS",
      "ALL AREAS",
      "NONE" 
      )
    )
}

perturb_testing_x <- function(disease_name) {
  custom_levels <- get_testing_x(disease_name) |>
    pull("collection_segregation") |>
    levels()

  get_testing_x(disease_name) |> 
    mutate(
      total_waste = 0.85 * total_waste,
      biodegradable = 0.85 * biodegradable,
      recyclable = 0.85 * recyclable,
      residual = 0.85 * residual,
      special = 0.85 * special
    ) |>
    mutate(
      collection_segregation = factor(
        convert_some_to_all(collection_segregation),
        levels = custom_levels)
    ) |>
    mutate(
      dumpsite = ifelse(dumpsite == TRUE, FALSE, FALSE)
    )
}

predict_perturbed <- function(disease_name) {
  testing_x <- perturb_testing_x(disease_name)
  
  read_model(disease_name) |> 
    predict(testing_x)
}

rge <- function(full_prediciton, perturbed_prediction, observed) {
  rg(full_prediciton, perturbed_prediction) 
}

evaluate_robustness <- function(disease_name, statistic) {
  full_prediction <- predict_model(disease_name)
  
  perturbed_prediction <- predict_perturbed(disease_name)
  
  observed <- get_testing_y(disease_name)
  
  statistic(full_prediction, perturbed_prediction, observed)
}

tibble(
  model_names = disease_names,
  percent_inc_mse =
    map_dbl(disease_names, \(x) evaluate_robustness(x, percent_inc_mse)),
  rge = map_dbl(disease_names, \(x) evaluate_robustness(x, rge))
) |> 
  write_csv(file = "Results/model_robustness.csv")
