read_model <- function(disease_name) {
  readRDS(paste0("Models/Full Models/", disease_name, "_model.rds"))
}

read_testing_set <- function(disease_name) {
  readRDS(paste0("Models/Testing Sets/", disease_name, "_testing_set.rds"))
}

get_testing_x <- function(disease_name) {
  read_testing_set(disease_name) |> 
    select(-name_of_place, -any_of(disease_name))
}

get_testing_y <- function(disease_name) {
  read_testing_set(disease_name) |> 
    pull(any_of(disease_name))
}

predict_model <- function(disease_name) {
  read_model(disease_name) |> 
    predict(get_testing_x(disease_name))
}

mse <- function(observed, predicted) {
  (predicted - observed) ^ 2 |> 
    mean() 
}

rmse <- function(observed, predicted) {
  mse(observed, predicted) |> 
    sqrt()
}

add_start_zero <- function(vector) {
  vector |> 
    append(0, after = 0)
}

rg <- function(vector_a, vector_b, plot = FALSE) {
  if (length(vector_a) != length(vector_b)) {
    stop("Vector A and B have different lengths!")
  }
  
  n <- length(vector_a)
  
  y <- (0:n) / n
  
  lorenz_x <- vector_a |>
    sort() |> 
    cumsum() / (n * mean(vector_a))
  
  double_lorenz_x <- vector_a |> 
    sort(decreasing = TRUE) |> 
    cumsum() / (n * mean(vector_a))
  
  concordance_x <- vector_a[rank(vector_b)] |>
    cumsum() / (n * mean(vector_a))

  if (plot == TRUE) {
    plot(y, y, type = "l", xlab = "p", ylab = "f(p)")
    lines(add_start_zero(lorenz_x), y, col = "green")
    lines(add_start_zero(double_lorenz_x), y, col = "orange")
    lines(add_start_zero(concordance_x), y, col = "red")
  }
  
  sum(double_lorenz_x - concordance_x) /
    sum(double_lorenz_x - lorenz_x)
}

<<<<<<< HEAD
rga <- function(observed, predicted, plot = FALSE) {
=======
rga <- function(observed, predicted, plot) {
>>>>>>> 35baf826fdf8cc2a7a25244d9095f41581c2f6b1
  rg(observed, predicted, plot)
}

evaluate_model_accuracy <- function(disease_name, statistic, ...) {
  y_hat <- predict_model(disease_name)
  
  y <- get_testing_y(disease_name)
  
  statistic(y, y_hat, ...)
}

disease_names <- c(
  "dengue",
  "gastroenteritis",
  "bronchial_asthma",
  "dermatitis",
  "chronic_obstructive_pulmonary_disease",
  "pulmonary_hypertension"
)

tibble(
  model_name = disease_names,
  rmse = map_dbl(disease_names, \(x) evaluate_model_accuracy(x, rmse)),
  rga = map_dbl(disease_names, \(x) evaluate_model_accuracy(x, rga, TRUE))
  ) |> 
  write_csv(file = "Results/model_accuracies.csv")
