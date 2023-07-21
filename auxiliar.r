model_matrix_metrics <- function(data, variable_matrix) {
    models_formula <- apply(variable_matrix, MARGIN = 2, function(names_col) {
        glue::glue("Poupanca ~ {paste(names_col, collapse = ' + ')}")
    })
    return(purrr::map(
        models_formula, ~ glm(data = data, formula = .x, family = "binomial")
    ))
}

best_round_threshold <- function(data, model, par) {
    yhat <- predict(model, data, type = "response") >= par
    return(sum(data$Poupanca == yhat) / nrow(data))
}