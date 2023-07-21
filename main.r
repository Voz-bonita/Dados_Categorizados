if (!require("pacman")) {
    install.packages("pacman")
}
pacman::p_load("readxl", "dplyr", "purrr", "glue")


raw_data <- read_excel("dados_trabalho.xlsx") %>%
    rename_all(~ c("ID", "Idade", "Socioecon", "Casa", "Setor", "Poupanca")) %>%
    select(-ID)

full_size <- nrow(raw_data)
n_cols <- ncol(raw_data)
sample_order <- sample(1:full_size, size = full_size, replace = FALSE)
train <- raw_data[1:100, ]
test <- raw_data[101:full_size, ]

model_matrix_metrics <- function(data, variable_matrix) {
    models_formula <- apply(variable_matrix, MARGIN = 2, function(names_col) {
        glue::glue("Poupanca ~ {paste(names_col, collapse = ' + ')}")
    })
    return(purrr::map(
        models_formula, ~ glm(data = data, formula = .x, family = "binomial")
    ))
}

all_models <- map(
    1:(n_cols - 1),
    ~ model_matrix_metrics(
        data = train,
        combn(names(train)[1:(n_cols - 1)], .x)
    )
) %>%
    reduce(c)

best_round_threshold <- function(data, model, par) {
    yhat <- predict(model, data, type = "response") >= par
    return(sum(data$Poupanca == yhat) / nrow(data))
}

n_models <- length(all_models)
model_info_to_plot <- data.frame(matrix(ncol = 4, nrow = n_models)) %>%
    rename_all(~ c("Parametros", "n_parametros", "AIC", "ACC"))
for (i in 1:n_models) {
    current_model <- all_models[[i]]
    model_info_to_plot[i, "Parametros"] <- current_model$formula
    model_info_to_plot[i, "n_parametros"] <- length(
        current_model$coefficients
    )
    model_info_to_plot[i, "AIC"] <- current_model$aic

    threshold <- optim(
        par = 0, fn = best_round_threshold, data = test, model = current_model,
        method = "Brent", lower = 0, upper = 1
    )$value

    ytrue <- test$Poupanca
    yhat <- predict(current_model, test, type = "response") >= threshold

    model_info_to_plot[i, "ACC"] <- sum(ytrue == yhat) / nrow(test)
}