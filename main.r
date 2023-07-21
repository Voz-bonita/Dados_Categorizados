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