if (!require("pacman")) {
    install.packages("pacman")
}
pacman::p_load(
    "readxl", "dplyr", "purrr",
    "glue", "ggplot2", "ggpubr",
    "kableExtra", "caret", "pROC",
    "plotROC"
)
# install.packages("arm")
source("auxiliar.r", encoding = "UTF-8")

set.seed(2023)
raw_data <- read_excel("dados_trabalho.xlsx") %>%
    rename_all(~ c("ID", "Idade", "Socioecon", "Casa", "Setor", "Poupanca")) %>%
    select(-c(ID))

full_size <- nrow(raw_data)
n_cols <- ncol(raw_data)
sample_order <- sample(1:full_size, size = full_size, replace = FALSE)
train <- raw_data[1:100, ]
test <- raw_data[101:full_size, ]


covariables <- names(train)[names(train) != "Poupanca"]
all_models <- map(
    1:(n_cols - 1),
    ~ model_matrix_metrics(
        data = train,
        combn(covariables, .x)
    )
) %>%
    reduce(c)

n_models <- length(all_models)
saturated_model <- all_models[[n_models]]

# RV para modelo saturado contra modelo apenas com intercepto
g2 <- saturated_model$null.deviance - saturated_model$deviance
g2_gl <- saturated_model$df.null - saturated_model$df.residual
pchisq(g2, g2_gl, lower.tail = FALSE)

model_info_to_plot <- data.frame(matrix(ncol = 8, nrow = n_models)) %>%
    rename_all(~ c(
        "Parametros", "n_parametros",
        "G^2", "g.l.", "p-valor",
        "AIC", "ACC", "round_threshold"
    ))

for (i in 1:n_models) {
    current_model <- all_models[[i]]
    model_info_to_plot[i, "Parametros"] <- current_model$formula
    model_info_to_plot[i, "n_parametros"] <- length(
        current_model$coefficients
    )
    g2 <- current_model$deviance - saturated_model$deviance
    g2_gl <- current_model$df.residual - saturated_model$df.residual
    model_info_to_plot[i, "G^2"] <- g2
    model_info_to_plot[i, "g.l."] <- g2_gl
    model_info_to_plot[i, "p-valor"] <- pchisq(
        g2, g2_gl,
        lower.tail = FALSE
    )
    model_info_to_plot[i, "AIC"] <- current_model$aic

    threshold <- optim(
        par = 0, fn = best_round_threshold, data = test, model = current_model,
        method = "Brent", lower = 0, upper = 1
    )$value
    model_info_to_plot[i, "round_threshold"] <- threshold
    ytrue <- test$Poupanca
    yhat <- predict(current_model, test, type = "response") >= threshold

    model_info_to_plot[i, "ACC"] <- sum(ytrue == yhat) / nrow(test)
}

model_info_to_plot %>%
    select(-n_parametros) %>%
    filter(`p-valor` > 0.05) %>%
    format_tab("\\label{tab:mod_sel}Modelos não rejeitados pelo teste da razão de verossimilhança", digits = 3, format = "latex")

aic_plot <- ggplot(
    data = model_info_to_plot,
    aes(x = `n_parametros`, y = `AIC`)
) +
    geom_point(size = 5) +
    xlab("Quantidade de parâmetros") +
    ylab("AIC") +
    theme_bw()

acc_plot <- ggplot(
    data = model_info_to_plot,
    aes(x = `n_parametros`, y = `ACC`)
) +
    geom_point(size = 5) +
    xlab("Quantidade de parâmetros") +
    ylab("Acurácia") +
    theme_bw()


ggarrange(aic_plot, acc_plot) %>%
    ggsave(filename = "assets/MetricasXParametros.png")

selected_index <- which.min(model_info_to_plot[["AIC"]])
selected_model <- all_models[[selected_index]]
s_model_threshold <- model_info_to_plot[selected_index, "round_threshold"]
pred_train <- as.numeric(fitted(selected_model) >= s_model_threshold)
pred_test <- as.numeric(predict(selected_model, test, type = "response") >= s_model_threshold)

bin_res <- arm::binned.resids(
    fitted(selected_model), 
    residuals(selected_model, type = "response"), 
    nclass = 8)$binned %>% 
    cbind("n_class" = 8) %>%
    rbind(cbind(
        arm::binned.resids(
            fitted(selected_model), 
            residuals(selected_model, type = "response"), 
            nclass = 15)$binned,
            "n_class" = 15
        )) %>%
    as.data.frame()

(ggplot(data = bin_res) +
    geom_point(aes(x = `xbar`, y = `ybar`, colour = "Resíduo Médio"), size = 5) +
    geom_line(aes(x = `xbar`, y = `2se`, colour = "Tolerância"), linewidth = 2) +
    geom_line(aes(x = `xbar`, y = -`2se`, colour = "Tolerância"), linewidth = 2) +
    scale_color_manual(name = "", values = c("Resíduo Médio" = "black", "Tolerância" = "steelblue")) +
    facet_wrap(vars(n_class), ncol=1, labeller = labeller(n_class = 
        c(
            "8" = "8 Agrupamentos",
            "15" = "15 Agrupamentos"
        )    
    )) +
    xlab("Valor Médio Predito") + ylab("Resíduo Médio") +
    theme_bw()) %>%
    ggsave(filename = "assets/Residuos.png")

conf_mat <- confusionMatrix(factor(pred_test), factor(test$Poupanca), positive = "1")
conf_mat$table
conf_mat$byClass[c("Sensitivity", "Specificity")]

png("assets/ROC.png")
plot.roc(
    test$Poupanca, predict(selected_model, test, type = "response"),
    percent=TRUE,
    print.thres=c(s_model_threshold, 0.569),
    col = "red",
    print.auc = TRUE,
    xlab = "Sensibilidade",
    ylab = "Especificidade"
) 
dev.off()

confusionMatrix(factor(as.numeric(fitted(selected_model) >= s_model_threshold)), factor(train$Poupanca), positive = "1")