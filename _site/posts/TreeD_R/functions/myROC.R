myROC <- function(predichos, reales) {
  suppressMessages(suppressWarnings(library(dplyr)))
  suppressMessages(suppressWarnings(library(ggplot2)))
  suppressMessages(suppressWarnings(library(pROC)))
  suppressMessages(suppressWarnings(library(Metrics)))
  x = roc(reales, predichos)
  df = data_frame(TPR = x$sensitivities,
                  FPR = 1 - x$specificities)
  gg = df %>%
    ggplot(aes(x = FPR, ymin = 0, ymax = TPR)) +
    geom_polygon(aes(y = TPR), fill = "#5A5156", alpha = 0.7) +
    geom_path(aes(y = TPR), col = "#F6222E", size = 1.3) +
    geom_abline(
      intercept = 0,
      slope = 1,
      color = "gray37",
      size = 1,
      linetype = "dashed"
    ) +
    theme_ipsum() +
    coord_equal() +
    labs(
      x = "FPR (1 - Especificidad)",
      y = "TPR (Sensitividad)",
      title = paste0("Curva ROC"),
      subtitle = paste0(
        "Valor AUC: ",
        Metrics::auc(actual = reales,
                     predicted = predichos) %>% round(4)
      )
    )
  return(gg)
}