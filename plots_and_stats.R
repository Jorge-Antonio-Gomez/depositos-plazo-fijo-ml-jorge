##### Función para cargar paquetes
################################################################################

# Función para cargar un vector de paquetes
load_packages <- function(packages) {
  
  # Instalar y cargar paquetes
  for (package in packages) {
  
  # Instalar paquete si no está disponible
  if (!require(package, character.only = TRUE)) {
    
    # Instalar paquete
    install.packages(package, dependencies = TRUE)
  }
  
  # Cargar paquete
  library(package, character.only = TRUE)
  
  # Mensaje de carga
  if (length(packages) == 1) {
    base::message(sprintf("📦 ¡El paquete %s ha sido cargado con éxito!", package))
  }
  }
  
  # Mensaje de finalización
  if (length(packages) > 1) {
  base::message(rep("-", 80))
  base::message("\n📦 ¡Todos los paquetes han sido cargados con éxito!")
  base::cat    ("\n    Paquetes cargados:\n")
  base::cat    ("\033[3m", paste0("   ", paste(packages, collapse = ", ")), "\033[0m\n\n")
  base::message(rep("-", 80))
  }
  
}

################################################################################



##### Conteo
################################################################################

plot_conteo_1 <- function(path = "Informe/img/job_distribution.pdf") {
  
  # Definir las traducciones de las categorías de trabajo
  traducciones_trabajo <- c(
    "management"     = "Gerencia",
    "technician"     = "Técnico",
    "entrepreneur"   = "Empresario",
    "blue-collar"    = "Oficio manual",
    "unknown"        = "Desconocido",
    "retired"        = "Jubilado",
    "admin."         = "Administrativo",
    "services"       = "Servicios",
    "self-employed"  = "Autoempleado",
    "unemployed"     = "Desempleado",
    "housemaid"      = "Amo/a de casa",
    "student"        = "Estudiante"
  )
  
  plt <- ggplot(data %>% mutate(y = factor(y, levels = c("no", "yes"))), aes(x = job, fill = y)) + 
  geom_bar(alpha = 0.8, width = 0.7) + 
  coord_flip() + 
  geom_hline(yintercept = 0, color = "#333333", linewidth = 0.5) +
  theme_minimal() +
  scale_fill_manual(
    values = c("yes" = "#46AF30", "no" = "#FF9800"),
    labels = c("yes" = "Sí", "no" = "No")
  ) +
  scale_x_discrete(
    labels = traducciones_trabajo
  ) +
  labs(
    title = "Suscripción de depósitos a plazo por categoría de trabajo",
    subtitle = "Conteo de clientes que aceptaron o rechazaron el producto, agrupados por categoría de trabajo",
    x = "Ocupaciones",
    y = "Cantidad",
    fill = "¿El cliente\nsuscribió el producto?"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#333333"),
    plot.subtitle = element_text(size = 12, color = "#666666"),
    axis.text = element_text(color = "#444444"),
    legend.position = "right",
    panel.grid.major.y = element_line(linetype = "dotted", color = "#CCCCCC"),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    labels = scales::comma_format()
  ) +
  guides(
    fill = guide_legend(
    title.position = "top",
    title.hjust = 0.5
    )
  )
  
  print(plt)
  ggsave(path, plt, width = 10, height = 5)
  
}

################################################################################



##### Proporciones
################################################################################

plot_proporcion_1 <- function(path = "Informe/img/job_distribution_prop.pdf") {
  
  # Definir las traducciones de las categorías de trabajo
  traducciones_trabajo <- c(
    "management"     = "Gerencia",
    "technician"     = "Técnico",
    "entrepreneur"   = "Empresario",
    "blue-collar"    = "Oficio manual",
    "unknown"        = "Desconocido",
    "retired"        = "Jubilado",
    "admin."         = "Administrativo",
    "services"       = "Servicios",
    "self-employed"  = "Autoempleado",
    "unemployed"     = "Desempleado",
    "housemaid"      = "Amo/a de casa",
    "student"        = "Estudiante"
  )
  
  plt <- ggplot(data %>% mutate(y = factor(y, levels = c("no", "yes"))), aes(x = job, fill = y)) + 
  geom_bar(alpha = 0.8, width = 0.7, position = "fill") + 
  coord_flip() + 
  geom_hline(yintercept = 0, color = "#333333", linewidth = 0.5) +
  geom_hline(yintercept = mean(data$y == "yes"), color = "#268F10", size = 1, linetype = "dashed", alpha = 0.8) +
  theme_minimal() +
  scale_fill_manual(
    values = c("yes" = "#46AF30", "no" = "#FF9800"),
    labels = c("yes" = "Sí", "no" = "No")
  ) +
  scale_x_discrete(
    labels = traducciones_trabajo
  ) +
  labs(
    title = "Suscripción de depósitos a plazo por categoría de trabajo",
    subtitle = "Proporción de clientes que aceptaron o rechazaron el producto, agrupados por categoría de trabajo",
    x = "Ocupación",
    y = "Proporción",
    fill = "¿El cliente\nsuscribió el producto?"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#333333"),
    plot.subtitle = element_text(size = 12, color = "#666666"),
    axis.text = element_text(color = "#444444"),
    legend.position = "right",
    panel.grid.major.y = element_line(linetype = "dotted", color = "#CCCCCC"),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    labels = scales::percent_format(),
    sec.axis = sec_axis(~ ., breaks = mean(data$y == "yes"), labels = scales::percent_format())
  ) +
  guides(
    fill = guide_legend(
    title.position = "top",
    title.hjust = 0.5
    )
  )
  
  print(plt)
  ggsave(path, plt, width = 10, height = 5)
  
}

################################################################################



##### Stats de estudiantes y retirados
################################################################################

some_jobs_sum_1 <- function() {
  
  some_jobs_sum <- data %>% filter(job == 'student' | job == 'retired') %>% 
  group_by(job) %>%
  summarise(
    yes = sum(y == 'yes'),
    no = sum(y == 'no')
  ) %>%
  mutate(prop = yes / (yes + no)) %>%
  print()
  
}

################################################################################



##### Histograma de predicciones (modelo 1)
################################################################################

plt_histogram_1 <- function(path = "Informe/img/pred_yes_distribution_model_1.pdf", tr = NULL) {
  
  density_yes <- density(results$.pred_yes[results$y == "yes"])
  density_no <- density(results$.pred_yes[results$y == "no"])
  
  max_density <- max(c(density_yes$y, density_no$y))
  
  if (is.null(tr)) {
  density_height <- max_density
  x_max_density <- density_yes$x[which.max(density_yes$y)]
  } else {
  height_yes <- approx(density_yes$x, density_yes$y, xout = tr)$y
  height_no <- approx(density_no$x, density_no$y, xout = tr)$y
  density_height <- max(height_yes, height_no, na.rm = TRUE)
  }
  
  plt <- ggplot(results %>% mutate(y = factor(y, levels = c("no", "yes"))), 
        aes(x = .pred_yes, fill = y)) + 
  geom_density(alpha = 0.7, color = "transparent") + 
  geom_hline(yintercept = 0, color = "#333333", linewidth = 0.5) +
  labs(
    title = "Distribución de probabilidades estimadas por clase real",
    x = "Probabilidad estimada de la clase 'Sí'",
    y = "Densidad",
    fill = "¿El cliente realmente\nsuscribió el producto?"
  ) +
  theme_minimal() + 
  scale_fill_manual(
    values = c("yes" = "#46AF30", "no" = "#FF9800"),
    labels = c("yes" = "Sí", "no" = "No")
  ) + 
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#333333"),
    axis.text = element_text(color = "#444444"),
    panel.grid.major.y = element_line(linetype = "dotted", color = "#CCCCCC"),
    panel.grid.minor = element_blank()
  ) + 
  scale_x_continuous(
    labels = scales::percent_format(),
    breaks = seq(0, 1, 0.05),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(limits = c(0, max_density * 1.2))
  
  if (!is.null(tr)) {
  plt <- plt +
    annotate(
    "segment",
    x = tr,
    xend = tr,
    y = 0,
    yend = density_height,
    color = "brown",
    linewidth = 0.5,
    linetype = "dashed"
    ) +
    annotate(
    "segment",
    x = tr,
    xend = tr,
    y = 0,
    yend = density_height,
    color = "#444",
    alpha = 0.2,
    linewidth = 0.5
    ) +
    annotate(
    "curve",
    x = tr,
    xend = tr,
    y = density_height + (0.11 * max_density),
    yend = density_height + (0.01 * max_density),
    curvature = 0,
    arrow = arrow(length = unit(0.02, "npc")),
    color = "#444",
    size = 0.3
    ) +
    annotate(
    "text",
    x = tr,
    y = density_height + (0.11 * max_density),
    label = paste("", scales::percent(tr, accuracy = 0.1)),
    color = "#444",
    size = 3,
    vjust = -0.3
    )
  }
  
  print(plt)
  ggsave(path, plt, width = 10, height = 4)
}


################################################################################



##### ROC AUC
################################################################################

roc_auc_curve_1 <- function(path = "Informe/img/roc_curve_m1.pdf") {
  
  roc_curve <- roc(
  response = results$y,
  predictor = results$.pred_yes,
  levels = c("no", "yes"),
  direction = "<")
  
  auc_val <- auc(roc_curve)
  
  roc_df <- data.frame(
  FPR = 1 - roc_curve$specificities,
  TPR = roc_curve$sensitivities
  )
  
  plt <- ggplot(roc_df, aes(x = FPR, y = TPR)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +
  xlab("Tasa de FP (FPR)") +
  ylab("Tasa de VP (TPR)") +
  theme_minimal() +
  annotate("text", x = 0.75, y = 0.08, label = paste("AUC =", round(auc_val, 3)), size = 3, color = "#167F10") +
  scale_x_continuous(
    labels = scales::percent_format(),
    expand = c(0.01, 0)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = c(0, 0.01)
  ) +
  theme(
    panel.border = element_rect(color = "#333333", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  ) +
  geom_line(color = "#46AF30", linewidth = 1) +
  theme(plot.margin = margin(t = 10, r = 12, b = 0, l = 4, unit = "pt"))
  
  print(plt)
  ggsave(path, plt, width = 2.5, height = 2)
  
}

################################################################################



##### F1 Score VS Threshold
################################################################################

f1_score_optimal_threshold <- function(path = "Informe/img/f1_score_optimization_m2.pdf") {
  
  plt <- ggplot(roc_curve_data_with_f1, aes(x = .threshold, y = f1_score)) +
  geom_line(color = "#46AF30", size = 1.2, alpha = 0.8) +
  geom_point(color = "#46AF30", size = 2, alpha = 0.7) +
  geom_point(
    data = ~ .x %>% slice(which.max(f1_score)),
    color = "brown", size = 4, show.legend = FALSE
  ) +
  annotate(
    "text",
    x = roc_curve_data_with_f1$.threshold[which.max(roc_curve_data_with_f1$f1_score)] + 0.07,
    y = max(roc_curve_data_with_f1$f1_score) - 0.05,
    label = paste("Máx F1-Score:", 
          scales::comma(max(roc_curve_data_with_f1$f1_score), accuracy = 0.01)),
    color = "brown",
    size = 3.5
  ) +
  labs(
    title = "Optimización del F1-Score según el umbral de clasificación",
    x = "Umbral de Clasificación",
    y = "F1-Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#333333", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "#666666", hjust = 0.5, margin = margin(b = 20)),
    axis.title = element_text(face = "bold", color = "#444444"),
    axis.text = element_text(color = "#666666"),
    panel.grid.major = element_line(color = "#ECECEC", linetype = "dotted"),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.1),
    limits = c(0, 1),
    expand = expansion(mult = 0.02)
  ) +
  scale_y_continuous(
    labels = scales::comma_format(accuracy = 0.1),
    breaks = seq(0, 1, 0.1),
    limits = c(0, 1),
    expand = expansion(mult = 0.02)
  ) +
  geom_hline(
    yintercept = 1, 
    linetype = "dashed", 
    color = "#AAAAAA", 
    alpha = 0.5,
    size = 0.4
  ) +
  geom_vline(
    xintercept = roc_curve_data_with_f1$.threshold[which.max(roc_curve_data_with_f1$f1_score)],
    linetype = "dashed",
    color = "brown",
    alpha = 0.6,
    size = 0.6
  )
  
  print(plt)
  ggsave(path, plt, width = 6, height = 4)
  
}

################################################################################




##### Función para extraer métricas de una matriz de confusión
################################################################################

conf_mat_metrics <- function(conf_mat) {
  
  conf_mat <- conf_mat
  
  accuracy     <- (conf_mat[1,1] + conf_mat[2,2]) / sum(conf_mat)
  sensitivity  <- conf_mat[1,1] / sum(conf_mat[,1])
  specificity  <- conf_mat[2,2] / sum(conf_mat[,2])
  precision    <- conf_mat[1,1] / sum(conf_mat[1,])
  f1_score     <- 2 * (conf_mat[1,1] / sum(conf_mat[1,])) * (conf_mat[1,1] / sum(conf_mat[,1])) / ((conf_mat[1,1] / sum(conf_mat[1,])) + (conf_mat[1,1] / sum(conf_mat[,1])))
  youden_index <- (conf_mat[1,1] / sum(conf_mat[,1])) + (conf_mat[2,2] / sum(conf_mat[,2])) - 1
  
  return(data.frame(accuracy, sensitivity, specificity, precision, f1_score, youden_index))
  
}

################################################################################
