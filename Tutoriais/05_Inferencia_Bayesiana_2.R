## ----setup, echo=FALSE----------------------------------------------------------------------------------------------
library(rmarkdown)
knitr::opts_chunk$set(eval = TRUE, fig.asp = 1)
# Área de transferência
htmltools::tagList(
  xaringanExtra::use_clipboard(
    button_text = "Copiar código <i class=\"fa fa-clipboard\"></i>",
    success_text = "Copiado! <i class=\"fa fa-check\" style=\"color: #90BE6D\"></i>",
    error_text = "Não copiado <i class=\"fa fa-times-circle\" style=\"color: #F94144\"></i>"
  ),
  rmarkdown::html_dependency_font_awesome()
)


## ----load_model-----------------------------------------------------------------------------------------------------
animals <- MASS::Animals
animals$log_body <- log(animals$body) 
animals$log_brain <- log(animals$brain)
large_dinosaurs <- c("Brachiosaurus", "Dipliodocus", "Triceratops")
dinosaur_labels <- c("Brachiosaurus", "Diplodocus", "Triceratops")
dinosaur_indices <- match(large_dinosaurs, rownames(animals))
animals_without_dinosaurs <- animals[-dinosaur_indices, ]
point_colors <- ifelse(seq_len(nrow(animals)) %in% dinosaur_indices,
                       "firebrick", "grey35")

library(ggplot2)
library(ggrepel)

plot_animals <- ggplot(animals, aes(x = log_body, y = log_brain)) +
  geom_point(aes(color = point_colors), size = 2) +
  scale_color_identity() +
  labs(x = "Log body weight (kg)", y = "Log brain weight (g)") +
  geom_text_repel(aes(label = ifelse(seq_len(nrow(animals)) %in% dinosaur_indices, rownames(animals), "")), 
                  color = "firebrick", size = 3, hjust = -0.1) +
  theme_minimal()
plot_animals


## ----prior_slope_simulation, eval=FALSE, fig.width=7, fig.height=5--------------------------------------------------
# set.seed(123)
# 
# n_prior_draws <- 30
# a_prior <- rnorm(n_prior_draws, mean = 0, sd = 1)
# b_prior <- rnorm(n_prior_draws, mean = 0, sd = 1)
# body_center <- mean(animals_without_dinosaurs$log_body)
# brain_center <- mean(animals_without_dinosaurs$log_brain)
# log_body_grid <- seq(
#   min(animals_without_dinosaurs$log_body) - body_center,
#   max(animals_without_dinosaurs$log_body) - body_center,
#   length.out = 100
# )
# 
# prior_lines <- do.call(rbind, lapply(seq_along(b_prior), function(draw_index) {
#   data.frame(
#     log_body_c = log_body_grid,
#     log_brain_c = a_prior[draw_index] + b_prior[draw_index] * log_body_grid,
#     draw = draw_index
#   )
# }))
# external_brain_mass_g <- c(0.06, 9200)
# external_brain_limits <- data.frame(
#   log_brain_c = log(external_brain_mass_g) - brain_center,
#   reference = c("Musaranho (0,06 g)", "Cachalote (9,2 kg)")
# )
# 
# ggplot() +
#   geom_line(data = prior_lines,
#             aes(x = log_body_c, y = log_brain_c, group = draw),
#             color = "steelblue", alpha = 0.3, linewidth = 0.7) +
#   geom_hline(data = external_brain_limits,
#              aes(yintercept = log_brain_c, linetype = reference),
#              color = "firebrick", linewidth = 0.6, alpha = 0.8,
#              inherit.aes = FALSE) +
#   scale_linetype_manual(
#     name = NULL,
#     values = c(
#       "Musaranho (0,06 g)" = "dashed",
#       "Cachalote (9,2 kg)" = "dotted"
#     )
#   ) +
#   labs(x = "Centered log body weight", y = "Centered log brain weight") +
#   theme_minimal() +
#   theme(legend.position = "right", legend.text = element_text(size = 8)) +
#   guides(linetype = guide_legend(ncol = 1))


## ----model_code, eval=FALSE, hide=TRUE------------------------------------------------------------------------------
# library(rethinking)
# 
# # Centralizando os dados. Compare ess_bulk com e sem a centralização.
# # A centralização ajuda a reduzir a correlação entre os parâmetros, o que melhora a eficiência do amostrador.
# animals_without_dinosaurs$log_body_c <- scale(animals_without_dinosaurs$log_body, center = TRUE, scale = FALSE)
# animals_without_dinosaurs$log_brain_c <- scale(animals_without_dinosaurs$log_brain, center = TRUE, scale = FALSE)
# 
# fit <- ulam(
#   alist(
#     log_brain_c ~ dnorm(mu, sigma),
#     mu <- a + b * log_body_c,
#     a ~ dnorm(0, 1),
#     b ~ dnorm(0, 1),
#     sigma ~ dexp(1)
#   ),
#   data = animals_without_dinosaurs,
#   chains = 4, cores = 4, iter = 2000
# )


## ----model_diagnostics, eval=FALSE----------------------------------------------------------------------------------
# precis(fit)


## ----extract_chain_draws, eval=FALSE--------------------------------------------------------------------------------
# draws <- attr(fit, "cstanfit")$draws(format = "draws_array")


## ----trace_plot, eval=FALSE-----------------------------------------------------------------------------------------
# bayesplot::mcmc_trace(draws, pars = c("a", "b", "sigma"))


## ----density_overlay, eval=FALSE------------------------------------------------------------------------------------
# bayesplot::mcmc_dens_overlay(draws, pars = c("a", "b", "sigma"))


## ----precis_output, eval=FALSE--------------------------------------------------------------------------------------
# precis(fit)
# plot(precis(fit))


## ----mcmc_intervals, eval=FALSE-------------------------------------------------------------------------------------
# bayesplot::mcmc_intervals(draws, pars = c("a", "b", "sigma"),
# prob = 0.5, prob_outer = 0.95)
# bayesplot::mcmc_areas(draws, pars = c("a", "b", "sigma"),
# prob = 0.95)


## ----prior_posterior_comparison, eval=FALSE-------------------------------------------------------------------------
# set.seed(123)
# prior <- list(a = rnorm(4000, 0, 1), b = rnorm(4000, 0, 1), sigma = rexp(4000, 1))
# posterior <- rethinking::extract.samples(fit)
# 
# draws_to_long <- function(samples, distribution) {
#   do.call(rbind, lapply(names(samples), function(parameter) {
#     data.frame(parameter, value = as.numeric(samples[[parameter]]), distribution)
#   }))
# }
# 
# draws <- rbind(draws_to_long(prior, "A priori"),
#                draws_to_long(posterior, "A posteriori"))
# 
# ggplot(draws, aes(value, color = distribution, fill = distribution)) +
#   geom_density(alpha = 0.15) +
#   facet_wrap(~parameter, scales = "free") +
#   scale_color_manual(values = c("A priori" = "firebrick", "A posteriori" = "steelblue")) +
#   scale_fill_manual(values = c("A priori" = "firebrick", "A posteriori" = "steelblue")) +
#   labs(x = "Valor do parâmetro", y = "Densidade", color = NULL, fill = NULL) +
#   theme_minimal()


## ----prior_posterior_regression, eval=FALSE-------------------------------------------------------------------------
# 
# # prior
# prior_draws <- data.frame(a = rnorm(50, 0, 1),
#                           b = rnorm(50, 0, 1),
#                           distribution = "A priori")
# 
# # posterior
# posterior <- rethinking::extract.samples(fit)
# posterior_index <- sample(seq_along(posterior$a), 50)
# posterior_draws <- data.frame(
#   a = as.numeric(posterior$a[posterior_index]),
#   b = as.numeric(posterior$b[posterior_index]),
#   distribution = "A posteriori"
# )
# draws <- rbind(prior_draws, posterior_draws)
# 
# body_center <- mean(animals_without_dinosaurs$log_body)
# brain_center <- mean(animals_without_dinosaurs$log_brain)
# log_body_grid <- seq(min(animals_without_dinosaurs$log_body),
#                      max(animals_without_dinosaurs$log_body), length.out = 100)
# regression_lines <- do.call(rbind, lapply(seq_len(nrow(draws)), function(i) {
#   data.frame(
#     log_body = log_body_grid,
#     log_brain = brain_center + draws$a[i] + draws$b[i] * (log_body_grid - body_center),
#     distribution = draws$distribution[i],
#     draw = i
#   )
# }))
# 
# ggplot() +
#   geom_point(data = animals_without_dinosaurs,
#              aes(log_body, log_brain), color = "grey30", size = 2) +
#   geom_line(data = regression_lines,
#             aes(log_body, log_brain, group = draw, color = distribution),
#             alpha = 0.2, linewidth = 0.6) +
#   scale_color_manual(values = c("A priori" = "firebrick", "A posteriori" = "steelblue")) +
#   labs(x = "Log body weight (kg)", y = "Log brain weight (g)", color = NULL) +
#   theme_minimal()


## ----predictive_simulation, eval=FALSE------------------------------------------------------------------------------
# set.seed(123)
# prediction_data <- animals_without_dinosaurs
# y_rep <- rethinking::sim(fit, data = prediction_data)
# mean_posterior <- colMeans(y_rep)
# replicate_index <- sample(seq_len(nrow(y_rep)), 30)
# y_rep <- y_rep[replicate_index, , drop = FALSE]
# replicated_density <- data.frame(
#   log_brain_c = as.vector(y_rep),
#   replicate = factor(rep(seq_len(nrow(y_rep)), times = ncol(y_rep)))
# )
# mean_posterior_density <- data.frame(log_brain_c = as.vector(mean_posterior))
# 
# ggplot() +
#   geom_density(data = replicated_density,
#                aes(log_brain_c, group = replicate, color = "Réplicas preditivas"),
#                alpha = 0.1, linewidth = 0.1) +
#   geom_density(data = prediction_data,
#                aes(log_brain_c, color = "Observado"), linewidth = 1) +
#   geom_density(data = mean_posterior_density,
#                aes(log_brain_c, color = "Média posterior preditiva"),
#                linetype = "dashed", linewidth = 1) +
#   scale_color_manual(values = c(
#     "Observado" = "grey20",
#     "Média posterior preditiva" = "firebrick",
#     "Réplicas preditivas" = "steelblue"
#   )) +
#   labs(x = "Log brain weight, centered", y = "Density", color = NULL) +
#   theme_minimal()


## ----predictive_simulation_original_scale, eval=FALSE---------------------------------------------------------------
# set.seed(123)
# prediction_data <- animals_without_dinosaurs
# 
# # 100 amostras da posterior preditiva para cada valor do preditor
# y_rep <- rethinking::sim(fit, data = prediction_data)[sample(1:1000, 100), ]
# brain_center <- mean(prediction_data$log_brain)
# simulated_pairs <- data.frame(
#   log_body = rep(prediction_data$log_body, each = nrow(y_rep)),
#   log_brain = brain_center + as.vector(y_rep)
# )
# 
# ggplot() +
#   geom_point(data = simulated_pairs,
#              aes(log_body, log_brain, color = "Simulado"), alpha = 0.12, size = 0.8) +
#   geom_point(data = prediction_data,
#              aes(log_body, log_brain, color = "Observado"), size = 2) +
#   scale_color_manual(values = c("Simulado" = "steelblue", "Observado" = "grey20"), name = NULL) +
#   labs(x = "Log body weight (kg)", y = "Log brain weight (g)") +
#   theme_minimal()


## ----milk_data_pairs, eval=FALSE------------------------------------------------------------------------------------
# data(milk, package = "rethinking")
# milk_data <- milk[complete.cases(milk[c("kcal.per.g", "perc.fat", "perc.lactose")]), ]
# cor(milk_data[c("kcal.per.g", "perc.fat", "perc.lactose")])
# 
# milk_panel <- function(x, y, ...) {
#   points(x, y, pch = 21, bg = 1, col = "white")
#   abline(lm(y ~ x), col = "tomato3", lwd = 1)
# }
# 
# pairs(milk_data[c("kcal.per.g", "perc.fat", "perc.lactose")],
#       panel = milk_panel, cex.labels = 0.9)
# 


## ----milk_standardize, eval=FALSE-----------------------------------------------------------------------------------
# milk_fit <- data.frame(
#   K = as.numeric(scale(milk_data$kcal.per.g)),
#   F = as.numeric(scale(milk_data$perc.fat)),
#   L = as.numeric(scale(milk_data$perc.lactose))
# )
# 


## ----milk_fat_model, eval=FALSE, results='hide'---------------------------------------------------------------------
# library(rethinking)
# fit_fat <- ulam(
#   alist(
#     K ~ normal(mu, sigma),
#     mu <- a + bF * F,
#     a ~ normal(0, 0.2),
#     bF ~ normal(0, 0.5),
#     sigma ~ exponential(1)
#   ),
#   data = milk_fit, chains = 4, cores = 4, iter = 2000, refresh = 0,
#   log_lik = TRUE
# )


## ----milk_fat_summary, eval=FALSE-----------------------------------------------------------------------------------
# precis(fit_fat)


## ----milk_lactose_model, eval=FALSE, results='hide'-----------------------------------------------------------------
# fit_lactose <- ulam(
#   alist(
#     K ~ normal(mu, sigma),
#     mu <- a + bL * L,
#     a ~ normal(0, 0.2),
#     bL ~ normal(0, 0.5),
#     sigma ~ exponential(1)
#   ),
#   data = milk_fit, chains = 4, cores = 4, iter = 2000, refresh = 0,
# log_lik = TRUE
# )


## ----milk_lactose_summary, eval=FALSE-------------------------------------------------------------------------------
# precis(fit_lactose)


## ----milk_model_comparison, eval=FALSE------------------------------------------------------------------------------
# library(bayesplot)
# library(ggplot2)
# library(patchwork)
# 
# post_fat <- rethinking::extract.samples(fit_fat)
# post_lactose <- rethinking::extract.samples(fit_lactose)
# 
# p_lac = ggplot(milk_fit, aes(x = F, y = K)) +
#   geom_point(color = "grey30", size = 2) +
#   geom_abline(intercept = mean(post_fat$a), slope = mean(post_fat$bF), color = "steelblue", size = 1) +
#   labs(x = "Gordura (z-score)", y = "Energia (z-score)") +
#   theme_minimal() +
#   ggtitle("Modelo com Gordura")
# 
# p_fat = ggplot(milk_fit, aes(x = L, y = K)) +
#   geom_point(color = "grey30", size = 2) +
#   geom_abline(intercept = mean(post_lactose$a), slope = mean(post_lactose$bL), color = "steelblue", size = 1) +
#   labs(x = "Lactose (z-score)", y = "Energia (z-score)") +
#   theme_minimal() +
#   ggtitle("Modelo com Lactose")
# 
# p_lac + p_fat


## ----milk_joint_model, eval=FALSE, results='hide'-------------------------------------------------------------------
# fit_joint <- ulam(
#   alist(
#     K ~ normal(mu, sigma),
#     mu <- a + bF * F + bL * L,
#     a ~ normal(0, 0.2),
#     bF ~ normal(0, 0.5),
#     bL ~ normal(0, 0.5),
#     sigma ~ exponential(1)
#   ),
#   data = milk_fit, chains = 4, cores = 4, iter = 2000, refresh = 0,
#   control = list(adapt_delta = 0.99), log_lik = TRUE
# )


## ----milk_joint_summary, eval=FALSE---------------------------------------------------------------------------------
# precis(fit_joint)


## ----milk_slope_comparison, eval=FALSE------------------------------------------------------------------------------
# post_fat <- rethinking::extract.samples(fit_fat)
# post_lactose <- rethinking::extract.samples(fit_lactose)
# post_joint <- rethinking::extract.samples(fit_joint)
# 
# slope_draws <- cbind(
#   "Gordura" = as.numeric(post_fat$bF),
#   "Gordura (+lactose)" = as.numeric(post_joint$bF),
#   "Lactose" = as.numeric(post_lactose$bL),
#   "Lactose (+gordura)" = as.numeric(post_joint$bL)
# )
# bayesplot::mcmc_intervals(slope_draws, prob = 0.5, prob_outer = 0.95) +
#   labs(x = "Coeficiente padronizado", y = NULL) +
#   theme_minimal()


## ----milk_joint_posterior_pairs, eval=FALSE-------------------------------------------------------------------------
# posterior_slope_correlation <- cor(as.numeric(post_joint$bF), as.numeric(post_joint$bL))
# posterior_slope_correlation
# 
# joint_slopes <- data.frame(
#   bF = as.numeric(post_joint$bF),
#   bL = as.numeric(post_joint$bL)
# )
# 
# pairs(joint_slopes, pch = 16, cex = 0.4,
#       col = grDevices::adjustcolor("steelblue", alpha.f = 0.2),
#       labels = c("bF, efeito da gordura", "bL, efeito da lactose"))
# 


## ----milk_pca_fit, eval=FALSE---------------------------------------------------------------------------------------
# # Podemos extrair a primeira componente e ajustar o modelo
# X <- as.matrix(milk_fit[c("F", "L")])
# pca_milk <- prcomp(X, center = FALSE, scale. = FALSE)
# milk_pca_fit <- milk_fit
# milk_pca_fit$PC1 <- as.numeric(pca_milk$x[, 1])
# 
# fit_pc1 <- ulam(
#   alist(
#     K ~ normal(mu, sigma),
#     mu <- a + bPC1 * PC1,
#     a ~ normal(0, 0.2),
#     bPC1 ~ normal(0, 0.5),
#     sigma ~ exponential(1)
#   ),
#   data = milk_pca_fit, chains = 4, cores = 4, iter = 2000, refresh = 0
# )
# 
# 
# precis(fit_pc1)
# 


## ----milk_posterior_predictive_comparison, eval=FALSE---------------------------------------------------------------
# set.seed(123)
# 
# yrep_joint <- rethinking::sim(fit_joint, data = milk_fit)
# yrep_pc1 <- rethinking::sim(fit_pc1, data = milk_pca_fit)
# 
# summarize_predictions <- function(draws) {
#   data.frame(
#     lower = apply(draws, 2, quantile, probs = 0.25),
#     mean = apply(draws, 2, mean),
#     upper = apply(draws, 2, quantile, probs = 0.75)
#   )
# }
# 
# joint_predictions <- summarize_predictions(yrep_joint)
# pc1_predictions <- summarize_predictions(yrep_pc1)
# comparison_predictions <- data.frame(
#   joint_lower = joint_predictions$lower,
#   joint_mean = joint_predictions$mean,
#   joint_upper = joint_predictions$upper,
#   pc1_lower = pc1_predictions$lower,
#   pc1_mean = pc1_predictions$mean,
#   pc1_upper = pc1_predictions$upper
# )
# 
# ggplot(comparison_predictions, aes(x = joint_mean, y = pc1_mean)) +
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey55") +
#   geom_segment(aes(x = joint_lower, xend = joint_upper,
#                    y = pc1_mean, yend = pc1_mean),
#                color = "steelblue", alpha = 0.5) +
#   geom_segment(aes(x = joint_mean, xend = joint_mean,
#                    y = pc1_lower, yend = pc1_upper),
#                color = "steelblue", alpha = 0.5) +
#   geom_point(size = 2) +
#   coord_equal() +
#   labs(title = "Comparação das previsões posteriores",
#        subtitle = "Intervalos preditivos centrais de 50%",
#        x = "Modelo conjunto: gordura + lactose",
#        y = "Modelo PCA: primeiro componente") +
#   theme_minimal()

