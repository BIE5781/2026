## ----setup, echo=FALSE----------------------------------------------------------------------------------------------
library(rmarkdown)
knitr::opts_chunk$set(eval = FALSE, fig.asp = 1)
# Área de transferência
htmltools::tagList(
  xaringanExtra::use_clipboard(
    button_text = "Copiar código <i class=\"fa fa-clipboard\"></i>",
    success_text = "Copiado! <i class=\"fa fa-check\" style=\"color: #90BE6D\"></i>",
    error_text = "Não copiado <i class=\"fa fa-times-circle\" style=\"color: #F94144\"></i>"
  ),
  rmarkdown::html_dependency_font_awesome()
)


## -------------------------------------------------------------------------------------------------------------------
set.seed(2)
# Dados:
y = rnorm(100, mean = 2, sd = 1)

# Log-posterior de mu, com sigma conhecido e fixado em 1
logP_mu_y = function(mu, data = y){
  ll = sum(dnorm(data, mean = mu, sd = 1, log = TRUE))
  logprior = dnorm(mu, mean = 0, sd = 1, log = TRUE)
  logprior + ll
}


## -------------------------------------------------------------------------------------------------------------------
x = seq(1, 3, 0.01)
log_posterior = Vectorize(logP_mu_y)(x)
posterior_density = exp(log_posterior - max(log_posterior))
posterior_density = posterior_density / sum(posterior_density * diff(x)[1])
plot(posterior_density ~ x, type = "l", lwd = 2, col = 2,
     xlab = "Média (mu)", ylab = "Densidade posterior")
abline(v = 2, lty = 2)


## -------------------------------------------------------------------------------------------------------------------
logP_mu_sigma_y = function(mu, sigma, data = y){
  ll = sum(dnorm(data, mean = mu, sd = sigma, log = TRUE))
  logprior_mu = dnorm(mu, mean = 0, sd = 1, log = TRUE)
  logprior_sigma = dexp(sigma, rate = 1, log = TRUE)
  ll + logprior_mu + logprior_sigma
}

mu_grid = seq(1, 3, length.out = 250)
sigma_grid = seq(0.1, 3, length.out = 250)
parameter_grid = expand.grid(mu = mu_grid, sigma = sigma_grid)
log_posterior_grid = mapply(
  logP_mu_sigma_y,
  mu = parameter_grid$mu,
  sigma = parameter_grid$sigma
)

posterior_density = exp(log_posterior_grid - max(log_posterior_grid))
posterior_density = posterior_density /
  sum(posterior_density * diff(mu_grid)[1] * diff(sigma_grid)[1])
parameter_grid$posterior_density = posterior_density

library(ggplot2)

ggplot(parameter_grid,
    aes(x = mu, y = sigma, fill = posterior_density)) +
  geom_raster() +
  coord_cartesian(xlim = c(1.5, 2.3), ylim = c(0.9, 1.4)) +
  scale_fill_viridis_c(name = "Densidade posterior", option = "A") +
  labs(x = "Média (mu)", y = "Desvio-padrão (sigma)")


## -------------------------------------------------------------------------------------------------------------------
curve(dnorm(x, mean = 0, sd = 1), -3, 3, lwd = 2, col = 2,
            ylab = "Densidade", xlab = "Valor do parâmetro")


## -------------------------------------------------------------------------------------------------------------------
curve(dexp(x, rate = 1), 0, 3, lwd = 2, col = 2,
            ylab = "Densidade", xlab = "Valor do parâmetro" )


## -------------------------------------------------------------------------------------------------------------------
N = 100 # tamanho da amostra

# Parâmetros da regressão linear
alpha = 1 # Intercepto
beta = 1  # Inclinação em relação a x
## Simula uma resposta normal cuja média é uma função linear de um preditor
x = rnorm(N)           # Variável preditora simulada: média = 0 e desvio-padrão = 1
mu = alpha + beta * x  # Modelo linear para o valor esperado
y = rnorm(N, mu)       # Variável resposta, com desvio-padrão residual igual a 1



## ----eval = FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------
# 
# library(rethinking)
# 
# ## Código do `rethinking`
# m1 = quap(alist(
#   y ~ dnorm(mu, sigma),     # Verossimilhança
#   mu <- a + b * x,          # Modelo linear
#   a ~ dnorm(0, 1),          # Distribuição a priori do intercepto
#   b ~ dnorm(0, 1),          # Distribuição a priori da inclinação
#   sigma ~ dexp(1)),         # Distribuição a priori do desvio-padrão residual
#   data = list(y = y, x = x) # Lista de dados
# )


## -------------------------------------------------------------------------------------------------------------------
precis(m1, digits = 2, prob = 0.93) # Resumo do ajuste


## ----create_dataset2------------------------------------------------------------------------------------------------
set.seed(4)

x = runif(50, 50, 100)
y = 1000 + 150 * x + rnorm(50, 0, 1000)
data = data.frame(x, y)


## ----plot2----------------------------------------------------------------------------------------------------------
plot(y ~ x, data = data)


## ----fit_ols--------------------------------------------------------------------------------------------------------
mod_ols <- lm(y ~ x, data = data)

# A função summary fornece várias informações sobre o modelo,
# incluindo estimativas, erros-padrão, valores-p para os coeficientes
# e o desvio-padrão residual (sigma).
summary(mod_ols)


## ----fit_bayesian, message=FALSE, eval=FALSE------------------------------------------------------------------------
# suppressPackageStartupMessages(library(rstanarm))
# mod_bayesian <- stan_glm(y ~ x, data = data)
# 
# summary(mod_bayesian)


## ----compare, eval=FALSE--------------------------------------------------------------------------------------------
# coef(mod_ols)
# coef(mod_bayesian)
# 
# library(ggplot2)
# 
# # Extrai o intercepto e a inclinação dos dois modelos
# ols_intercept <- coef(mod_ols)[1]
# ols_slope <- coef(mod_ols)[2]
# bayesian_intercept <- coef(mod_bayesian)[1]
# bayesian_slope <- coef(mod_bayesian)[2]
# 
# 
# ggplot(data, aes(x, y)) +
#   geom_point() +
#   geom_abline(intercept = ols_intercept, slope = ols_slope, color = "red", linetype = "solid") +
#   geom_abline(intercept = bayesian_intercept, slope = bayesian_slope, color = "blue", linetype = "dashed") +
#   theme_bw()
# 


## ----fit_rethinking, message=FALSE, eval=FALSE----------------------------------------------------------------------
# suppressPackageStartupMessages(library(rethinking))
# 
# mod_rethinking = quap(
#   alist(
#     y ~ dnorm(mu, sigma),
#     mu <- a + b * x,
#     a ~ dnorm(0, 1),
#     b ~ dnorm(0, 1),
#     sigma ~ dexp(1)
#   ),
#   data = data
# )
# precis(mod_rethinking)


## ----scale, eval=FALSE----------------------------------------------------------------------------------------------
# data$x_scaled = as.numeric(scale(data$x))
# data$y_scaled = as.numeric(scale(data$y))
# 
# mod_rethinking_scaled = quap(
#   alist(
#     y_scaled ~ dnorm(mu, sigma),
#     mu <- a + b * x_scaled,
#     a ~ dnorm(0, 1),
#     b ~ dnorm(0, 1),
#     sigma ~ dexp(1)
#   ),
#   data = data
# )
# mod_stanarm_scaled = stan_glm(y_scaled ~ x_scaled, data = data)
# mod_ols_scaled = lm(y_scaled ~ x_scaled, data = data)


## ----compare_scaled, eval=FALSE-------------------------------------------------------------------------------------
# coef(mod_ols_scaled)
# coef(mod_stanarm_scaled)
# coef(mod_rethinking_scaled)


## ----data, eval=TRUE------------------------------------------------------------------------------------------------
df <- read.csv("https://raw.githubusercontent.com/lmmx/crawley-r-statistics-book-notes/master/code/ipomopsis.csv")
names(df) <- c("Root", "Fruit", "Grazing")
df$Root <- as.numeric(scale(df$Root, center = TRUE, scale = TRUE))
df$Fruit <- as.numeric(scale(df$Fruit, center = TRUE, scale = TRUE))

# Plantas sem proteção são o controle; as protegidas por cerca recebem o tratamento:
df$Grazing0 <- ifelse(df$Grazing == "Ungrazed", 1, 0)


## ----fit_model------------------------------------------------------------------------------------------------------
# Ajusta o modelo usando quap
m1 <- quap(
  alist(
    Fruit ~ dnorm(mu, sigma),
    mu <- a + b * Grazing0 + c * Root,
    a ~ dnorm(0, 1),
    b ~ dnorm(0, 1),
    c ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = df
)


## ----summarize_model------------------------------------------------------------------------------------------------
precis(m1)

m1_coef <- precis(m1)[1]


## ----plot_predictions-----------------------------------------------------------------------------------------------
# Representa os dados observados
plot(df$Fruit ~ df$Root,
     col = ifelse(df$Grazing0 == 1, "red", "black"),
     pch = ifelse(df$Grazing0 == 1, 17, 16),
     xlab = "Diâmetro basal (escore z)",
     ylab = "Massa seca dos frutos (escore z)")

# Adiciona as retas previstas: as duas retas compartilham a inclinação c
# e diferem no intercepto pelo efeito do tratamento, b
abline(a = m1_coef["a", ], b = m1_coef["c", ], col = "black", lwd = 2)
abline(a = m1_coef["a", ] + m1_coef["b", ], b = m1_coef["c", ], col = "red", lwd = 2)

# Adiciona a legenda
legend("topleft", legend = c("Protegidas (cerca)", "Controle"), col = c("red", "black"),
       pch = c(17, 16), lty = 1, bty = "n")



## ----coefficient_effects--------------------------------------------------------------------------------------------
plot(precis(m1))


## ----binary_contrast------------------------------------------------------------------------------------------------
df$Grazing_contrast <- ifelse(df$Grazing0 == 1, 1, -1)


## ----heteroskedastic_model------------------------------------------------------------------------------------------
df$Grazing_i <- ifelse(df$Grazing0 == 1, 2, 1)

m_heteroskedastic <- quap(
  alist(
    Fruit ~ dnorm(mu, sigma[Grazing_i]),
    mu <- a[Grazing_i] + c * Root,
    a[Grazing_i] ~ dnorm(0, 1),
    c ~ dnorm(0, 1),
    sigma[Grazing_i] ~ dexp(1)
  ),
  data = df
)

precis(m_heteroskedastic, depth = 2)


## ----test_variance_difference---------------------------------------------------------------------------------------
posterior_samples <- extract.samples(m_heteroskedastic, n = 1000)
sigma1 <- posterior_samples$sigma[,1]
sigma2 <- posterior_samples$sigma[,2]
difference <- sigma1 - sigma2
hist(difference, breaks = 30, main = "Diferença entre desvios-padrão residuais", xlab = "sigma1 - sigma2")
abline(v = 0, col = "red", lwd = 2)
proportion <- mean(difference > 0)
proportion


## ----animals_visualize, eval=TRUE-----------------------------------------------------------------------------------
animals <- MASS::Animals
animals$log_body <- log(animals$body)
animals$log_brain <- log(animals$brain)
body_center <- mean(animals$log_body)
animals$log_body_c <- animals$log_body - body_center
large_dinosaurs <- c("Brachiosaurus", "Dipliodocus", "Triceratops")
dinosaur_labels <- c("Brachiosaurus", "Diplodocus", "Triceratops")
dinosaur_indices <- match(large_dinosaurs, rownames(animals))
animals_without_dinosaurs <- animals[-dinosaur_indices, ]
point_colors <- ifelse(seq_len(nrow(animals)) %in% dinosaur_indices,
                       "firebrick", "grey35")

plot(log_brain ~ log_body, data = animals, pch = 19, col = point_colors,
  xlab = "Log da massa corporal (kg)", ylab = "Log da massa cerebral (g)")
text(animals$log_body[dinosaur_indices], animals$log_brain[dinosaur_indices],
     labels = dinosaur_labels, pos = 2, col = "firebrick", cex = 0.8)


## ----animals_ulam_fits, eval=FALSE, message=FALSE, warning=FALSE, results='hide'------------------------------------
# suppressPackageStartupMessages(library(rethinking))
# 
# m_animals_all <- ulam(
#   alist(
#     log_brain ~ dnorm(mu, sigma),
#     mu <- a + b * log_body_c,
#     a ~ dnorm(0, 1),
#     b ~ dnorm(0, 1),
#     sigma ~ dexp(1)
#   ),
#   data = animals,
#   chains = 4, cores = 4, iter = 2000, refresh = 0
# )
# 
# m_animals_without_dinosaurs <- ulam(
#   alist(
#     log_brain ~ dnorm(mu, sigma),
#     mu <- a + b * log_body_c,
#     a ~ dnorm(0, 1),
#     b ~ dnorm(0, 1),
#     sigma ~ dexp(1)
#   ),
#   data = animals_without_dinosaurs,
#   chains = 4, cores = 4, iter = 2000, refresh = 0
# )
# 


## ----animals_ulam_summaries, eval=FALSE-----------------------------------------------------------------------------
# precis(m_animals_all, digits = 2)
# precis(m_animals_without_dinosaurs, digits = 2)


## ----animals_fit_comparison, eval=FALSE, fig.asp=1------------------------------------------------------------------
# 
# posterior_all <- extract.samples(m_animals_all)
# posterior_without_dinosaurs <- extract.samples(m_animals_without_dinosaurs)
# body_values <- seq(min(animals$log_body), max(animals$log_body), length.out = 100)
# body_values_c <- body_values - body_center
# point_colors <- ifelse(seq_len(nrow(animals)) %in% dinosaur_indices,
#                        "firebrick", "grey35")
# 
# plot(log_brain ~ log_body, data = animals, pch = 19, col = point_colors,
#   xlab = "Log da massa corporal (kg)", ylab = "Log da massa cerebral (g)")
# text(animals$log_body[dinosaur_indices], animals$log_brain[dinosaur_indices],
#      labels = dinosaur_labels, pos = 2, col = "firebrick", cex = 0.8)
# lines(body_values,
#       mean(posterior_all$a) + mean(posterior_all$b) * body_values_c,
#       col = "navy", lwd = 2)
# lines(body_values,
#       mean(posterior_without_dinosaurs$a) +
#         mean(posterior_without_dinosaurs$b) * body_values_c,
#       col = "darkorange3", lwd = 2, lty = 2)
# legend("topleft",
#        legend = c("Todas as espécies", "Sem os grandes dinossauros", "Grandes dinossauros"),
#        col = c("navy", "darkorange3", "firebrick"),
#        lty = c(1, 2, NA), pch = c(NA, NA, 19), bty = "n")


## ----student_t_tails, eval=TRUE, fig.width=7, fig.height=5----------------------------------------------------------
x <- seq(-8, 8, length.out = 1000)
curve_normal <- dnorm(x)
curve_t_low <- dt(x, df = 3)
curve_t_high <- dt(x, df = 10)
curve_colors <- c("gray25", "tomato3", "steelblue")

plot(x, curve_normal, type = "l", log = "y", ylim = c(1e-6, 1),
     col = curve_colors[1], lwd = 2,
     xlab = "Valor do resíduo", ylab = "Densidade (escala logarítmica)")
lines(x, curve_t_low, col = curve_colors[2], lwd = 2)
lines(x, curve_t_high, col = curve_colors[3], lwd = 2, lty = 2)
legend("topright",
       legend = c("Normal", "t de Student (nu = 3)", "t de Student (nu = 10)"),
       col = curve_colors, lwd = 2, lty = c(1, 1, 2), bty = "n")


## ----animals_student_fits, eval=FALSE, message=FALSE, warning=FALSE-------------------------------------------------
# 
# m_animals_student_all <- ulam(
#   alist(
#     log_brain ~ dstudent(nu, mu, sigma),
#     mu <- a + b * log_body_c,
#     nu <- 2 + exp(log_nu),
#     a ~ dnorm(0, 1),
#     b ~ dnorm(0, 1),
#     sigma ~ dexp(1),
#     log_nu ~ dnorm(1, 1)
#   ),
#   data = animals,
#   chains = 4, cores = 4, iter = 2000, refresh = 0
# )
# 
# m_animals_student_without_dinosaurs <- ulam(
#   alist(
#     log_brain ~ dstudent(nu, mu, sigma),
#     mu <- a + b * log_body_c,
#     nu <- 2 + exp(log_nu),
#     a ~ dnorm(0, 1),
#     b ~ dnorm(0, 1),
#     sigma ~ dexp(1),
#     log_nu ~ dnorm(1, 1)
#   ),
#   data = animals_without_dinosaurs,
#   chains = 4, cores = 4, iter = 2000, refresh = 0
# )
# 
# precis(m_animals_student_all, digits = 2)
# precis(m_animals_student_without_dinosaurs, digits = 2)


## ----animals_sensitivity, eval=FALSE--------------------------------------------------------------------------------
# set.seed(42)
# post_normal_all <- extract.samples(m_animals_all)
# post_normal_without <- extract.samples(m_animals_without_dinosaurs)
# post_student_all <- extract.samples(m_animals_student_all)
# post_student_without <- extract.samples(m_animals_student_without_dinosaurs)
# 
# slope_change_normal <- abs(mean(post_normal_all$b) - mean(post_normal_without$b))
# slope_change_student <- abs(mean(post_student_all$b) - mean(post_student_without$b))
# 
# data.frame(
#   residual_distribution = c("Normal", "Student-t"),
#   slope_all_cases = c(mean(post_normal_all$b), mean(post_student_all$b)),
#   slope_without_dinosaurs = c(mean(post_normal_without$b), mean(post_student_without$b)),
#   absolute_slope_change = c(slope_change_normal, slope_change_student)
# )


## ----animals_student_comparison, eval=FALSE, fig.asp=1--------------------------------------------------------------
# body_values <- seq(min(animals$log_body), max(animals$log_body), length.out = 100)
# body_values_c <- body_values - body_center
# point_colors <- ifelse(seq_len(nrow(animals)) %in% dinosaur_indices,
#                        "firebrick", "grey35")
# 
# plot(log_brain ~ log_body, data = animals, pch = 19, col = point_colors,
#      xlab = "Log da massa corporal (kg)", ylab = "Log da massa cerebral (g)")
# text(animals$log_body[dinosaur_indices], animals$log_brain[dinosaur_indices],
#   labels = dinosaur_labels, pos = 2, col = "firebrick", cex = 0.8)
# 
# lines(body_values,
#   mean(post_normal_all$a) + mean(post_normal_all$b) * body_values_c,
#       col = "navy", lwd = 2)
# lines(body_values,
#   mean(post_normal_without$a) + mean(post_normal_without$b) * body_values_c,
#       col = "navy", lwd = 2, lty = 2)
# lines(body_values,
#   mean(post_student_all$a) + mean(post_student_all$b) * body_values_c,
#       col = "darkorange3", lwd = 2)
# lines(body_values,
#   mean(post_student_without$a) + mean(post_student_without$b) * body_values_c,
#       col = "darkorange3", lwd = 2, lty = 2)
# 
# legend("topleft",
#   legend = c("Normal: todas as espécies", "Normal: sem os grandes dinossauros",
#     "t de Student: todas as espécies", "t de Student: sem os grandes dinossauros",
#     "Grandes dinossauros"),
#        col = c("navy", "navy", "darkorange3", "darkorange3", "firebrick"),
#        lty = c(1, 2, 1, 2, NA), pch = c(NA, NA, NA, NA, 19), bty = "n")

