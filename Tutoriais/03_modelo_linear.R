## ----setup, echo=FALSE----------------------------------------------------------------------------------------------
library(rmarkdown)
knitr::opts_chunk$set(eval = TRUE)
# clipboard
htmltools::tagList(
  xaringanExtra::use_clipboard(
    button_text = "Copy code <i class=\"fa fa-clipboard\"></i>",
    success_text = "Copied! <i class=\"fa fa-check\" style=\"color: #90BE6D\"></i>",
    error_text = "Not copied 😕 <i class=\"fa fa-times-circle\" style=\"color: #F94144\"></i>"
  ),
  rmarkdown::html_dependency_font_awesome()
  )


## ----create_dataset-------------------------------------------------------------------------------------------------
N = 30
x <- runif(N, 0, 5)
n <- length(x)
mu <- 1.2 + 3.5 * x # α = 1.2 e β = 3.5.
y <- rnorm(N, mean = mu, sd = 3)
lm_data <- data.frame(x,  mu, y, res = y - mu)


## ----plot, fig.asp=1------------------------------------------------------------------------------------------------
plot(y ~ x, pch = 19)


## ----beta0 and beta1, results='hide'--------------------------------------------------------------------------------
beta_hat <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x)) ^ 2)
beta_hat

# Esta é apenas a razão entre a covariância de x e y e a variância de x
cov(x, y) / var(x)

# Como mu = alpha + beta*x:
alpha_hat <- mean(y) - beta_hat * mean(x)
alpha_hat


## ----OLS, fig.asp=1-------------------------------------------------------------------------------------------------
plot(y ~ x, pch = 19)
abline(a = alpha_hat, b = beta_hat, col = 2, lwd = 2)


## ----results='hide'-------------------------------------------------------------------------------------------------
m1 = lm(y ~ x)
m1


## ----results='hide'-------------------------------------------------------------------------------------------------
summary(m1)


## ----extract_residuals----------------------------------------------------------------------------------------------
# Extraindo os valores ajustados (a porção de y explicada por x)
mu_hat <- fitted(m1)

# Extraindo os resíduos (a variação residual não explicada)
res <- residuals(m1)



## ----plot_res_distance, fig.asp=1-----------------------------------------------------------------------------------
# 1. Visualizando os resíduos como distância da reta
plot(y ~ x, pch = 19, main = "Perspectiva 1: Distância da reta de regressão")
abline(m1, col = "red", lwd = 2)

# Desenhando linhas conectando os pontos reais aos valores preditos
segments(x0 = x, y0 = y, x1 = x, y1 = mu_hat, col = "blue", lty = 2)



## ----plot_res_variable, fig.asp=1-----------------------------------------------------------------------------------
# 2. Visualizando os resíduos como uma variável independente
plot(res ~ x, pch = 19, col = "blue", 
     main = "Perspectiva 2: A variação residual como variável",
     ylab = "Variação residual não explicada")
abline(h = 0, col = "red", lwd = 2, lty = 2)



## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
# 1. Criar a variável preditora X (Categórica)
n <- 100
estado_nutricional <- sample(c("bem_alimentado", "mal_alimentado"), n, replace = TRUE)
peso_medio_por_classe = c("bem_alimentado" = 75, "mal_alimentado" = 60)
peso <- peso_medio_por_classe[estado_nutricional] + rnorm(n, mean = 0, sd = 5)

# Criar o dataframe
dados <- data.frame(peso, estado_nutricional)
head(dados)


## ----results='hide'-------------------------------------------------------------------------------------------------
m2 = lm(peso ~ estado_nutricional)
summary(m2)


## ----message = FALSE, warning = FALSE-------------------------------------------------------------------------------
# install.packages( "ggplot2") # se necessario...
library(ggplot2)

ggplot(dados, aes(estado_nutricional, peso, color = estado_nutricional)) + geom_boxplot() + geom_jitter(width = 0.1)  


## -------------------------------------------------------------------------------------------------------------------
set.seed(1)
n = 100
# Duas preditoras, x e z
x = rnorm(n)
z = rnorm(n)

# y é função de x e z.
a = 1; b_x = 1.35; b_z = 3.5
y = a + b_x * x + b_z * z + rnorm(n)


## ----results='hide'-------------------------------------------------------------------------------------------------
lm(y ~ x) |> coefficients() |> round(2)
lm(y ~ z) |> coefficients() |> round(2)


## ----results='hide'-------------------------------------------------------------------------------------------------
lm(y ~ x + z) |> coefficients() |> round(2)


## -------------------------------------------------------------------------------------------------------------------
set.seed(2)
n = 100
u = rnorm(n)

# X e Z são funções de u?
x = 1 + 0.5 * u + rnorm(n)
z = 3 + 2   * u + rnorm(n)

# y é função de x e z.
a = 1; b_x = 1.35; b_z = 3.5
y = a + b_x * x + b_z * z + rnorm(n)


## ----results='hide'-------------------------------------------------------------------------------------------------
lm(y ~ x) |> coefficients() |> round(2)
lm(y ~ z) |> coefficients() |> round(2)


## ----results='hide'-------------------------------------------------------------------------------------------------
lm(y ~ x + z) |> coefficients() |> round(2)


## ----results='hide'-------------------------------------------------------------------------------------------------
# 1. Regredimos a variável X contra a variável Z
m_xz <- lm(x ~ z)

# 2. Extraímos os resíduos (a variação de X que não tem relação com Z)
res_x <- residuals(m_xz)

# 3. Regredimos Y contra esse X residual
lm(y ~ res_x) |> coefficients() |> round(2)



## ----results='hide'-------------------------------------------------------------------------------------------------
# Isolando Z: regredimos Z em função de X e extraímos os resíduos
res_z <- residuals(lm(z ~ x))

# Regredimos Y contra o Z residual
lm(y ~ res_z) |> coefficients() |> round(2)



## -------------------------------------------------------------------------------------------------------------------
ratones <- read.table("https://raw.githubusercontent.com/diogro/evofencom/refs/heads/main/Tutoriais/ratones.tsv", header = TRUE)


## ----results='hide'-------------------------------------------------------------------------------------------------
str(ratones)


## ----label = distances, echo = FALSE, fig.cap = "Vista ventral e dorsal de um crânio de roedor, com os pontos envolvidos no cálculo das distâncias de interesse circulados em vermelho", out.width = "100%"----
knitr::include_graphics("ratones_dists.png")


## ----warning=FALSE--------------------------------------------------------------------------------------------------
ggplot(ratones, aes(NSL_NA, IS_PNS, color = SEX, shape = line, group = line)) + 
  geom_point() + stat_ellipse() + scale_color_manual(values = 1:2) + theme_classic()


## ----results='hide'-------------------------------------------------------------------------------------------------
x = ratones$NSL_NA
y = ratones$IS_PNS
cor(x, y)


## -------------------------------------------------------------------------------------------------------------------
traits = names(ratones)[12:46]
f = paste0("cbind(", paste(traits, collapse = ","), ") ~ SEX + line") 

# Replicando o formato original dos dados.
ratones_res = ratones 

# Substituindo as medidas pelos resíduos de um modelo linear, incluindo sexo e linhagem.
ratones_res[, traits] = lm(as.formula(f), data = ratones) |> residuals()


## ----warning=FALSE--------------------------------------------------------------------------------------------------
ggplot(ratones_res, aes(NSL_NA, IS_PNS, color = SEX, shape = line, group = line)) + 
  geom_point() + stat_ellipse() + scale_color_manual(values = 1:2) + theme_classic()


## ----results='hide'-------------------------------------------------------------------------------------------------
x = ratones_res$NSL_NA
y = ratones_res$IS_PNS
cor(x, y)


## ----tulips_data----------------------------------------------------------------------------------------------------
tulips <- read.csv("tulips.csv")
tulips$blooms <- tulips$blooms / max(tulips$blooms)
tulips$water <- as.numeric(scale(tulips$water, scale = FALSE))
tulips$shade <- as.numeric(scale(tulips$shade, scale = FALSE))


## ----fit_interaction------------------------------------------------------------------------------------------------
m3 <- lm(blooms ~ water * shade, data = tulips)


## ----summarize_interaction, results='hide'--------------------------------------------------------------------------
summary(m3)


## ----visualize_interaction------------------------------------------------------------------------------------------
plot(blooms ~ water, col = 2:4, pch = 16, data = tulips)
for (s in unique(tulips$shade)) {
  abline(
    a = coef(m3)["(Intercept)"] + coef(m3)["shade"] * s,
    b = coef(m3)["water"] + coef(m3)["water:shade"] * s,
    col = match(s, sort(unique(tulips$shade))) + 1
  )
}
legend("topleft", legend = c("Sombra -1", "Sombra 0", "Sombra 1"), col = 2:4, pch = 16)

