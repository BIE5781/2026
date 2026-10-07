## ----eval = TRUE----------------------------------------------------------------------------------------------------
library(sads)
library(bbmle)


## ----eval=TRUE------------------------------------------------------------------------------------------------------
## todos os conjuntos de dados compilados por Ohyama et al
raw <- read.csv("https://github.com/piLaboratory/bie5781/raw/master/data/jbi14149-sup-0003-appendixs3.csv")
## Seleciona dados do estudo de Suarez et al
suarez <- raw[raw$Study_ID=="23", c("Island_area", "Species.Richness")]


## ----sxa plot,  eval=TRUE-------------------------------------------------------------------------------------------
plot(Species.Richness ~ Island_area, data= suarez,
     xlab = "Área do fragmento (m2)", ylab = "Número de espécies", log="xy")


## -------------------------------------------------------------------------------------------------------------------
log(1e-12)


## ----log-lik linear, eval = TRUE------------------------------------------------------------------------------------
LL1 <- function(beta0, beta1, sigma){
    mu <- beta0 + beta1*log(suarez$Island_area)
    -sum( dnorm(x = log(suarez$Species.Richness), mean = mu, sd = sigma, log=TRUE) )
}

## -------------------------------------------------------------------------------------------------------------------
LL1(beta0 = 1, beta1 = 1, sigma =1)


## ----eval = TRUE----------------------------------------------------------------------------------------------------
ml1 <- mle2(LL1, start = list(beta0 = 0, beta1 = 0.1, sigma = 0.5))


## Parametrizacao alternativa com log(sigma)
LL1 <- function(beta0, beta1, lsigma){
    mu <- beta0 + beta1*log(suarez$Island_area)
    -sum( dnorm(x = log(suarez$Species.Richness), mean = mu, sd = exp(lsigma), log=TRUE) )
    
}

ml1 <- mle2(LL1, start = list(beta0 = 0, beta1 = 0.1, lsigma = log(0.5)))

## -------------------------------------------------------------------------------------------------------------------
summary(ml1)


## -------------------------------------------------------------------------------------------------------------------
(ml1.cf <- coef(ml1))


## -------------------------------------------------------------------------------------------------------------------
plot(log(Species.Richness) ~ log(Island_area), data= suarez,
     xlab = "ln área do fragmento (m2)", ylab = "ln Número de espécies")
abline(coef = ml1.cf, col ="blue")


## -------------------------------------------------------------------------------------------------------------------
if(!require(sads)){install.packages("sads"); library(sads)}
ml1.p <- profile(ml1, alpha =0.05) ## calcula o perfil
## Uma janela gráfica para 3 gráficos
par(mfrow = c(1,3))
## Os gráficos
plotprofmle(ml1.p, ylim = c(0,3))
## volta a janela para 1 gráfico
par(mfrow=c(1,1))


## -------------------------------------------------------------------------------------------------------------------
likelregions(ml1.p)


## -------------------------------------------------------------------------------------------------------------------
## Ajusta o mesmo modelo por MQ
ls1 <- lm(log(Species.Richness) ~ log(Island_area), data= suarez)


## -------------------------------------------------------------------------------------------------------------------
(ls1.cf <- coef(ls1)) # MQ
ml1.cf[1:2]


## -------------------------------------------------------------------------------------------------------------------
confint(ls1)
likelregions(ml1.p)


## -------------------------------------------------------------------------------------------------------------------
## valores previstos:
ml1.yhat <- ml1.cf[1] + ml1.cf[2]*log(suarez$Island_area)
## resíduos
ml1.res <- log(suarez$Species.Richness) - ml1.yhat
## Raiz da média dos quadrados dos resíduos
sum (ml1.res^2) / nrow(suarez) 
## compare com a MLE
ml1.cf[3]^2


## -------------------------------------------------------------------------------------------------------------------
## valores previstos:
ls1.yhat <- ls1.cf[1] + ls1.cf[2]*log(suarez$Island_area)
## resíduos
ls1.res <- log(suarez$Species.Richness) - ls1.yhat
## Raiz da soma dos quadrados dos resíduos dividida por n-2
sum (ls1.res^2) / (nrow(suarez)-2) 
## compare com o valor estimado pelo ajuste
summary(ls1)$sigma^2


## -------------------------------------------------------------------------------------------------------------------
sd.mle = function(x)  sqrt( (1/length(x)) * sum( (x - mean(x))^2 ) )


## -------------------------------------------------------------------------------------------------------------------
set.seed(42)                                        # sementes de n aleatórios
samp10 = sort(rep(1:1000, 10))                      # identificador das 1000 amostras de tamanho 10
vals10 = rnorm(1000*10, mean=100, sd=5)             # valores das observações
dens10 = density( tapply(vals10, samp10, sd.mle) )  # obtendo a MLE para cada amostra e a densidade empírica
dens10$y = dens10$y/ max(dens10$y)                  # convertendo o valor de densidade para valor relativo ao máximo
plot( dens10, type="l", xlab="Valor estimado",
        ylab="Densidade" ,main="")                  # gráfico da distribuição das MLE
abline(v = 5, col="red", lwd=2 )                    # posição do valor do parâmetro no gráfico


## -------------------------------------------------------------------------------------------------------------------
samp100 = sort(rep(1:1000, 100))
vals100 = rnorm(1000*100, mean=100, sd=5)
dens100 = density( tapply(vals100, samp100, sd.mle) )
dens100$y = dens100$y/ max(dens100$y)
lines( dens100, col="blue" )


## -------------------------------------------------------------------------------------------------------------------
## Amostras de tamanho 10
var( tapply(vals10, samp10, sd.mle) )
var( tapply(vals10, samp10, sd) )
## Amostras de tamanho 100
var( tapply(vals100, samp100, sd.mle) )
var( tapply(vals100, samp100, sd) )


## -------------------------------------------------------------------------------------------------------------------
par(mfrow=c(2,1))
qqnorm( tapply(vals10, samp10, sd.mle),
          main="MLE do desvio-padrão, N=10" )
qqline( tapply(vals10, samp10, sd.mle) )
qqnorm( tapply(vals100, samp100, sd.mle),
          main="MLE do desvio-padrão, N=100")
qqline( tapply(vals100, samp100, sd.mle) )
par(mfrow=c(1,1))


## -------------------------------------------------------------------------------------------------------------------
var.mle = function(x) mean( (x - mean(x))^2 )   # MLE da variância
x1 = vals10[samp10 == 1]                        # primeira amostra de tamanho 10
sqrt( var.mle(x1) )                             # raiz da MLE da variância
sd.mle(x1)                                      # MLE do desvio-padrão


## -------------------------------------------------------------------------------------------------------------------
## Log-verossimilhança negativa com o desvio-padrão como parâmetro
LL.sd = function(mu, sigma) -sum( dnorm(x1, mean = mu, sd = sigma, log = TRUE) )
## Log-verossimilhança negativa com a variância como parâmetro
LL.var = function(mu, v) -sum( dnorm(x1, mean = mu, sd = sqrt(v), log = TRUE) )

m.sd  = mle2(LL.sd,  start = list(mu = 100, sigma = 5),
             method = "L-BFGS-B", lower = c(mu = -Inf, sigma = 1e-6))
m.var = mle2(LL.var, start = list(mu = 100, v = 25),
             method = "L-BFGS-B", lower = c(mu = -Inf, v = 1e-6))

coef(m.sd)["sigma"]       # MLE do desvio-padrão
sqrt( coef(m.var)["v"] )  # raiz da MLE da variância


## -------------------------------------------------------------------------------------------------------------------
var.trad10 = tapply( vals10, samp10, var)    # estimativas com correção de viés da variância de amostras de tamanho 10
plot( density(var.trad10), type="l" )        # distribuição das estimativas
abline( v = mean(var.trad10) )               # média das estimativas
abline( v = 5^2, col="red" )                 # valor do parâmetro


## -------------------------------------------------------------------------------------------------------------------
sd.trad10 =  tapply(vals10, samp10, sd )
plot(density(sd.trad10), type="l")
abline( v = mean(sd.trad10) )
abline( v = 5, col="red" )


## -------------------------------------------------------------------------------------------------------------------
install.packages(c("coda", "mvtnorm", "devtools", "loo", "dagitty", "shape"))
devtools::install_github("rmcelreath/rethinking")


## -------------------------------------------------------------------------------------------------------------------
library(rethinking)
dados <- data.frame(lnS = log(suarez$Species.Richness),
                    lnA = log(suarez$Island_area))


## -------------------------------------------------------------------------------------------------------------------
modelo <- alist(
    lnS ~ dnorm(mu, sigma),   # ln S_i ~ Normal(mu_i, sigma)
    mu <- b0 + b1 * lnA       # mu_i = beta_0 + beta_1 ln A_i
)


## -------------------------------------------------------------------------------------------------------------------
ml1.r <- quap(modelo, data = dados,
              start = list(b0 = 0, b1 = 0.1, sigma = 0.5))


## -------------------------------------------------------------------------------------------------------------------
precis(ml1.r, prob = 0.95)


## -------------------------------------------------------------------------------------------------------------------
coef(ml1.r)
ml1.cf
ls1.cf


## -------------------------------------------------------------------------------------------------------------------
precis(ml1.r, prob = 0.95)
likelregions(ml1.p)


## -------------------------------------------------------------------------------------------------------------------
modelo.log <- alist(
    lnS ~ dnorm(mu, exp(log_sigma)),
    mu <- b0 + b1 * lnA
)
ml1.r2 <- quap(modelo.log, data = dados,
               start = list(b0 = 0, b1 = 0.1, log_sigma = log(0.5)))
exp(coef(ml1.r2)["log_sigma"])

