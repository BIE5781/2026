
## -------------------------------------------------------------------------------------------------------------------
mu1 <- 2
sig1 <- 0.1
mult1 <- 1.5
valores <- mu1 + mult1*c(-sig1,sig1) 
dnorm(x = valores, mean = mu1, sd = sig1)


## -------------------------------------------------------------------------------------------------------------------
curve(dnorm(x, mean = mu1, sd = sig1),
      from = mu1 - 3*sig1,
      to = mu1 + 3*sig1, 
      xlab = "x", ylab = "densidade de probabilidade")


## -------------------------------------------------------------------------------------------------------------------
points(valores,dnorm(valores, mu1, sig1),
       col="red", pch=19, cex=1.5)
## Segmentos que marcam os valores nos exiso x e y
segments(x0 = valores[c(1,2,2)],
         y0=c( 0, 0, dnorm(valores[2], mu1, sig1)),
         x1 =  c(valores, mult1),
         y1= dnorm(valores[c(1,2,2)], mean = mu1, sd = sig1),
         col="red", lty=2)



## ----eval=TRUE------------------------------------------------------------------------------------------------------

area.norm <- function(from, to, mean, sd, cor="red", shade=1e4,...){
  len <- (to-from)*shade
  x <- seq(from,to,length=len)
  segments(x0 = x,
           x1 = x,
           y0 = rep(0,len),
           y1 = dnorm(x, mean = mean, sd = sd), col = cor)
}



## -------------------------------------------------------------------------------------------------------------------
     area.norm(1.85,2.15,mean=2,sd=0.1)


## -------------------------------------------------------------------------------------------------------------------
integrate(f = dnorm,
          lower = valores[1],
          upper = valores[2] ,
          mean = mu1, sd = sig1)


## -------------------------------------------------------------------------------------------------------------------
integrate(dnorm, lower = -Inf, upper = Inf, mean = mu1, sd = sig1)
integrate(dnorm, lower = Inf, upper = Inf) ## normal padronizada


## ----echo = FALSE, eval =TRUE, fig.width=9, fig.height=3------------------------------------------------------------
par(mfrow=c(1,3))
curve(dnorm(x,mean=2,sd=0.1),from=1.7, to=2.3, 
      xlab="",ylab="",axes="F")
area.norm(1.7,2.15,mean=2,sd=0.1,cor="black")
curve(dnorm(x,mean=2,sd=0.1),from=1.7, to=2.3, 
      xlab="",ylab="",axes="F")
area.norm(1.7,1.85,mean=2,sd=0.1,cor="black")
curve(dnorm(x,mean=2,sd=0.1),from=1.7, to=2.3, 
      xlab="",ylab="",axes="F")
area.norm(1.85,2.15,mean=2,sd=0.1,cor="black")
mtext(c("-","="),at=c(0.6,1.6),padj=1.1,cex=8)


## -------------------------------------------------------------------------------------------------------------------
    pnorm(valores[2], mean = mu1, sd = sig1) - pnorm(valores[1], mean = mu1, sd = sig1)


## -------------------------------------------------------------------------------------------------------------------
## Area da cauda esquerda a excluir
cauda.esq <- pnorm(valores[1], mu1, sig1)
## Area da cauda direita a excluir
cauda.dir <- pnorm(valores[2], mu1, sig1, lower.tail=F)
1 - (cauda.esq + cauda.dir)


## -------------------------------------------------------------------------------------------------------------------
## 1 - o dobro da cauda esquerda
1 - 2*cauda.esq
## 1 - o dobro da cauda direita
1 - 2*cauda.dir


## -------------------------------------------------------------------------------------------------------------------
library(MASS)
help(michelson)


## -------------------------------------------------------------------------------------------------------------------
## Intervalos das classes
mich.brk <- seq(600, 1175, by=75)
## Cria fator com a classe de cada medida
mich.count <- cut(michelson$Speed, breaks = mich.brk)


## -------------------------------------------------------------------------------------------------------------------
## Cria um vetor com o n de observacoes em cada classe
tb1 <- table(mich.count) |> as.vector()
## Volta os nomes das classes
names(tb1) <- names(table(mich.count))
barplot(tb1, space=0, xlab = "Velocidade (km/s - 299000)", ylab = "Frequência")


## -------------------------------------------------------------------------------------------------------------------
tb2 <- tb1/sum(tb1)
barplot(tb2, space = 0, xlab = "Velocidade (km/s - 299000)", ylab = "Frequencia relativa")


## -------------------------------------------------------------------------------------------------------------------
tb3 <- tb2/75
barplot(tb3, space = 0, xlab = "Velocidade (km/s - 299000)", ylab = "Densidade")


## -------------------------------------------------------------------------------------------------------------------
hist(michelson$Speed, breaks = mich.brk,
     prob = TRUE,
     xlab = "Velocidade (km/s - 299000)",
     ylab = "Densidade")


## -------------------------------------------------------------------------------------------------------------------
## Normal com mu = media amostral e sigma = sd amostral
dnorm(x, mean = mean(michelson$Speed), sd = sd(michelson$Speed)) |>
    curve(add = TRUE, col = "blue")


## -------------------------------------------------------------------------------------------------------------------
## Parametros
mu2 <- 14.4
sig2 <- 1
## Grafico da normal
dnorm(x, mean = mu2, sd = sig2) |> 
    curve(from = 8, to = 20, 
          xlab = "Largura (in)",
          ylab = "densidade de probabilidade")


## -------------------------------------------------------------------------------------------------------------------
(larg <- qnorm(mean = mu2, sd= sig2, p = 0.98))


## -------------------------------------------------------------------------------------------------------------------
area.norm(8, larg, mean = mu2, sd = sig2)


## -------------------------------------------------------------------------------------------------------------------
## Desvio padrao 50% maior
sig3 <- sig2*1.5

qnorm(mu2, sig3, p=0.98)


## ----code_folding=TRUE----------------------------------------------------------------------------------------------
## Cria a curva original de novo
dnorm(x, mu2, sig2) |>
    curve(from=8, to=20, 
          xlab="Largura (in)",ylab="densidade de probabilidade")
## Adiciona nova curva
dnorm(x, mu2, sig3) |>
    curve( add = TRUE, col = "blue")


## -------------------------------------------------------------------------------------------------------------------
## Defina aqui dois valores de mu e dois de sigma
mu <- c(0,1)
sig <- c(1,2)
## Isto define a amplitude do grafico
minimo <- qnorm(0.001, min(mu), max(sig))
maximo <- qnorm(0.999, max(mu), max(sig))
## traca as curvas    
dnorm(x, mu[1], sig[1])|> curve(from = minimo, to = maximo, col = 1)
dnorm(x, mu[1], sig[2]) |> curve(col = 2, add = TRUE)
dnorm(x, mu[2], sig[1]) |> curve(col = 3, add = TRUE)
dnorm(x, mu[2], sig[2]) |> curve(col = 4, add = TRUE)
legend("topright",
       legend=c(bquote(paste(mu==.(mu[1])," , ",sigma==.(sig[1]))),
                bquote(paste(mu==.(mu[1])," , ",sigma==.(sig[2]))),
                bquote(paste(mu==.(mu[2])," , ",sigma==.(sig[1]))),
                bquote(paste(mu==.(mu[2])," , ",sigma==.(sig[2])))),
       lty = 1, col = 1:4, bty="n")


## -------------------------------------------------------------------------------------------------------------------
notas <- sample(0:10, size = 60, replace = TRUE)


## -------------------------------------------------------------------------------------------------------------------
mean(notas)


## -------------------------------------------------------------------------------------------------------------------
hist(notas)


## -------------------------------------------------------------------------------------------------------------------
## vetor para guardar as medias
medias <- c()
## O bom e velho loop
for(i in 1:1000)
    medias[i] <- sample(0:10, size = 60, replace = TRUE) |> mean() 


## -------------------------------------------------------------------------------------------------------------------
hist(medias)


## -------------------------------------------------------------------------------------------------------------------
abline(v = mean(medias), col="blue")
abline(v = 5, col="red")


## ----code_folding=TRUE----------------------------------------------------------------------------------------------
## Variancia teorica da uniforme
v.unif <- ((10-0+1)^2-1)/12
## Variancia esperada pelo TCL 
v.TCL <- v.unif/60
## Diferenca entre a variancia TCL e a das simulações
var(medias) - v.norm


## ----dados, eval=TRUE, echo=FALSE-----------------------------------------------------------------------------------
if(!require(tidyverse)) {install.packages(tidyverse); library(tidyverse)}
tab1 <-
    read.csv("https://raw.githubusercontent.com/BIE5781/ShinyApps/refs/heads/main/OLS/Stigler_99_tab_17_1.csv") |>
    mutate(
        deg = as.numeric(str_extract(Midpoint, "^\\d+")),
        min = as.numeric(str_extract(Midpoint, "(?<=°|◦)\\s*\\d+")),
        sec = as.numeric(str_extract(Midpoint, "(?<=′|'|´)\\s*\\d+")),
        decimal_deg = deg + (min / 60) + (sec / 3600),
        Radians = decimal_deg * (pi / 180)) |>
    select(-deg, -min, -sec, -decimal_deg) |>
    mutate(y = Modules/Degrees, x = (sin(Radians))^2)

tab1 |>
    select(X, Modules, Degrees, Midpoint, x, y) |>
    kable(col.names = c("Arco", "Comprimento (módulos)", "Amplitude (graus)",
                        "Latitude do ponto médio", "x", "y"),
          digits = c(0, 2, 5, 0, 4, 1),
          format.args = list(big.mark = ".", decimal.mark = ","))


## ----echo = TRUE, eval = FALSE--------------------------------------------------------------------------------------
# if(!require(shiny)){install.packages("shiny"); library(shiny)}
# runGitHub("ShinyApps", "BIE5781", subdir = "OLS")

