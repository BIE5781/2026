## Leitura dos dados do site https://www.randomservices.org/random/data/Galton.html
galton <- read.table("../06_modelos-gaussianos/Galton.txt", header = TRUE)

## Apenas meninos, e só o primeiro filho de cada família
meninos <- galton[galton$Gender == "M", ]
meninos <- meninos[!duplicated(meninos$Family), ]

## Preditoras centradas na média (alturas em polegadas)
dat <- with(meninos, data.frame(
    H = Height,
    P = Father - mean(Father),
    M = Mother - mean(Mother)))

## As alturas do pai e da mãe são pouco correlacionadas
cor(dat$P, dat$M)
plot(M ~ P, data = dat, pch = 19, col = "blue",
     xlab = "Altura do pai (centrada, in)",
     ylab = "Altura da mãe (centrada, in)")

## Regressões simples
m.pai <- lm(H ~ P, data = dat)
m.mae <- lm(H ~ M, data = dat)

## Regressão múltipla: pai e mãe
m.pais <- lm(H ~ P + M, data = dat)

summary(m.pai)
summary(m.mae)
summary(m.pais)

## Os coeficientes quase não mudam entre os modelos simples e o múltiplo
rbind(simples  = c(P = unname(coef(m.pai)["P"]), M = unname(coef(m.mae)["M"])),
      multipla = coef(m.pais)[c("P", "M")])

## Intervalos de confiança dos coeficientes
confint(m.pai)
confint(m.mae)
confint(m.pais)
