## Dados em escala log, centrados na média
dat <- with(trees, data.frame(
    lV = log(Volume) - mean(log(Volume)),
    lG = log(Girth)  - mean(log(Girth)),
    lH = log(Height) - mean(log(Height))))

## As preditoras são correlacionadas
cor(dat$lG, dat$lH)

## Regressão simples: só a altura
m.simples <- lm(lV ~ lH, data = dat)

## Regressão múltipla: diâmetro e altura
m.multipla <- lm(lV ~ lG + lH, data = dat)

summary(m.simples)
summary(m.multipla)
confint(m.simples)
confint(m.multipla)

## ---------------------------------------------------------------
## "Descontando" o diâmetro: regressões nos resíduos
## ---------------------------------------------------------------

## Altura em função do diâmetro
m.HG <- lm(lH ~ lG, data = dat)

## Volume em função do diâmetro
m.VG <- lm(lV ~ lG, data = dat)

## Resíduos: o que sobra de cada variável depois de descontar o diâmetro
dat$res.H <- resid(m.HG)
dat$res.V <- resid(m.VG)

## Regressão dos resíduos: a inclinação é igual à de lH na múltipla
m.res <- lm(res.V ~ res.H, data = dat)

## Comparação direta
c(simples  = unname(coef(m.simples)["lH"]),
  multipla = unname(coef(m.multipla)["lH"]),
  residuos = unname(coef(m.res)["res.H"]))

## Gráfico de variável adicionada (added-variable plot)
plot(res.V ~ res.H, data = dat, pch = 19, col = "blue",
     xlab = "log(altura) | log(diâmetro)",
     ylab = "log(volume) | log(diâmetro)")
abline(m.res, lwd = 2)
