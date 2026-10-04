library(rethinking)

## Dados em escala log, centrados na média
dat <- with(trees, data.frame(
    lV = log(Volume) - mean(log(Volume)),
    lG = log(Girth)  - mean(log(Girth)),
    lH = log(Height) - mean(log(Height))))

## Ajustes por máxima verossimilhança com quap().
## Os valores iniciais dos parâmetros são informados em 'start'.
## O desvio padrão é estimado em escala log (lsigma), para garantir sigma > 0.

## Regressão simples: só a altura
m.simples <- quap(
    alist(
        lV ~ dnorm(mu, exp(lsigma)),
        mu <- a + bH * lH
    ), data = dat,
    start = list(a = 0, bH = 0, lsigma = 0))

## Regressão múltipla: diâmetro e altura
m.multipla <- quap(
    alist(
        lV ~ dnorm(mu, exp(lsigma)),
        mu <- a + bG * lG + bH * lH
    ), data = dat,
    start = list(a = 0, bG = 0, bH = 0, lsigma = 0))

precis(m.simples)
precis(m.multipla)

## sigma na escala original
exp(coef(m.multipla)["lsigma"])

## Comparação gráfica dos coeficientes nos dois modelos
## Gráfico dos coeficientes (média e intervalo de 89%), só com os
## parâmetros que existem em cada modelo
plot_coefs <- function(modelos, pars) {
    tab <- do.call(rbind, lapply(names(modelos), function(nome) {
        p <- as.data.frame(precis(modelos[[nome]]))
        p <- p[rownames(p) %in% pars, , drop = FALSE]
        data.frame(par = rownames(p), modelo = nome, media = p$mean,
                   inf = p[["5.5%"]], sup = p[["94.5%"]])
    }))
    tab <- tab[order(match(tab$par, pars)), ]
    y <- nrow(tab):1
    op <- par(mar = c(5, 10, 1, 1))
    on.exit(par(op))
    plot(tab$media, y, xlim = range(tab$inf, tab$sup), yaxt = "n",
         pch = 19, xlab = "Valor estimado", ylab = "")
    segments(tab$inf, y, tab$sup, y, lwd = 2)
    axis(2, at = y, labels = paste(tab$par, "|", tab$modelo), las = 1)
}

plot_coefs(list("simples" = m.simples, "múltipla" = m.multipla),
           pars = c("bG", "bH"))

## ---------------------------------------------------------------
## "Descontando" o diâmetro: regressões nos resíduos
## ---------------------------------------------------------------

## Altura em função do diâmetro
m.HG <- quap(
    alist(
        lH ~ dnorm(mu, exp(lsigma)),
        mu <- a + b * lG
    ), data = dat,
    start = list(a = 0, b = 0, lsigma = 0))

## Volume em função do diâmetro
m.VG <- quap(
    alist(
        lV ~ dnorm(mu, exp(lsigma)),
        mu <- a + b * lG
    ), data = dat,
    start = list(a = 0, b = 0, lsigma = 0))

## Resíduos calculados com as estimativas de MV
cf.HG <- coef(m.HG)
cf.VG <- coef(m.VG)
dat$res.H <- dat$lH - (cf.HG["a"] + cf.HG["b"] * dat$lG)
dat$res.V <- dat$lV - (cf.VG["a"] + cf.VG["b"] * dat$lG)

## Regressão dos resíduos: a inclinação deve ser igual a bH da múltipla
m.res <- quap(
    alist(
        res.V ~ dnorm(mu, exp(lsigma)),
        mu <- a + bH * res.H
    ), data = dat,
    start = list(a = 0, bH = 0, lsigma = 0))

## Comparação direta
c(simples  = coef(m.simples)["bH"],
  multipla = coef(m.multipla)["bH"],
  residuos = coef(m.res)["bH"])

## Conferência com lm()
coef(lm(lV ~ lG + lH, data = dat))

## Gráfico de variável adicionada (added-variable plot)
plot(res.V ~ res.H, data = dat, pch = 19, col = rangi2,
     xlab = "log(altura) | log(diâmetro)",
     ylab = "log(volume) | log(diâmetro)")
cf <- coef(m.res)
abline(a = cf["a"], b = cf["bH"], lwd = 2)
