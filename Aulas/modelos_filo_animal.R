## Ajusta os modelos da aula "Modelos filogenéticos e modelo animal" e guarda
## os resultados em Aulas/cache/, um arquivo .rds por seção da aula:
##
##   cache/filo_primatas.rds   regressões tamanho do cérebro ~ tamanho do grupo
##   cache/animal_esquilos.rds modelo animal para o tamanho dos esquilos
##
## Uso (a partir da pasta Aulas/):
##   Rscript modelos_filo_animal.R          # ajusta as duas seções
##   Rscript modelos_filo_animal.R filo     # só os modelos filogenéticos
##   Rscript modelos_filo_animal.R animal   # só o modelo animal
##
## A aula (Aula_modelos_filogeneticos_animal.Rmd) lê os blocos rotulados
## deste arquivo com knitr::read_chunk(). Assim, o código mostrado nos
## slides é exatamente o código que foi ajustado aqui. A aula não ajusta
## nenhum modelo: ela só lê os arquivos em cache/.

secoes <- commandArgs(trailingOnly = TRUE)
if (length(secoes) == 0) secoes <- c("filo", "animal")
stopifnot(all(secoes %in% c("filo", "animal")),
          file.exists("../Tutoriais/squirels2.txt"))
dir.create("cache", showWarnings = FALSE)

## ---- pacotes ----
library(rethinking)
library(ape)

## Guarda amostras das posteriores, precis e diagnósticos de uma lista de modelos
resume_modelos <- function(modelos, descartar = character()) {
  list(
    post = lapply(modelos, function(m) {
      p <- extract.samples(m)
      p[setdiff(names(p), descartar)]
    }),
    precis = lapply(modelos, function(m) precis(m, depth = 2, prob = 0.95)),
    diag = sapply(modelos, function(m) {
      pr <- precis(m, depth = 2)
      ## (não usamos rethinking::divergent(): com o cmdstanr atual ela soma a
      ## coluna errada dos diagnósticos do amostrador)
      diag_amostrador <- attr(m, "cstanfit")$sampler_diagnostics(format = "draws_df")
      c(divergencias = sum(diag_amostrador$divergent__),
        rhat_max = max(pr$rhat, na.rm = TRUE),
        ess_min = min(pr$ess_bulk, na.rm = TRUE))
    }),
    data = Sys.time(),
    versoes = c(R = R.version.string,
                rethinking = as.character(packageVersion("rethinking")),
                cmdstan = as.character(cmdstanr::cmdstan_version()))
  )
}

## Acrescenta ao precis uma linha com uma quantidade derivada da posterior
adiciona_derivada <- function(tab, nome, x) {
  tab <- as.data.frame(tab)
  linha <- tab[1, ]
  linha[] <- NA
  linha[c("mean", "sd", "2.5%", "97.5%")] <-
    list(mean(x), sd(x), unname(PI(x, 0.95)[1]), unname(PI(x, 0.95)[2]))
  rownames(linha) <- nome
  rbind(tab, linha)
}

set.seed(42)

## ===================================================================
## Seção 1: tamanho do cérebro e tamanho do grupo em primatas
## (McElreath 2020, Statistical Rethinking 2ª ed., seção 14.5.2)
## ===================================================================
if ("filo" %in% secoes) {

## ---- dados-primatas ----
data(Primates301)
data(Primates301_nex)
d <- Primates301
d$name <- as.character(d$name)
dstan <- d[complete.cases(d$group_size, d$body, d$brain), ]
spp_obs <- dstan$name

dat_list <- list(
  N_spp = nrow(dstan),
  M = standardize(log(dstan$body)),       # massa corporal
  B = standardize(log(dstan$brain)),      # tamanho do cérebro
  G = standardize(log(dstan$group_size)), # tamanho do grupo
  Imat = diag(nrow(dstan))
)

## ---- m-reg ----
m_reg <- ulam(
  alist(
    B ~ normal(mu, sigma),
    mu <- a + bM * M + bG * G,
    a ~ normal(0, 1),
    c(bM, bG) ~ normal(0, 0.5),
    sigma ~ exponential(1)
  ), data = dat_list, chains = 4, cores = 4)

## ---- m-mvn ----
m_mvn <- ulam(
  alist(
    B ~ multi_normal(mu, SIGMA),
    mu <- a + bM * M + bG * G,
    matrix[N_spp, N_spp]: SIGMA <- Imat * sigma_sq,
    a ~ normal(0, 1),
    c(bM, bG) ~ normal(0, 0.5),
    sigma_sq ~ exponential(1)
  ), data = dat_list, chains = 4, cores = 4)

## ---- matriz-filo ----
arvore <- keep.tip(Primates301_nex, spp_obs)
## covariâncias sob movimento browniano
V <- vcv(corBrownian(phy = arvore))
## na mesma ordem dos dados
V <- V[spp_obs, spp_obs]
## matriz de correlação
dat_list$R <- V / max(V)

## ---- m-bm ----
m_bm <- ulam(
  alist(
    B ~ multi_normal(mu, SIGMA),
    mu <- a + bM * M + bG * G,
    matrix[N_spp, N_spp]: SIGMA <- R * sigma_sq,
    a ~ normal(0, 1),
    c(bM, bG) ~ normal(0, 0.5),
    sigma_sq ~ exponential(1)
  ), data = dat_list, chains = 4, cores = 4)

## ---- m-lambda ----
m_lambda <- ulam(
  alist(
    B ~ multi_normal(mu, SIGMA),
    mu <- a + bM * M + bG * G,
    matrix[N_spp, N_spp]: SIGMA <-
      R * sigma_F_sq + Imat * sigma_E_sq,
    a ~ normal(0, 1),
    c(bM, bG) ~ normal(0, 0.5),
    c(sigma_F_sq, sigma_E_sq) ~ exponential(1)
  ), data = dat_list, chains = 4, cores = 4)

## ---- lambda ----
post_lambda <- extract.samples(m_lambda)
lambda <- with(post_lambda,
  sigma_F_sq / (sigma_F_sq + sigma_E_sq))

## ---- fim-primatas ----
res_filo <- resume_modelos(list(m_reg = m_reg, m_mvn = m_mvn,
                                m_bm = m_bm, m_lambda = m_lambda))
res_filo$post$m_lambda$lambda <- lambda
res_filo$precis$m_lambda <- adiciona_derivada(res_filo$precis$m_lambda,
                                              "lambda", lambda)
saveRDS(res_filo, "cache/filo_primatas.rds")
print(res_filo$diag)
}

## ===================================================================
## Seção 2: modelo animal para o tamanho de esquilos
## (os mesmos dados são usados nos tutoriais de modelo animal)
## ===================================================================
if ("animal" %in% secoes) {

## ---- dados-esquilos ----
esquilos <- read.table("../Tutoriais/squirels2.txt", header = TRUE)
pedigree <- read.table("../Tutoriais/squirelsPED.txt", header = TRUE)

## Matriz de parentesco aditivo A (2 x coeficiente de coancestria)
A <- as.matrix(nadiv::makeA(pedigree[, c("ID", "DAM", "SIRE")]))
A <- A[as.character(esquilos$ID), as.character(esquilos$ID)]

dat_esq <- list(
  N = nrow(esquilos),
  id = seq_len(nrow(esquilos)),
  S = standardize(esquilos$size),
  sexo = esquilos$sex,               # 1 ou 2
  F = standardize(esquilos$forage),  # qualidade do forrageamento
  L_A = t(chol(A))                   # A = L_A %*% t(L_A)
)

## ---- m-animal ----
m_animal <- ulam(
  alist(
    S ~ normal(mu, sigma_E),
    mu <- a[sexo] + bF * F + u[id],
    ## u ~ MVNormal(0, sigma_A^2 A), na forma
    ## não centrada: u = sigma_A * L_A * z
    vector[N]: z ~ normal(0, 1),
    transpars> vector[N]: u <<- sigma_A * (L_A * z),
    a[sexo] ~ normal(0, 1),
    bF ~ normal(0, 0.5),
    c(sigma_A, sigma_E) ~ exponential(1)
  ), data = dat_esq, chains = 4, cores = 4)

## ---- h2 ----
post_animal <- extract.samples(m_animal)
h2 <- with(post_animal,
  sigma_A^2 / (sigma_A^2 + sigma_E^2))

## ---- fim-esquilos ----
res_animal <- resume_modelos(list(m_animal = m_animal), descartar = "z")
res_animal$post$m_animal$h2 <- h2
res_animal$precis$m_animal <- adiciona_derivada(
  precis(m_animal, prob = 0.95, pars = c("a", "bF", "sigma_A", "sigma_E"), depth = 2),
  "h2", h2)
res_animal$sd_size <- sd(esquilos$size)
saveRDS(res_animal, "cache/animal_esquilos.rds")
print(res_animal$diag)
}
