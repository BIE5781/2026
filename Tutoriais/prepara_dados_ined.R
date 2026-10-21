## =============================================================================
## Preparação dos dados de óbitos por COVID-19 por país e faixa etária
##
## Fonte: The Demography of COVID-19 Deaths Database (INED)
##   Caporali, A., Garcia, J., Couppié, É. et al. (2022). The demography of
##   COVID-19 deaths database, a gateway to well-documented international data.
##   Scientific Data 9, 93. https://doi.org/10.1038/s41597-022-01191-y
##   Cópia dos dados no figshare (dezembro de 2021):
##   https://doi.org/10.6084/m9.figshare.c.5807027
##
## Resultado: arquivo "obitos_covid_paises_selecionados.rds", com os óbitos
## acumulados até a data de corte e a população de cada país, em faixas
## etárias de 20 anos (0-19, 20-39, 40-59, 60-79, 80+).
## =============================================================================

library(dplyr)
library(readr)

## 1. Download da cópia do figshare (só na primeira vez) ----------------------
arquivos <- c(
    mortes   = "https://ndownloader.figshare.com/files/33802358",  # Cum_deaths_by_age_sex.csv
    pop      = "https://ndownloader.figshare.com/files/33802394",  # Population.csv
    codebook = "https://ndownloader.figshare.com/files/33802352")  # Codebook_Variable_Description.xlsx
destinos <- c("Cum_deaths_by_age_sex.csv",
              "Population.csv",
              "Codebook_Variable_Description.xlsx")
for (i in seq_along(arquivos))
    if (!file.exists(destinos[i]))
        download.file(arquivos[i], destinos[i], mode = "wb")

mortes.raw <- read.csv(destinos[1])
pop.raw    <- read.csv(destinos[2])

## 2. Óbitos: uma série por país, na última data até o corte ------------------
corte <- as.Date("2021-11-27")   # mesma data final dos óbitos do Brasil

mortes <- mortes.raw |>
    mutate(data = as.Date(death_reference_date)) |>
    filter(data <= corte)

## Um país pode ter mais de uma série (ex.: por data de ocorrência e por data
## de registro). Para cada país, fica a série com a contagem mais recente.
fonte <- mortes |>
    group_by(country, excelsheet) |>
    summarise(data = max(data), .groups = "drop") |>
    group_by(country) |>
    slice_max(data, n = 1, with_ties = FALSE) |>
    ungroup()
fonte |> arrange(data) |> print(n = Inf)   # confira as datas e as séries escolhidas

mortes <- mortes |>
    semi_join(fonte, by = c("country", "excelsheet", "data")) |>
    select(country, age_group, data, obitos = cum_death_both)

## 3. Idade inicial de cada classe ----------------------------------------------
## Lê o primeiro número do rótulo ("<1" vira 0; linhas "Total ..." viram NA)
mortes <- mortes |>
    mutate(age_lower = ifelse(grepl("^Total", age_group), NA,
                       ifelse(grepl("^<", age_group), 0,
                              as.numeric(sub("^\\D*(\\d+).*", "\\1", age_group)))))
## Óbitos com idade desconhecida, por país (ficam fora da análise)
mortes |> filter(age_group == "Total unknown") |> select(country, obitos)
mortes <- filter(mortes, !is.na(age_lower))

## 4. População: limpa os números e usa a estimativa mais recente -------------
pop <- pop.raw |>
    mutate(pop = parse_number(pop_both),
           pop_date = as.Date(pop_date)) |>
    select(country, age_lower, pop_date, pop) |>
    group_by(country) |>
    filter(pop_date == max(pop_date)) |>
    ungroup()

## 5. Classes de idade comuns ----------------------------------------------------
## Idades de corte presentes nos dois arquivos, para cada país
esquemas     <- lapply(split(mortes$age_lower, mortes$country), unique)
esquemas.pop <- lapply(split(pop$age_lower, pop$country), unique)
comuns <- mapply(intersect, esquemas, esquemas.pop[names(esquemas)])
sapply(comuns, function(x) paste(sort(x), collapse = ", "))

## Classes de 20 anos; a última (80+) é aberta
cortes <- c(0, 20, 40, 60, 80)
rotulos <- c(paste(head(cortes, -1), cortes[-1] - 1, sep = "-"),
             paste0(tail(cortes, 1), "+"))

## Países cujos dois arquivos têm todos esses cortes
compativel <- sapply(comuns, function(x) all(cortes %in% x))
names(compativel)[!compativel]        # países excluídos por incompatibilidade

reagrupa <- function(d, valor) {
    d |>
        filter(country %in% names(compativel)[compativel]) |>
        mutate(idade = factor(rotulos[findInterval(age_lower, cortes)], levels = rotulos)) |>
        group_by(pais = country, idade) |>
        summarise(n = sum({{ valor }}), .groups = "drop")
}

## 6. Tabela final ----------------------------------------------------------------
final <- reagrupa(mortes, obitos) |>
    rename(obitos = n) |>
    inner_join(rename(reagrupa(pop, pop), pop = n), by = c("pais", "idade")) |>
    ## Países excluídos da análise (ver justificativa no tutorial)
    filter(!pais %in% c("Romania", "Japan", "South Korea", "Moldova")) |>
    mutate(pop.e5 = pop / 1e5,          # população em unidades de 100 mil habitantes
           taxa = obitos / pop.e5)      # óbitos por 100 mil habitantes

## Ponto médio de cada classe: início + metade da largura (10, 30, 50, 70).
## A classe aberta 80+ recebe 90 (valor assumido, como se tivesse 20 anos).
meio <- c(head(cortes, -1) + diff(cortes) / 2, tail(cortes, 1) + 10)
final$idade.meio <- meio[as.integer(final$idade)]
distinct(final, idade, idade.meio)
length(unique(final$pais))             # número de países

saveRDS(final, "obitos_covid_paises_selecionados.rds")
