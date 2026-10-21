## =============================================================================
## Preparação dos dados da população brasileira por faixa etária
##
## Fonte: IBGE, Censo Demográfico 2022, pirâmide etária do Brasil
##   (Panorama do Censo 2022: https://censo2022.ibge.gov.br/panorama/)
##   Arquivo exportado: "Censo_2022_Piramide_etaria_Brasil.csv"
##
## Resultado: arquivo "pop_brasileira_censo_2022.rds", com a população total
## (mulheres + homens) em classes de 5 anos e o ponto médio de cada classe.
## =============================================================================

library(dplyr)

censo <- read.csv2("Censo_2022_Piramide_etaria_Brasil.csv",
                   fileEncoding = "UTF-8", check.names = FALSE)[, 1:3] |>
    rename(idade = "Grupo de idade",
           pop.fem = "População feminina(pessoas)",
           pop.mas = "População masculina(pessoas)")

pop.br <- censo |>
    transmute(idade,
              age_lower = as.numeric(sub("^(\\d+).*", "\\1", idade)),
              pop = pop.fem + pop.mas) |>
    arrange(age_lower) |>                 # o arquivo vem da maior para a menor idade
    mutate(idade.meio = age_lower + 2.5,  # ponto médio das classes de 5 anos
                                          # (a classe aberta "100 anos ou mais" recebe 102,5)
           idade = factor(idade, levels = idade)) |>
    select(idade, idade.meio, pop)
pop.br
sum(pop.br$pop)   # confere: 203.080.756

saveRDS(pop.br, file = "pop_brasileira_censo_2022.rds")
