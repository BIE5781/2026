## Funções para desenhar DAGs pequenos e minimalistas com ggplot2 + patchwork.
##
## - dag_plot():   desenha um DAG do dagitty em escala fixa (polegadas por unidade
##                 de coordenada), de modo que o tamanho da figura segue das coordenadas.
## - dag_painel(): junta vários DAGs com patchwork, com rótulos "(a) título".
## - dag_salva():  exporta a figura no tamanho correto e a inclui no documento.
##
## Convenções no código dagitty:
##   - `pos="x,y"` define as coordenadas (y cresce para cima);
##   - `[latent]`   nós não observados (círculo branco);
##   - `[adjusted]` variáveis de controle (círculo vermelho).

library(ggplot2)
library(patchwork)

dag_estilo <- list(
  escala      = 0.75,      # polegadas por unidade de coordenada
  margem      = 0.4,       # margem em volta dos nós (unidades de coordenada)
  ponto       = 2.6,       # tamanho dos nós (estilo "ponto")
  texto       = 13,        # tamanho dos rótulos (pt)
  seta_mm     = 1.8,       # comprimento da ponta da seta (mm)
  linha       = 0.35,      # espessura das setas
  dist_rotulo = 0.27,      # distância rótulo-nó (unidades de coordenada)
  cor_obs     = "black",
  cor_latente = "white",
  cor_ajuste  = "#e8291c",
  rotulo_tag  = 11         # tamanho dos rótulos dos painéis (pt)
)

## Lê nós, arestas, latentes e ajustados de um objeto ou texto dagitty.
dag_dados <- function(dag, latent = NULL, adjusted = NULL) {
  if (is.character(dag)) dag <- dagitty::dagitty(dag)
  coords <- dagitty::coordinates(dag)
  if (anyNA(coords$x)) stop("Defina as coordenadas de todos os nós com pos=\"x,y\".")
  nos <- data.frame(no = names(coords$x), x = unname(coords$x), y = unname(coords$y))
  if (is.null(latent)) latent <- dagitty::latents(dag)
  if (is.null(adjusted)) adjusted <- dagitty::adjustedNodes(dag)
  nos$tipo <- ifelse(nos$no %in% adjusted, "ajustado",
                     ifelse(nos$no %in% latent, "latente", "observado"))
  arestas <- dagitty::edges(dag)
  arestas <- arestas[arestas$e == "->", c("v", "w")]
  list(nos = nos, arestas = arestas)
}

## Desenha um DAG.
## `rotulos`: posição dos rótulos por nó, com pontos cardeais ("n", "s", "e", "w",
##            "ne", "nw", "se", "sw"). Padrão: acima dos nós na metade de cima do
##            grafo, abaixo dos demais.
## `estilo`:  "ponto" (nós como pontos, rótulos ao lado) ou "texto" (só o nome do nó).
dag_plot <- function(dag, rotulos = NULL, estilo = c("ponto", "texto"),
                     latent = NULL, adjusted = NULL, e = dag_estilo) {
  estilo <- match.arg(estilo)
  d <- dag_dados(dag, latent, adjusted)
  nos <- d$nos
  pol <- function(x) x / e$escala  # converte polegadas em unidades de coordenada

  # posição dos rótulos
  pos <- ifelse(nos$y >= mean(range(nos$y)), "n", "s")
  if (!is.null(rotulos)) pos[match(names(rotulos), nos$no)] <- rotulos
  dx <- c(n = 0, s = 0, e = 1, w = -1, ne = 0.6, nw = -0.6, se = 0.6, sw = -0.6)[pos]
  dy <- c(n = 1, s = -1, e = 0, w = 0, ne = 0.75, nw = 0.75, se = -0.75, sw = -0.75)[pos]
  nos$lx <- nos$x + dx * e$dist_rotulo
  nos$ly <- nos$y + dy * e$dist_rotulo

  # as setas começam e terminam na borda dos nós
  raio <- if (estilo == "ponto") {
    pol((e$ponto * .pt + .stroke / 2) / 2 / 72)  # raio do ponto, em polegadas -> unidades
  } else {
    pol(e$texto / 72 * 0.75)                     # espaço em volta do nome do nó
  }
  de <- match(d$arestas$v, nos$no)
  para <- match(d$arestas$w, nos$no)
  vx <- nos$x[para] - nos$x[de]
  vy <- nos$y[para] - nos$y[de]
  comp <- sqrt(vx^2 + vy^2)
  folga <- if (estilo == "ponto") pol(0.02) else 0
  setas <- data.frame(
    x = nos$x[de] + (raio + folga) * vx / comp,
    y = nos$y[de] + (raio + folga) * vy / comp,
    xend = nos$x[para] - (raio + folga) * vx / comp,
    yend = nos$y[para] - (raio + folga) * vy / comp
  )

  p <- ggplot() +
    geom_segment(data = setas, aes(x, y, xend = xend, yend = yend),
                 linewidth = e$linha,
                 arrow = arrow(type = "closed", length = unit(e$seta_mm, "mm")))
  if (estilo == "ponto") {
    p <- p +
      geom_point(data = nos, aes(x, y, fill = tipo), shape = 21, size = e$ponto,
                 stroke = 0.5, color = "black") +
      geom_text(data = nos, aes(lx, ly, label = no), size = e$texto / .pt)
  } else {
    p <- p + geom_text(data = nos, aes(x, y, label = no), size = e$texto / .pt)
  }

  xlim <- range(nos$x) + c(-1, 1) * e$margem
  ylim <- range(nos$y) + c(-1, 1) * e$margem
  p <- p +
    scale_fill_manual(values = c(observado = e$cor_obs, latente = e$cor_latente,
                                 ajustado = e$cor_ajuste), guide = "none") +
    coord_equal(xlim = xlim, ylim = ylim, expand = FALSE, clip = "off") +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))
  attr(p, "tamanho") <- c(largura = diff(xlim), altura = diff(ylim)) * e$escala
  p
}

## Junta vários DAGs lado a lado (ou em grade) e rotula os painéis.
## `titulos`: textos opcionais que seguem as letras, ex. "(a) Modelo 1".
dag_painel <- function(plots, ncol = length(plots), titulos = NULL,
                       espaco = 0.15, e = dag_estilo) {
  n <- length(plots)
  nrow <- ceiling(n / ncol)
  tam <- sapply(plots, attr, "tamanho")
  idx <- matrix(c(seq_len(n), rep(NA, nrow * ncol - n)), nrow, ncol, byrow = TRUE)
  larg <- apply(idx, 2, function(i) max(tam["largura", i[!is.na(i)]]))
  alt <- apply(idx, 1, function(i) max(tam["altura", i[!is.na(i)]]))

  tags <- paste0("(", letters[seq_len(n)], ")")
  if (!is.null(titulos)) tags <- paste(tags, titulos)
  altura_tag <- 1.8 * e$rotulo_tag / 72

  p <- wrap_plots(plots, ncol = ncol, widths = larg, heights = alt) +
    plot_annotation(tag_levels = list(tags)) &
    theme(plot.tag.position = "bottom",
          plot.tag = element_text(size = e$rotulo_tag, face = "plain",
                                  margin = margin(t = 4)),
          plot.margin = margin(0, espaco * 36, 0, espaco * 36))
  attr(p, "tamanho") <- c(largura = sum(larg) + ncol * espaco,
                          altura = sum(alt) + nrow * (altura_tag + 0.1))
  p
}

## Exporta a figura no tamanho definido por dag_plot()/dag_painel() e a inclui no
## documento no mesmo tamanho.
dag_salva <- function(p, arquivo, dpi = 300) {
  tam <- attr(p, "tamanho")
  dir.create(dirname(arquivo), showWarnings = FALSE, recursive = TRUE)
  ggsave(arquivo, p, width = tam[["largura"]], height = tam[["altura"]],
         dpi = dpi, bg = "white")
  knitr::include_graphics(arquivo, dpi = dpi)
}
