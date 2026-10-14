pak::pkg_install(c("rmcelreath/rethinking", "rstanarm", "bayesplot", "ggplot2", "cowplot", "viridis", "patchwork", "latex2exp", "extrafont", "tidyverse", "stats4"))
library(rethinking)
library(rstanarm)
library(bayesplot)
library(ggplot2)
library(cowplot)
library(viridis)
library(patchwork)
library(latex2exp)
library(extrafont)
library(tidyverse)
library(stats4)

#font_import()
loadfonts(device = "all", quiet = TRUE) 


## Chimp example

data("chimpanzees")
d <- chimpanzees
d$treatment <- 1 + d$prosoc_left + 2*d$condition

dat_list <- list(
  pulled_left = d$pulled_left,
  actor = d$actor,
  block_id = d$block,
  treatment = as.integer(d$treatment) )

set.seed(13)
m.Hierarchical <- ulam(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a0 + z[actor]*sigma_a +    # actor intercepts
                     x[block_id]*sigma_g + # block intercepts
                    b[treatment] ,         # treatment intercepts
    b[treatment] ~ dnorm( 0 , 0.5 ),
    z[actor] ~ dnorm( 0 , 1 ),
    x[block_id] ~ dnorm( 0 , 1 ),
    a0 ~ dnorm( 0 , 1.5 ),
    sigma_a ~ dexp(1),
    # block intercepts
    sigma_g ~ dexp(1),
    gq> vector[actor]:a <<- a0 + z*sigma_a,
    gq> vector[block_id]:g <<- x*sigma_g
  ) , data=dat_list , chains=4 , cores=4, log_lik = TRUE )

m.noPooling <- ulam(
  alist(
    # likelihood
    pulled_left ~ dbinom( 1, p ) ,
    logit(p) <- a0 +    a[actor] + # actor intercepts
                     g[block_id] + # block intercepts
                    b[treatment] , # treatment intercepts

    # priors 
    b[treatment] ~ dnorm( 0 , 0.5 ),
    a0 ~ dnorm( 0, 1.5 ), # overall intercept
    a[actor] ~ dnorm( 0 , 1.5 ),
    g[block_id] ~ dnorm( 0 , 1.5 )
  ) , data=dat_list , chains=4 , cores=4 , log_lik = TRUE )

plot(rethinking::compare(m.Hierarchical, m.noPooling))

precis(m.H.nc, depth = 2)
