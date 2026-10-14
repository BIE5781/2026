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

ggplot(data.frame(x = rnorm(1000)), aes(x)) +
  geom_histogram(aes(y = ..density..), bins=2^6, colour="white", fill="DarkGray") +
  geom_function(fun = dnorm, colour = 2, linewidth  = 2) + theme_cowplot() +
  labs(x = expression(theta))

fig.height = 500
fig.width = fig.height * 1.2
axis.font.size = 30
figure.font = "Calibri"

### Baseball example

# set.seed(4)
set.seed(5)
N_players = 25

true_avg = rbeta(N_players, 8, 20)
hist(true_avg)

d = matrix(NA, ncol = 2, nrow = N_players)
for(i in 1:N_players){
  d[i, 1] = at_bats = sample(5:30, 1)
  d[i, 2] = rbinom(1, at_bats, true_avg[i])
}
MLE = d[,2]/d[,1]
m1 = ulam(alist(
  hits ~ binomial(at_bats, theta),
  theta <- p[player],
  p[player] ~ beta(nu0, nu1),
  nu0 ~ lognormal(0, 1),
  nu1 ~ lognormal(0, 1)
), data = list(hits = d[,2], 
               at_bats = d[,1], 
               player = 1:N_players), 
chains = 4, cores = 4, iter = 2000)
ulam = precis(m1, depth = 2)$mean[1:N_players]
xl = range(c(ulam, MLE, true_avg)) + c(-0.01, 0.01)
{
png("figures/baseball_estimates.png", height = 500, width = 700, bg = "transparent")
par(mar = c(5, 6, 2,2))
  plot(MLE, rep(3, N_players), 
     ylim = c(0.5, 3.5), pch = 19, 
     col = "blue", xlim = xl, xlab = "Batting Average (AVG)",
     yaxt="n", ylab="" , cex.axis=2, cex.lab = 2)
points(ulam, rep(1, N_players), pch = 19, col = 2)
points(true_avg, rep(2, N_players), pch = 19, col = 1)
segments(MLE, rep(3, N_players), true_avg, rep(2, N_players), lwd = 0.7, lty = 2)
segments(ulam, rep(1, N_players), true_avg, rep(2, N_players), lwd = 0.7, lty = 2)
axis(2, at = c(1, 2, 3), labels = c("Bayes", "True", "MLE"), cex.axis=2, adj = 1,     las = 2,)
dev.off()
}

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
m13.4nc <- ulam(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a_bar + z[actor]*sigma_a + # actor intercepts
      x[block_id]*sigma_g +
      b[treatment] ,
    b[treatment] ~ dnorm( 0 , 0.5 ),
    z[actor] ~ dnorm( 0 , 1 ),
    x[block_id] ~ dnorm( 0 , 1 ),
    a_bar ~ dnorm( 0 , 1.5 ),
    sigma_a ~ dexp(1),
    # block intercepts
    sigma_g ~ dexp(1),
    gq> vector[actor]:a <<- a_bar + z*sigma_a,
    gq> vector[block_id]:g <<- x*sigma_g
  ) , data=dat_list , chains=4 , cores=4, log_lik = TRUE )

m13.5nc <- ulam(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a_bar + z[actor] + # actor intercepts
      x[block_id] +
      b[treatment] ,
    b[treatment] ~ dnorm( 0 , 0.5 ),
    z[actor] ~ dnorm( 0 , 1.5 ),
    x[block_id] ~ dnorm( 0 , 1.5 ),
    a_bar ~ dnorm( 0 , 1.5 )
  ) , data=dat_list , chains=4 , cores=4 , log_lik = TRUE )

rethinking::compare(m13.4nc, m13.5nc)
precis(m13.4nc, depth = 2)
samples = as.data.frame(extract.samples(m13.4nc))


png("figures/actors_sigma_a.png", height = 500, width = 1000, bg = "transparent")
plot_grid(mcmc_intervals(samples, regex_pars = "^a\\..", transformations = inv_logit) + 
  geom_vline(data = NULL, xintercept = 0.5) + scale_x_continuous(limits = c(0, 1)) + 
  labs(x = "Actor effect\n(Left handedness)", y = "Actors") +  
  scale_y_discrete(labels = 1:7) + 
  theme(axis.title = element_text(size = axis.font.size, 
                                  family = figure.font, 
                                  color = "#586E75"), 
        axis.text = element_text(size = axis.font.size*0.8, 
                                 family = figure.font, 
                                 color = "#586E75")) ,
ggplot(samples) + 
  geom_density(aes(x= sigma_a), color = 2, linewidth = 2) + 
  theme_minimal() +
  labs(y = "Density", x = "Standard deviation") +  
  theme(plot.title = element_text(size = axis.font.size, 
                                  family = figure.font, 
                                  color = "#586E75")
        , axis.title = element_text(size = axis.font.size, 
                                  family = figure.font, 
                                  color = "#586E75"), 
        axis.text = element_text(size = axis.font.size*0.8, 
                                 family = figure.font, 
                                 color = "#586E75")))
dev.off()

png("figures/blocks_sigma_g.png", height = 500, width = 1000, bg = "transparent")
plot_grid(mcmc_intervals(samples, regex_pars = "^g\\..", transformations = inv_logit) + 
  geom_vline(data = NULL, xintercept = 0.5) + scale_x_continuous(limits = c(0, 1)) + 
  labs(x = "Block effect", y = "blocks") +  
  scale_y_discrete(labels = 1:7) + 
  theme(axis.title = element_text(size = axis.font.size, 
                                  family = figure.font, 
                                  color = "#586E75"), 
        axis.text = element_text(size = axis.font.size*0.8, 
                                 family = figure.font, 
                                 color = "#586E75")) ,
  ggplot(samples) + 
  geom_density(aes(x= sigma_g), color = 1, linewidth = 2) + 
  theme_minimal() +
  labs(y = "Density", x = "Standard deviation") +  
  theme(plot.title = element_text(size = axis.font.size, 
                                  family = figure.font, 
                                  color = "#586E75")
        , axis.title = element_text(size = axis.font.size, 
                                    family = figure.font, 
                                    color = "#586E75"), 
        axis.text = element_text(size = axis.font.size*0.8, 
                                 family = figure.font, 
                                 color = "#586E75")))
dev.off()

png("figures/sigma_ag.png", height = 500, width = 700, bg = "transparent")
ggplot(samples) + 
  geom_density(aes(x= sigma_a), color = 2, linewidth = 2) + 
  geom_density(aes(x= sigma_g), color = 1, linewidth = 2) + 
  annotate("text", x = 0.6, y = 3, label= "Block",
           family= figure.font, 
           size= axis.font.size*0.3) +
  annotate("text", x = 2.5, y = 0.8, label= "Actor", color = 2,
           family= figure.font, 
           size= axis.font.size*0.3) +
  theme_minimal() +
  labs(y = "Density", x = "Standard deviation") +  
  theme(plot.title = element_text(size = axis.font.size, 
                                  family = figure.font, 
                                  color = "#586E75")
        , axis.title = element_text(size = axis.font.size, 
                                    family = figure.font, 
                                    color = "#586E75"), 
        axis.text = element_text(size = axis.font.size*0.8, 
                                 family = figure.font, 
                                 color = "#586E75"))
dev.off()

png("figures/chimp_treatments.png", height = 500, width = 700, bg = "transparent")
mcmc_intervals(samples, regex_pars = "^b\\..", transformations = inv_logit) + 
  geom_vline(data = NULL, xintercept = 0.5) + scale_x_continuous(limits = c(0, 1)) + 
  labs(x = "Treatment effects", y = "Treatments") +  
  scale_y_discrete(labels = c("1. R/N", "2. L/N", "3. R/P", "4. L/P")) + 
  theme(axis.title = element_text(size = axis.font.size, 
                                  family = figure.font, 
                                  color = "#586E75"), 
        axis.text = element_text(size = axis.font.size*0.8, 
                                 family = figure.font, 
                                 color = "#586E75"))
dev.off()


diffs <- data.frame(
  db13 = samples$b.1 - samples$b.3,
  db24 = samples$b.2 - samples$b.4 )
png("figures/chimp_contrasts.png", height = 500, width = 700, bg = "transparent")
mcmc_intervals(diffs, transformations = inv_logit) + 
  geom_vline(data = NULL, xintercept = 0.5) + scale_x_continuous(limits = c(0, 1)) + 
  labs(x = "Treatment effects", y = "Treatments") +  
  scale_y_discrete(labels = c("R / N - P", "L / N - P")) + 
  theme(axis.title = element_text(size = axis.font.size, 
                                  family = figure.font, 
                                  color = "#586E75"), 
        axis.text = element_text(size = axis.font.size*0.8, 
                                 family = figure.font, 
                                 color = "#586E75"))
dev.off()


m11.4 <- ulam(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a[actor] + b[treatment] ,
    a[actor] ~ dnorm( 0 , 1.5 ),
    b[treatment] ~ dnorm( 0 , 0.5 )
  ) , data=dat_list , chains=4 , log_lik=TRUE )
precis( m11.4 , depth=2 )

post <- extract.samples(m11.4)
p_left <- inv_logit( post$a )
plot( precis( as.data.frame(p_left) ) , xlim=c(0,1) )

labs <- c("R/N","L/N","R/P","L/P")
plot( precis( m11.4 , depth=2 , pars="b" ) , labels=labs )

diffs <- list(
  db13 = post$b[,1] - post$b[,3],
  db24 = post$b[,2] - post$b[,4] )
plot( precis(diffs) )

pl <- by( d$pulled_left , list( d$actor , d$treatment ) , mean )
pl[1,]

plot( NULL , xlim=c(1,28) , ylim=c(0,1) , xlab="" ,
      ylab="proportion left lever" , xaxt="n" , yaxt="n" )
axis( 2 , at=c(0,0.5,1) , labels=c(0,0.5,1) )
abline( h=0.5 , lty=2 )
for ( j in 1:7 ) abline( v=(j-1)*4+4.5 , lwd=0.5 )
for ( j in 1:7 ) text( (j-1)*4+2.5 , 1.1 , concat("actor ",j) , xpd=TRUE )
for ( j in (1:7)[-2] ) {
  lines( (j-1)*4+c(1,3) , pl[j,c(1,3)] , lwd=2 , col=rangi2 )
  lines( (j-1)*4+c(2,4) , pl[j,c(2,4)] , lwd=2 , col=rangi2 )
}
points( 1:28 , t(pl) , pch=16 , col="white" , cex=1.7 )
points( 1:28 , t(pl) , pch=c(1,1,16,16) , col=rangi2 , lwd=2 )
yoff <- 0.01
text( 1 , pl[1,1]-yoff , "R/N" , pos=1 , cex=0.8 )
text( 2 , pl[1,2]+yoff , "L/N" , pos=3 , cex=0.8 )
text( 3 , pl[1,3]-yoff , "R/P" , pos=1 , cex=0.8 )
text( 4 , pl[1,4]+yoff , "L/P" , pos=3 , cex=0.8 )
mtext( "observed proportions\n" )



library(rstan)
library(dplyr)
library(tidyr)
library(ggplot2)
library(growthcurver) #contains the growthdata dataset
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

long_growthdata <- growthdata %>% gather(well,absorbance,-time)
glimpse(long_growthdata)

png("figures/growth_data.png", height = 500, width = 700, bg = "transparent")
ggplot(long_growthdata,aes(time,absorbance,group=well)) +
  geom_line() + 
  theme_minimal() + theme(axis.title = element_text(size = axis.font.size, 
                                               family = figure.font, 
                                               color = "#586E75"), 
                     axis.text = element_text(size = axis.font.size*0.8, 
                                              family = figure.font, 
                                              color = "#586E75"))
dev.off()
