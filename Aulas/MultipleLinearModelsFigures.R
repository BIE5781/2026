pak::pkg_install(c("rstanarm", "bayesplot", "ggplot2", "cowplot", "viridis", "patchwork", "latex2exp", "extrafont", "tidyverse", "stats4",  "ascii"))
pak::pkg_install(c("colorspace", "viridisLite", "RColorBrewer", "munsell", "labeling", "isoband", "gtable", "farver", "scales", "numDeriv", "generics", "backports", "mvtnorm", "coda"))
remotes::install_github("rmcelreath/rethinking", force = TRUE)
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
library(rio)
library(ascii)

# log-level model

set.seed(123)
n <- 100
diameter <- runif(n, 10, 100)  # Tree diameters in cm
# Biomass follows an exponential relationship with diameter
# log(biomass) = a + b * diameter
biomass <- exp(2 + 0.02 * diameter + rnorm(n, 0, 0.3))

biomass = rlnorm(n, 2 + 0.02 * diameter, 0.3)

# Fit the model

# lognormal model in stan_glm
df = data.frame(diameter, biomass)
stan_fit = stan_glm(log(biomass) ~ diameter, data = df)
rstan::summary(stan_fit)[1:3, 1:5]

# Visualization
p = ggplot(data.frame(diameter = diameter, biomass = biomass), 
       aes(x = diameter, y = biomass)) +
  geom_point() +
  labs(x = "Tree Diameter (cm)", 
       y = "Biomass") +
  theme_cowplot(10)
p
ggsave("Aulas/figures/lognormal-plot.png", plot = p, 
       width = 1000, height = 750, units = "px")

# Model summary
model <- lm(log(biomass) ~ diameter)
precis(model)

# lognormal model
rt_fit = ulam(alist(biomass ~ lognormal(mu, sigma),
           mu <- a + b*diameter,
           a ~ normal(0, 2),
           b ~ normal(0, 1),
           sigma ~ exponential(1)),
        data = df, chains = 4, cores = 4)
precis(rt_fit, prob = 0.95)

# draw the nonlinear regression line using y = exp(a + b*x)
p = ggplot(data.frame(diameter = diameter, biomass = biomass), 
       aes(x = diameter, y = biomass)) +
  geom_point() +
  labs(x = "Tree Diameter (cm)", 
       y = "Biomass") +
       theme_cowplot(10) +
       geom_line(aes(y = exp(coef(rt_fit)["a"] + coef(rt_fit)["b"]*diameter)), col = 2)
p
ggsave("Aulas/figures/lognormal-model.png", plot = p, 
       width = 1000, height = 750, units = "px")

p2 = p + scale_y_log10(breaks = c(5, 10, 20, 40, 80, 80)) + labs(y = "Biomass (log scale)")
p2
ggsave("Aulas/figures/lognormal-model-logscale.png", plot = p2, 
       width = 1000, height = 750, units = "px")

# log-level  model
rt_fit = ulam(alist(
           log_biomass <- log(biomass),
           log_biomass ~ normal(mu, sigma),
           mu <- a + b*diameter,
           a ~ normal(0, 2),
           b ~ normal(0, 1),
           sigma ~ exponential(1)),
        data = data.frame(diameter = diameter, biomass = biomass), chains = 4, cores = 4)
precis(rt_fit, prob = 0.95)



set.seed(1)

df = import("2025/ipomopsis.txt")
p = ggplot(df, aes(Root, Fruit, color = Grazing)) + 
    geom_point() + 
    labs(x = "Basal diameter (mm)", y = "Fruit Dry Mass (mg)") +
    scale_color_manual(values = c("black", 2)) +
    theme_cowplot(10) + 
    theme(legend.position = "bottom") 
ggsave("Aulas/figures/ipomopsis-plot.png", plot = p, 
       width = 900, height = 750, units = "px")

# scale variables
df$Root = scale(df$Root, center = TRUE, scale = TRUE)
df$Fruit = scale(df$Fruit, center = TRUE, scale = TRUE)
p = ggplot(df, aes(Root, Fruit, color = Grazing)) + 
    geom_point() + 
    labs(x = "Basal diameter (z-scores)", y = "Fruit Dry Mass (z-scores)") +
    scale_color_manual(values = c("black", 2)) +
    theme_cowplot(10) + 
    theme(legend.position = "bottom") 
ggsave("Aulas/figures/ipomopsis-plot-scaled.png", plot = p, 
       width = 900, height = 750, units = "px")


# model with just treatment

fit = stan_glm(Fruit ~ Grazing, data = df, cores = 4)
summary(fit)[1:3, 1:5]

summary(fit, probs = c(0.025, 0.975))
df$Grazing0 = ifelse(df$Grazing == "Ungrazed", 1, 0)
m1 = ulam(alist(Fruit ~ normal(mu, sigma),
           mu <- a + b*Grazing0,
           a ~ normal(0, 1),
           b ~ normal(0, 1),
           sigma ~ exponential(1)), 
     data = df, chains = 4, cores = 4)
precis(m1, prob = 0.95)

p = ggplot(df, aes(x = Grazing0, y = Fruit, color = Grazing)) + 
    geom_jitter(width = 0.05) + 
    geom_abline(intercept = coef(m1)["a"], slope = coef(m1)["b"]) + 
    theme_cowplot(10) + 
    scale_color_manual(values = c("black", 2)) +
    theme(legend.position = "bottom") +
    scale_x_continuous(breaks = c(0, 1), labels = c("0\n(Grazed)", "1\n(Ungrazed)")) +
    labs(x = "Grazing", y = "Fruit Dry Mass (z-scores)")
ggsave("Aulas/figures/ipomopsis-model-single.png", plot = p, 
       width = 900, height = 750, units = "px")
e
# model with treatment and root size

m2 = ulam(alist(Fruit ~ normal(mu, sigma),
           mu <- a + b*Grazing0 + c*Root,
           a ~ normal(0, 1),
           b ~ normal(0, 1),
           c ~ normal(0, 1),
           sigma ~ exponential(1)), 
     data = df, chains = 4, cores = 4)
precis(m2, prob = 0.95)

fit = stan_glm(Fruit ~ Grazing + Root, data = df, cores = 4)
summary(fit)[1:4, 1:5]

p = ggplot(df, aes(x = Root, y = Fruit, color = Grazing)) + 
    geom_point() + 
    geom_abline(intercept = coef(m2)["a"], slope = coef(m2)["c"]) + 
    geom_abline(intercept = coef(m2)["a"] + coef(m2)["b"], slope = coef(m2)["c"], col = 2) +
    theme_cowplot(10) + 
    labs(x = "Basal diameter (z-scores)", y = "Fruit Dry Mass (z-scores)") +
    scale_color_manual(values = c("black", 2)) +
    theme(legend.position = "bottom")
p
ggsave("Aulas/figures/ipomopsis-model-double.png", plot = p, 
       width = 900, height = 750, units = "px")

png("Aulas/figures/ipomopsis-model-comparison.png", width = 1200, height = 600)
par(mfrow = c(1, 2))
plot(precis(m1, prob = 0.95), pars = c("a", "b"))
plot(precis(m2, prob = 0.95), pars = c("a", "b", "c"))
dev.off()



# Simualate an interaction between a binary and continuous variable

set.seed(1)

N = 100
Grazing = rbinom(N, 1, 0.5)
Root = rnorm(N)

Fruit = 1 + 2*Grazing + 3*Root + 4*Grazing*Root + rnorm(N, sd = 2)
#plot

df = data.frame(Fruit = Fruit, Grazing = c("Grazed", "Ungrazed")[Grazing+1], Root = Root)
p = ggplot(df, aes(Root, Fruit, color = Grazing, group = Grazing)) + 
    geom_point(size = 2.5, col = "white") +
    geom_point() + 
    labs(x = "Basal diameter", y = "Fruit Dry Mass") +
    scale_color_manual(values = c("black", 2)) +
    theme_cowplot(10) + 
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
    theme(legend.position = "bottom")
p
ggsave("Aulas/figures/ipomopsis-interaction-plot.png", plot = p, 
       width = 900, height = 750, units = "px")



# Age toxin example:breaks

set.seed(1)
age <- runif(200, 0, 50)
toxin <- rnorm(200, 10 + 0.4*age, 1)
size = 30 + 0.5*age - 1*toxin + rnorm(200, 0, 5)
df = data.frame(age = age, toxin = toxin, size = size)

p = ggplot(df, aes(toxin, size, color = age)) + 
    geom_point() + 
    labs(x = "Toxin", y = "Size") +
    scale_color_viridis() +
    theme_cowplot(10)
p
ggsave("Aulas/figures/toxin-plot.png", plot = p, 
       width = 1500, height = 800, units = "px")

df0 = df
df0$age = scale(df0$age, center = TRUE, scale = FALSE)
df0$toxin = scale(df0$toxin, center = TRUE, scale = FALSE)
df0$size = scale(df0$size, center = TRUE, scale = FALSE)

lm_fit = lm(size ~ toxin + age, data = df0) 
summary(lm_fit)
stan_fit = stan_glm(size ~ toxin, data = df0, cores = 4)
summary(stan_fit, probs = c(0.025, 0.975))[1:4, c(1, 3, 4, 5)]

mcmc_recover_intervals(as.data.frame(stan_fit), 
              true = c(30, -1, 0.5, 5),
               pars = c("(Intercept)", "toxin", "age", "sigma"), 
               point_size = 1, inner_size = 0.5, outer_size = 0.25)

rt_fit = ulam(alist(size ~ normal(mu, sigma),
           mu <- a + b*toxin + c*age,
           a ~ normal(0, 0.3),
           b ~ normal(0, 1),
           c ~ normal(0, 1),
           sigma ~ exponential(1)),
        data = df0, chains = 4, cores = 4)
precis(rt_fit, prob = 0.95)
png("Aulas/figures/toxin-precis.png")
plot(precis(rt_fit, prob = 0.95))
points(x = c(5, 0.5, -1, 0), y = 1:4, col = 2, pch = 16)
dev.off()

p = mcmc_recover_intervals(as.data.frame(rethinking::extract.samples(rt_fit)), 
              true = c(1, 1, 0.5, 5),
              pars = c("a", "b", "c", "sigma"), size = 3)
ggsave("Aulas/figures/toxin-model-comparison.png", plot = p, 
       width = 2000, height = 1000, units = "px")


# Interactions

data("tulips")
tulips

# plot
tulips$shadeF = as.factor(tulips$shade)
shift = rnorm(nrow(tulips), 0, 0.01)
tulips$waterJ = tulips$water + shift
p = ggplot(tulips, aes(x = waterJ, y = blooms, color = shadeF)) + 
    geom_point(size = 3, col = 1) + 
    geom_point(size = 2) + 
    scale_x_continuous(breaks = c(1, 2, 3)) +
    labs(x = "Water level", y = "Mean height (mm)", color = "Shade level") +
    theme_cowplot(10) + 
    theme(legend.position = c(0.1, 0.8)) 
p
ggsave("Aulas/figures/tulips-plot.png", plot = p, 
       width = 900, height = 750, units = "px")

#model



data("tulips")
df = tulips
df$water = scale(df$water, center = TRUE, scale = FALSE)
df$shade = scale(df$shade, center = TRUE, scale = FALSE)
df$blooms = scale(df$blooms, center = TRUE, scale = TRUE)
df$shadeF = as.factor(df$shade)

df$waterJ = df$water + shift
p = ggplot(df, aes(x = waterJ, y = blooms, color = shadeF)) + 
    geom_point(size = 3, col = 1) + 
    geom_point(size = 2) + 
    scale_x_continuous(breaks = c(-1, 0, 1)) +
    labs(x = "Water level", y = "Mean height (z-scores)", color = "Shade level") +
    theme_cowplot(10) + 
    theme(legend.position = c(0.1, 0.8)) 
p
ggsave("Aulas/figures/tulips-plot-scaled.png", plot = p, 
       width = 900, height = 750, units = "px")

lm_fit = lm(blooms ~ water*shade, data = df)
precis(lm_fit)

stan_fit = stan_glm(blooms ~ water + shade + water:shade, data = df, cores = 4)
summary(stan_fit, probs = c(0.025, 0.975))[1:5, 1:5]

rt_fit = ulam(alist(blooms ~ normal(mu, sigma),
           mu <- a + b*water + c*shade + d*water*shade,
           a ~ normal(0, 0.1),
           b ~ normal(0, 1),
           c ~ normal(0, 1),
           d ~ normal(0, 1),
           sigma ~ exponential(1)),
        data = df, chains = 4, cores = 4)
precis(rt_fit, prob = 0.95)

png("Aulas/figures/tulips-precis.png")
plot(precis(rt_fit, prob = 0.95))
dev.off()
# a + b*w + c*shade + d*w*shade
cf = coef(rt_fit)
a = cf["a"]
b = cf["b"]
c = cf["c"]
d = cf["d"]
col = scales::hue_pal()(3)
p = ggplot(df, aes(x = water, y = blooms, color = factor(shade))) + 
    geom_point(size = 3, col = 1) + 
    geom_point(size = 2) + 
    geom_abline(intercept = a - c, slope = b - d, col = col[1]) +
    geom_abline(intercept = a    , slope = b    , col = col[2]) +
    geom_abline(intercept = a + c, slope = b + d, col = col[3]) +
    theme_cowplot(10) + 
    labs(x = "Water level", y = "Mean height (z-scores)", color = "Shade level") +
    theme(legend.position = c(0.1, 0.8))
p
ggsave("Aulas/figures/tulips-model.png", plot = p, 
       width = 900, height = 750, units = "px")


# Power law relations between size and metabolic rate

dat = rio::import("https://raw.githubusercontent.com/BIE5781/2026/refs/heads/main/Aulas/bodyMass_metabolicRate.csv")
str(dat)
df = dat |>   
       select(phylum, class, "body mass", "metabolic rate") |> 
       rename("body.mass" = "body mass",
              "metabolic.rate" = "metabolic rate") |>
       filter(phylum == "Arthropoda") |>
       na.omit () |>
       as_tibble()
df
p1 = ggplot(df, aes(x = `body.mass`, y = `metabolic.rate`, 
                    color = class, shape = class)) + 
    geom_point() + 
    #scale_x_log10() + scale_y_log10() +
    labs(x = "Body mass (g)", y = "Metabolic Rate (W)") + 
    theme_cowplot(10)

library(ggforce)

p2 = ggplot(df, aes(x = `body.mass`, y = `metabolic.rate`, 
                    color = class, shape = class)) +  
    geom_point() + 
    scale_x_log10() + scale_y_log10() +
    labs(x = "Body mass (log scale)", y = "Metabolic Rate (log scale)") + 
    theme_cowplot(10) + theme(legend.position = "bottom")
p2
p1 + p2 + plot_layout(guides = 'collect')

ggsave("Aulas/figures/metabolic-rate-plot.png", plot = p2, 
       width = 2000, height = 1500, units = "px")

3/(4^3)
dbinom(2, 3, 0.25)/3

df = df |> 
       mutate(mass = log(body.mass),
              rate = log(metabolic.rate),
              phylo_i = as.numeric(factor(phylum)),
              class_i = as.numeric(factor(class)))
df$class_i |> unique()
df$phylo_i |> unique()

plot(df$mass, df$rate)

m_rate_mass <- ulam(alist(
    rate ~ normal(mu, sigma),
    mu <- a[class_i] + b[class_i] * mass,
    b[class_i] ~ normal(0, 1),
    a[class_i] ~ normal(0, 1),
    sigma ~ exponential(1)
), data = df[, c("mass", "rate", "class_i")], 
chains = 4, cores = 4)

precis(m_single, prob = 0.95, depth = 2)

# Make prediction plot with regression line and multiple intercepts for each class
prediction = expand_grid(
       class_i = sort(unique(df$class_i)),
       mass = seq(min(df$mass), max(df$mass), length.out = 100)
) |>
       mutate(class = levels(factor(df$class))[class_i])
prediction$rate = apply(
       link(m_single, data = list(mass = prediction$mass, class_i = prediction$class_i)),
       2, mean
)

p = ggplot(df, aes(x = mass, y = rate, color = class, shape = class)) +
       geom_point(alpha = 0.7) +
       geom_line(data = prediction, aes(group = class), linewidth = 0.8) +
       labs(x = "Body mass (log scale)", y = "Metabolic rate (log scale)",
               color = "Class", shape = "Class") +
    theme_cowplot(10) + theme(legend.position = "bottom")
p
ggsave("Aulas/figures/metabolic-rate-class-model.png", plot = p,
          width = 2000, height = 1500, units = "px")

levels(factor(df$class))
levels(factor(df$phylum))
samples = as.data.frame(rethinking::extract.samples(m_single))
slope_names = grep("^b([.]|\\[)", names(samples), value = TRUE)
slope_names = slope_names[order(as.integer(gsub("[^0-9]", "", slope_names)))]
class_levels = levels(factor(df$class))
stopifnot(length(slope_names) == length(class_levels))
slope_draws = samples[, slope_names, drop = FALSE]
names(slope_draws) = class_levels

mcmc_areas(slope_draws, prob = 0.95, prob_outer = 0.99) +
       labs(x = "Slope", y = "Class") +
       scale_y_discrete(labels = class_levels) +
       theme_cowplot(10) + geom_vline(xintercept = 3/4, linetype = "dashed", color = "firebrick")
ggsave("Aulas/figures/metabolic-rate-class-slopes.png", 
          width = 2000, height = 1500, units = "px")

pairwise_differences = list()
for (row_index in seq_along(class_levels)) {
       for (column_index in seq_along(class_levels)) {
              if (row_index > column_index) {
                     pair_name = paste(row_index, column_index, sep = "_")
                     pairwise_differences[[pair_name]] =
                            slope_draws[[row_index]] - slope_draws[[column_index]]
              }
       }
}
difference_range = range(unlist(pairwise_differences))

comparison_plots = vector("list", length(class_levels)^2)
for (row_index in seq_along(class_levels)) {
       for (column_index in seq_along(class_levels)) {
              plot_index = (row_index - 1) * length(class_levels) + column_index
              if (row_index > column_index) {
                     pair_name = paste(row_index, column_index, sep = "_")
                     prob_positive = mean(pairwise_differences[[pair_name]] > 0)
                     comparison_plots[[plot_index]] = ggplot(
                            data.frame(difference = pairwise_differences[[pair_name]]),
                            aes(x = difference)
                     ) +
                            geom_density(fill = "steelblue", alpha = 0.35) +
                            geom_vline(xintercept = 0, linetype = "dashed", color = "firebrick") +
                            coord_cartesian(xlim = difference_range) +
                            labs(title = paste(class_levels[row_index], "-", class_levels[column_index]),
                                      subtitle = sprintf("Pr(diff > 0) = %.2f", prob_positive),
                                 x = "Slope difference", y = "Density") +
                            theme_cowplot(10) +
                            theme(plot.title = element_text(size = 9),
                                       plot.subtitle = element_text(size = 7),
                                  axis.text = element_text(size = 7),
                                  axis.title = element_text(size = 8))
              } else {
                     comparison_plots[[plot_index]] = patchwork::plot_spacer()
              }
       }
}
p_slopes = patchwork::wrap_plots(comparison_plots, ncol = length(class_levels))
p_slopes
ggsave("Aulas/figures/metabolic-rate-class-slopes-contrasts.png", plot = p_slopes,
          width = 2000, height = 2000, units = "px")
