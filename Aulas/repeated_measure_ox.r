library(MethComp)
library(rethinking)
library(tidyverse)



data(ox)



wide_ox = ox |>
    pivot_wider(id_cols = c(item, repl), 
                names_from = "meth", 
                values_from = "y") |>
    mutate(item = as.integer(factor(item)),
        sCO = as.numeric(scale(CO)),
        sPulse = as.numeric(scale(pulse)))
    
wide_ox_mean <- wide_ox |> 
    group_by(item) |>
    summarise(
        mean_CO = mean(CO),
        mean_pulse = mean(pulse)
    ) 
    
 wide_ox_mean |>
    ggplot(aes(x = mean_pulse, y = mean_CO)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "firebrick") +
    theme_cowplot(10) +
    labs(x = "Mean pulse Measurement", y = "Mean CO Measurement")  
lm(mean_CO ~ mean_pulse, data = wide_ox_mean) |>
    summary()
    
wide_ox |>
    ggplot(aes(x = sPulse, y = sCO)) +
    geom_point(alpha = 0.3) +
    geom_point(data = wide_ox_mean, aes(x = scale(mean_pulse), y = scale(mean_CO)), color = 4, size = 2) +
    geom_smooth(method = "lm", se = FALSE, color = "firebrick") +
    theme_cowplot(10) +
    labs(x = "Pulse Measurement (Z-Scores)", y = "Blood CO Measurement (Z-Scores)")

dlist_ox <- list(
  N_item = length(unique(wide_ox$item)),
  sCO = wide_ox$sCO,
  sPulse = wide_ox$sPulse,
  item_i = wide_ox$item,
  id = unique(wide_ox$item)
)

ulam_model <- ulam(
  alist(
    # Model for the observed data
    sCO ~ normal(co_true[item_i], s_error_co),
    sPulse ~ normal(pulse_true[item_i], s_error_pulse),

    # Latend variable model
    vector[N_item]:co_true ~ normal(mu, sigma_co),
    vector[N_item]:mu <- a + bP * pulse_true[id],

    vector[N_item]:pulse_true ~ normal(0, sigma_pulse),

    a ~ normal(0, 0.3),
    bP ~ normal(0, 0.3),

    # True SDs
    sigma_co ~ exponential(1),
    sigma_pulse ~ exponential(1),
    # Measurement SDs
    s_error_co ~ exponential(1),
    s_error_pulse ~ exponential(1)
  ),
  data = dlist_ox,
  chains = 4, cores = 4, iter = 3000
)

(estimates = precis(ulam_model, depth = 2))
# saveRDS(estimates, "Aulas/estimates_repeated_measure_ox.rds")

estimates[grep("pulse_true", rownames(estimates)), ]
estimates[grep("co_true", rownames(estimates)), ]

mutate(wide_ox_mean, pulse_true = estimates[grep("pulse_true", rownames(estimates)), "mean"],
       co_true = estimates[grep("co_true", rownames(estimates)), "mean"]) |>
    ggplot(aes(x = pulse_true, y = co_true)) +
    geom_point(size = 2) +
    geom_point(data = wide_ox_mean, aes(x = scale(mean_pulse), y = scale(mean_CO)), color = 4, size = 1) +
    geom_smooth(method = "lm", se = FALSE, color = "firebrick") +
    theme_cowplot(10) +
    labs(x = "Estimated True Pulse Measurement", y = "Estimated True CO Measurement")
