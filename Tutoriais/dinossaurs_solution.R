# Solução do exercício sobre a influência dos dinossauros
suppressPackageStartupMessages({
	library(rethinking)
	library(ggplot2)
	library(ggrepel)
	library(bayesplot)
})

set.seed(123)
output_dir <- file.path("figures", "dinosaurs_solution")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

save_plot <- function(plot, name, width = 8, height = 5) {
	ggsave(file.path(output_dir, paste0(name, ".png")), plot,
				 width = width, height = height, units = "in", dpi = 150, bg = "white")
}

# Prepare the data and use mammal means as common centering constants
animals <- MASS::Animals
animals$species <- rownames(animals)
animals$log_body <- log(animals$body)
animals$log_brain <- log(animals$brain)
large_dinosaurs <- c("Brachiosaurus", "Dipliodocus", "Triceratops")
animals$dinosaur <- as.integer(animals$species %in% large_dinosaurs)
animals$group <- ifelse(animals$dinosaur == 1, "Dinosaurs", "Mammals")
stopifnot(all(large_dinosaurs %in% animals$species))

mammals <- animals[animals$dinosaur == 0, ]
dinosaurs <- animals[animals$dinosaur == 1, ]
body_center <- mean(mammals$log_body)
brain_center <- mean(mammals$log_brain)
animals$log_body_c <- animals$log_body - body_center
animals$log_brain_c <- animals$log_brain - brain_center
mammals <- animals[animals$dinosaur == 0, ]
dinosaurs <- animals[animals$dinosaur == 1, ]

plot_animals <- ggplot(animals, aes(log_body, log_brain)) +
	geom_point(aes(color = group), size = 2) +
	geom_text_repel(data = dinosaurs, aes(label = species), color = "firebrick", size = 3) +
	scale_color_manual(values = c("Mammals" = "grey40", "Dinosaurs" = "firebrick")) +
	labs(x = "Log body weight (kg)", y = "Log brain weight (g)", color = NULL) +
	theme_minimal()
save_plot(plot_animals, "01_animals")

# Prior simulation for the regression lines
prior_draws <- data.frame(a = rnorm(30, 0, 1), b = rnorm(30, 0, 1))
body_grid <- seq(min(mammals$log_body), max(mammals$log_body), length.out = 100)
prior_lines <- do.call(rbind, lapply(seq_len(nrow(prior_draws)), function(index) {
	data.frame(
		log_body = body_grid,
		log_brain = brain_center + prior_draws$a[index] +
			prior_draws$b[index] * (body_grid - body_center),
		draw = index
	)
}))
brain_limits <- data.frame(
	log_brain = log(c(0.06, 9200)),
	reference = c("Etruscan shrew", "Sperm whale")
)
plot_prior <- ggplot() +
	geom_line(data = prior_lines,
						aes(log_body, log_brain, group = draw),
						color = "steelblue", alpha = 0.3, linewidth = 0.7) +
	geom_point(data = mammals, aes(log_body, log_brain), color = "grey40", size = 1.8) +
	geom_hline(data = brain_limits,
						 aes(yintercept = log_brain, linetype = reference),
						 color = "firebrick", linewidth = 0.6) +
	labs(x = "Log body weight (kg)", y = "Log brain weight (g)", linetype = NULL) +
	theme_minimal()
save_plot(plot_prior, "02_prior_lines")

# Fit a common-slope model with and without dinosaurs and a group-intercept model
fit_mammals <- ulam(
	alist(
		log_brain_c ~ dnorm(mu, sigma),
		mu <- a + b * log_body_c,
		a ~ dnorm(0, 1),
		b ~ dnorm(0, 1),
		sigma ~ dexp(1)
	),
	data = mammals, chains = 4, cores = 4, iter = 2000, refresh = 0
)

fit_all_shared <- ulam(
	alist(
		log_brain_c ~ dnorm(mu, sigma),
		mu <- a + b * log_body_c,
		a ~ dnorm(0, 1),
		b ~ dnorm(0, 1),
		sigma ~ dexp(1)
	),
	data = animals, chains = 4, cores = 4, iter = 2000,
	refresh = 0, log_lik = TRUE
)

fit_dinosaur_intercept <- ulam(
	alist(
		log_brain_c ~ dnorm(mu, sigma),
		mu <- a + b * log_body_c + dinosaur_effect * dinosaur,
		a ~ dnorm(0, 1),
		b ~ dnorm(0, 1),
		dinosaur_effect ~ dnorm(0, 2),
		sigma ~ dexp(1)
	),
	data = animals, chains = 4, cores = 4, iter = 2000,
	refresh = 0, log_lik = TRUE
)

fits <- list(
	mammals_only = fit_mammals,
	all_shared = fit_all_shared,
	dinosaur_intercept = fit_dinosaur_intercept
)
model_labels <- c(
	mammals_only = "Mammals only",
	all_shared = "All species, shared slope",
	dinosaur_intercept = "Dinosaur intercept"
)
model_data <- list(
	mammals_only = mammals,
	all_shared = animals,
	dinosaur_intercept = animals
)
posterior <- lapply(fits, rethinking::extract.samples)
model_parameters <- list(
	mammals_only = c("a", "b", "sigma"),
	all_shared = c("a", "b", "sigma"),
	dinosaur_intercept = c("a", "b", "dinosaur_effect", "sigma")
)

for (model_name in names(fits)) {
	cat("\n", model_labels[[model_name]], "\n")
	print(precis(fits[[model_name]], digits = 2))

	chain_draws <- attr(fits[[model_name]], "cstanfit")$draws(format = "draws_array")
	parameters <- model_parameters[[model_name]]
	save_plot(bayesplot::mcmc_trace(chain_draws, pars = parameters),
						paste0("trace_", model_name), 9, 6)
	save_plot(bayesplot::mcmc_dens_overlay(chain_draws, pars = parameters),
						paste0("density_chains_", model_name), 9, 6)
	save_plot(bayesplot::mcmc_intervals(chain_draws, pars = parameters,
																			prob = 0.5, prob_outer = 0.95),
						paste0("intervals_", model_name), 8, 5)
}

# Compare each posterior with the matching priors
prior <- list(
	a = rnorm(4000, 0, 1),
	b = rnorm(4000, 0, 1),
	sigma = rexp(4000, 1),
	dinosaur_effect = rnorm(4000, 0, 1)
)
to_long <- function(samples, model, distribution, parameters) {
	selected <- samples[parameters]
	counts <- lengths(selected)
	data.frame(
		model = model,
		parameter = rep(parameters, times = counts),
		value = unlist(selected, use.names = FALSE),
		distribution = distribution
	)
}
prior_posterior <- do.call(rbind, lapply(names(fits), function(model_name) {
	parameters <- model_parameters[[model_name]]
	rbind(
		to_long(prior[parameters], model_labels[[model_name]], "Prior", parameters),
		to_long(posterior[[model_name]], model_labels[[model_name]],
						"Posterior", parameters)
	)
}))
plot_prior_posterior <- ggplot(prior_posterior,
															 aes(value, color = distribution, fill = distribution)) +
	geom_density(alpha = 0.15) +
	facet_wrap(~ model + parameter, scales = "free", ncol = 3) +
	scale_color_manual(values = c("Prior" = "firebrick", "Posterior" = "steelblue")) +
	scale_fill_manual(values = c("Prior" = "firebrick", "Posterior" = "steelblue")) +
	labs(x = "Parameter value", y = "Density", color = NULL, fill = NULL) +
	theme_minimal()
save_plot(plot_prior_posterior, "posterior_vs_prior", 10, 8)

posterior_summary <- do.call(rbind, lapply(names(fits), function(model_name) {
	do.call(rbind, lapply(model_parameters[[model_name]], function(parameter) {
		values <- as.numeric(posterior[[model_name]][[parameter]])
		data.frame(
			model = model_labels[[model_name]],
			parameter = parameter,
			mean = mean(values),
			lower_95 = unname(quantile(values, 0.025)),
			upper_95 = unname(quantile(values, 0.975))
		)
	}))
}))
print(posterior_summary)

# Plot posterior-mean regression lines in the original log coordinates
make_line <- function(model_name, data_subset, dinosaur_value, series) {
	samples <- posterior[[model_name]]
	x_grid <- seq(min(data_subset$log_body), max(data_subset$log_body), length.out = 100)
	group_shift <- if ("dinosaur_effect" %in% names(samples)) {
		mean(samples$dinosaur_effect) * dinosaur_value
	} else {
		0
	}
	data.frame(
		model = model_labels[[model_name]],
		log_body = x_grid,
		log_brain = brain_center + mean(samples$a) +
			mean(samples$b) * (x_grid - body_center) + group_shift,
		series = series
	)
}
regression_lines <- rbind(
	make_line("mammals_only", mammals, 0, "Mammal-only fit"),
	make_line("all_shared", animals, 0, "Shared-slope fit"),
	make_line("dinosaur_intercept", mammals, 0, "Mammal group"),
	make_line("dinosaur_intercept", dinosaurs, 1, "Dinosaur group")
)
plot_points <- do.call(rbind, lapply(names(fits), function(model_name) {
	data.frame(animals, model = model_labels[[model_name]])
}))
plot_regressions <- ggplot() +
	geom_point(data = plot_points[plot_points$dinosaur == 0, ],
						 aes(log_body, log_brain), color = "grey45", size = 1.5) +
	geom_point(data = plot_points[plot_points$dinosaur == 1, ],
						 aes(log_body, log_brain), color = "firebrick", size = 2) +
	geom_line(data = regression_lines,
						aes(log_body, log_brain, color = series), linewidth = 1) +
	facet_wrap(~model) +
	scale_color_manual(values = c(
		"Mammal-only fit" = "grey20",
		"Shared-slope fit" = "steelblue",
		"Mammal group" = "darkgreen",
		"Dinosaur group" = "firebrick"
	)) +
	labs(x = "Log body weight (kg)", y = "Log brain weight (g)", color = NULL) +
	theme_minimal()
save_plot(plot_regressions, "regression_comparison", 10, 5)

# Posterior predictive density and paired-data checks
set.seed(456)
replicated_density <- list()
replicated_pairs <- list()
observed_density <- list()
mean_posterior_density <- list()
observed_pairs <- list()
for (model_name in names(fits)) {
	data_used <- model_data[[model_name]]
	y_rep <- rethinking::sim(fits[[model_name]], data = data_used, n = 500)
	mean_posterior <- colMeans(y_rep)
	selected <- sample(seq_len(nrow(y_rep)), min(30, nrow(y_rep)))
	y_selected <- y_rep[selected, , drop = FALSE]
	model_label <- model_labels[[model_name]]

	replicated_density[[model_name]] <- data.frame(
		log_brain_c = as.vector(y_selected),
		replicate = factor(rep(seq_len(nrow(y_selected)), times = ncol(y_selected))),
		model = model_label,
		type = "Replicated"
	)
	observed_density[[model_name]] <- data.frame(
		log_brain_c = data_used$log_brain_c,
		replicate = NA_integer_,
		model = model_label,
		type = "Observed"
	)
	mean_posterior_density[[model_name]] <- data.frame(
		log_brain_c = mean_posterior,
		replicate = NA_integer_,
		model = model_label,
		type = "Posterior predictive mean"
	)
	replicated_pairs[[model_name]] <- data.frame(
		log_body = rep(data_used$log_body, each = nrow(y_selected)),
		log_brain = brain_center + as.vector(y_selected),
		model = model_label,
		type = "Replicated"
	)
	observed_pairs[[model_name]] <- data.frame(
		log_body = data_used$log_body,
		log_brain = data_used$log_brain,
		model = model_label,
		type = "Observed"
	)
}

density_data <- rbind(
	do.call(rbind, observed_density),
	do.call(rbind, mean_posterior_density),
	do.call(rbind, replicated_density)
)
plot_predictive_density <- ggplot() +
	geom_density(data = density_data[density_data$type == "Replicated", ],
							 aes(log_brain_c, group = interaction(model, replicate), color = type),
							 alpha = 0.12, linewidth = 0.35) +
	geom_density(data = density_data[density_data$type == "Observed", ],
							 aes(log_brain_c, color = type), linewidth = 0.9) +
	geom_density(data = density_data[density_data$type == "Posterior predictive mean", ],
						 aes(log_brain_c, color = type), linetype = "dashed", linewidth = 0.9) +
	facet_wrap(~model, scales = "free") +
	scale_color_manual(values = c(
		"Observed" = "grey20",
		"Posterior predictive mean" = "firebrick",
		"Replicated" = "steelblue"
	)) +
	labs(x = "Centered log brain weight", y = "Density", color = NULL) +
	theme_minimal()
save_plot(plot_predictive_density, "predictive_density", 10, 5)

pair_data <- rbind(do.call(rbind, observed_pairs),
									 do.call(rbind, replicated_pairs))
plot_predictive_pairs <- ggplot() +
	geom_point(data = pair_data[pair_data$type == "Replicated", ],
						 aes(log_body, log_brain), color = "steelblue", alpha = 0.1, size = 0.8) +
	geom_point(data = pair_data[pair_data$type == "Observed", ],
						 aes(log_body, log_brain), color = "grey20", size = 1.5) +
	facet_wrap(~model) +
	labs(x = "Log body weight (kg)", y = "Log brain weight (g)") +
	theme_minimal()
save_plot(plot_predictive_pairs, "predictive_pairs", 10, 5)

# Compare predictive fit on the same data
cat("\nWAIC comparison for the two models fitted to all species\n")
print(rethinking::compare(fit_all_shared, fit_dinosaur_intercept))

cat("\nThe group model uses one shared slope. dinosaur_effect is the shift in the dinosaur intercept.\n")
