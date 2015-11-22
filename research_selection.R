'
`research_selection.R`

This R script runs a small simulation of a research selection process that
can produce a sponsorship effect without bias or research misconduct.  

For discussion see here:  
http://jefais.tumblr.com/post/133751046358/sponsorship-effects-without-bias
'

require(cowplot)
require(ggplot2)

n_researchers <- 100		# no. researchers in each generation
n_generations <- 20			# no. generations to run the simulation
perc_replace <- .10			# what percentage of researchers to replace in each generation

n_experiments <- 20			# no. experiments run by each researcher in each generation
n <- 50						# sample size in each experiment
perc_null <- .5				# percent of experiments where the null is true
mu_bound <- 10				# positive bound on uniform distribution for mu ≠ 0
alpha <- .05				# statistical significance threshold


# Set a seed for the simulation
set.seed(1029384756)


new_researchers <- function(n = perc_replace * n_researchers) {
	'
	Generate a set of n new researchers
	'
	# Use the next two lines for a uniform distribution
	#bound_effect <- 3.33		# positive bound on uniform distribution for researcher effect
	#return(runif(perc_replace*n_researchers, min = -bound_effect, max = bound_effect))
	# Use the next three lines for a Gaussian distribution
	effect_mean = 0
	effect_sd = 1
	return(rnorm(n, mean = effect_mean, sd = effect_sd))
}

run_experiments <- function (researcher_effect) {
	'
	Run the experiments for a researcher with the given `researcher_effect`.  
	Each experiment is a one-sided one-sample t-test against the null mu_0 = 0.
	Returns the percent of successful (statistically significant) experiments.  
	'
	results <- c()
	for (exp in 1:n_experiments) {
		# Generate the sample
		null_true <- runif(1) >= perc_null
		if (null_true) {
			mu <- 0
		} else {
			mu <- runif(1, min = -mu_bound, max = mu_bound)
		}
		obs <- rnorm(n, mean = mu, sd = 1)
		obs.effected <- obs + researcher_effect
		p <- t.test(obs.effected, alternative = 'greater')$p.value
		results <- c(results, p <= alpha)
	}
	return(sum(results)/n_experiments)
}

# Estimate the no-replacement success rate
no_selection <- c()
researchers <- new_researchers(n_researchers)
for (i in 1:n_generations) {
	for (researcher in researchers) {
		no_selection <- c(no_selection, run_experiments(researcher))
	}
}
no_selection_mean <- mean(no_selection)

# Initialize the data frames to hold the data
# The first one holds the researcher-level data. 
#  This is long: each row has data for a single researcher in a single generation
data_researcher <- data.frame(generation = c(), effect = c(), success.rate = c())
# This one holds the generation-level data.
#  Each row has data for a single generation
data_generation <- data.frame(generation = c(), 
							   effect.mean = c(), effect.sd = c(),
							   success.mean = c(), success.sd = c())

# Generation 1 researchers
researchers <- new_researchers(n_researchers)
# Run the simulation:
# For each generation, 
for (gen in 1:n_generations) {
	success_gen <- c()
	# For each researcher, 
	for (researcher_effect in researchers) {
		# Run the experiments and get the success rate
		success <- run_experiments(researcher_effect)
		success_gen <- c(success_gen, success)
		# Add the effect and success rates to the data.researcher frame
		new_row <- data.frame(generation = gen, 
							 effect = researcher_effect, 
							 success.rate = success)
		data_researcher <- rbind(data_researcher, new_row)
	}
	# Calculate the generation-level statistics
	new_row <- data.frame(generation = gen, 
						 effect.mean = mean(researchers), 
						 effect.sd = sd(researchers),
						 success.mean = mean(success_gen), 
						 success.sd = sd(success_gen))
	# And add to the data_generation frame
	data_generation <- rbind(data_generation, new_row)
	
	# Researchers to keep
	researchers <- sort(researchers)
	keep <- researchers[(perc_replace*n_researchers+1) : n_researchers]
	# New researchers
	new <- new_researchers()
	researchers <- c(new, keep)
}

# Plot simulation results
# Effect
effect_plot <- ggplot() +
	geom_point(data = data_researcher, aes(x = generation, y = effect), 
			   alpha = .2, position = position_jitter(height=0)) +
	geom_line(data = data_generation, aes(x = generation, y = effect.mean),
			  size = 2, color = 'blue') +
	ylab('researcher effect')
effect_sd_plot <- ggplot() +
	geom_line(data = data_generation, aes(x = generation, y = effect.sd),
			  size = 2, color = 'blue') +
	ylab('researcher effect (sd)')
#plot_grid(effect_plot, effect_sd_plot, align='h', ncol = 1)

# Success rate
success_plot <- ggplot() +
	geom_point(data = data_researcher, aes(x = generation, y = success.rate), 
			   alpha = .2, position = position_jitter(height=0)) +
	geom_line(data = data_generation, aes(x = generation, y = success.mean),
			  size = 2, color = 'blue') +
	geom_hline(aes(yintercept = no_selection_mean), 
			   size = 1, color = 'red', linetype = 'dashed') +
	ylab('success rate')
success_sd_plot <- ggplot() +
	geom_line(data = data_generation, aes(x = generation, y = success.sd),
			  size = 2, color = 'blue') +
	ylab('success rate (sd)')
#plot_grid(success_plot, success_sd_plot, align='h', ncol = 1)

outputs <- plot_grid(effect_plot, success_plot, effect_sd_plot, success_sd_plot, align = 'hv')
outputs

# Uncomment to save
#save_plot('outputs.jpeg', outputs, ncol = 2, nrow = 2, base_aspect_ratio = 1.3)
