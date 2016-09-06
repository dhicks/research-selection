'
`research_selection.R`

This R script runs a small simulation of a research selection process that
can produce a sponsorship effect without bias or research misconduct.  

For discussion see here:  
http://jefais.tumblr.com/post/133751046358/sponsorship-effects-without-bias
'

library(cowplot)
library(doParallel)
	cl = makeCluster(4, outfile = '')
	registerDoParallel(cl)
library(dplyr)
# library(tikzDevice)
# 	options(tikzDocumentDeclaration = "\\documentclass{philpaper}\n", 
# 			tikzDefaultEngine = 'luatex')

n_researchers <- 100		# no. researchers in each year
n_years <- 30				# no. years to run each simulation
perc_replace <- .05			# fraction of researchers to replace in each year

n_experiments <- 30			# no. experiments run by each researcher in each year
sigma_mu <- .35				# standard deviation for true values of mu in experiments
sigma <- 1					# standard deviation for noise in experiments
alpha <- .05				# statistical significance threshold
## sample size in each experiment
## calculated by the sample needed for 80% power to detect a difference of .2 \sigma
n = power.t.test(sd = sigma, sig.level = alpha, 
				 ## desired power to detect effect >= delta
				 delta = .2 * sigma, power = .8, 
				 type = 'one.sample', alternative = 'one.sided') %>%
	.$n %>% ceiling()

#rho <- .25					# ratio between researcher effect sd and sigma_mu
							#  ie, how wide is range of researcher effects, compared 
							#  to the range of experimental effects
#tau <- 1 / (1 + rho^2)		# fraction of Var(mu) due to variance in researcher effects
rho_range <- 2 * 0:5 / 10   # vector with values of rho to test
n_rep <- 15					# no. repetitions of the simulation to run at each value of rho


# Set a seed for the simulation
set.seed(9876543)


new_researchers <- function(rho, n = perc_replace * n_researchers) {
	'
	Generate a set of n new researchers
	'
	# Use the next two lines for a uniform distribution
	#bound_effect <- 3.33		# positive bound on uniform distribution for researcher effect
	#return(runif(perc_replace*n_researchers, min = -bound_effect, max = bound_effect))
	# Use the next three lines for a Gaussian distribution
	return(rnorm(n, mean = 0, sd = rho * sigma_mu))
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
		mu <- rnorm(1, mean = researcher_effect, sd = sigma_mu)
		obs <- rnorm(n, mean = mu, sd = sigma)
		p <- t.test(obs, alternative = 'greater')$p.value
		results <- c(results, p <= alpha)
	}
	return(sum(results)/n_experiments)
}

no_selection <- function() {
	'
	Estimate the no-replacement success rate
	'
	no_selection <- c()
	researchers <- new_researchers(rho, n_researchers)
	for (i in 1:n_years) {
		for (researcher in researchers) {
			no_selection <- c(no_selection, run_experiments(researcher))
		}
	}
	no_selection_mean <- mean(no_selection)
	return(no_selection_mean)
}


# Estimate the no-selection success rate
#no_selection_mean <- no_selection()

# Initialize the data frames to hold the data
# The first one holds the researcher-level data. 
#  This is long: each row has data for a single researcher in a single year
data_researcher <- data.frame(year = c(), effect = c(), success.rate = c(), rho = c())
# This one holds the year-level data.
#  Each row has data for a single year
data_year <- data.frame(year = c(), 
							   effect.mean = c(), effect.sd = c(),
							   success.mean = c(), success.sd = c(),
							   rho = c(), rep = c())

# Run the simulation:
ptime = system.time({
	data_researcher = foreach (rho=rho_range, .combine = rbind) %:% 
		foreach(rep = 1:n_rep, .combine = rbind) %dopar% {
			## Print a status update to the console
			print(paste('rho: ', rho, '; rep ', rep, sep = ''))
			data_researcher_this_rep = data.frame(year = c(), effect = c(), success.rate = c(), rho = c())
			# year 1 researchers
			researchers <- new_researchers(rho, n_researchers)
			# For each year, 
			for (gen in 1:n_years) {
				success_gen <- c()
				data_researcher_this_year = data.frame()
				# For each researcher, 
				for (researcher_effect in researchers) {
					# Run the experiments and get the success rate
					success <- run_experiments(researcher_effect)
					# success_gen <- c(success_gen, success)
					# Add the effect and success rates to the data_researcher frame
					new_row <- data.frame(year = gen, 
										  effect = researcher_effect, 
										  success.rate = success,
										  rho = rho, 
										  rep = rep)
					data_researcher_this_year <- rbind(data_researcher_this_year, new_row)
				}
				data_researcher_this_rep = rbind(data_researcher_this_rep, data_researcher_this_year)

				# Researchers to keep
				researchers <- sort(researchers)
				keep <- researchers[(perc_replace*n_researchers+1) : n_researchers]
				# New researchers
				new <- new_researchers(rho)
				researchers <- c(new, keep)
			}
			data_researcher_this_rep
		}
})

## How long did the simulation take? 
ptime

## Calculate year-level statistics
data_year = data_researcher %>% group_by(year, rho, rep) %>% 
	summarize(effect.mean = mean(effect), 
			  effect.sd = sd(effect), 
			  success.mean = mean(success.rate), 
			  success.sd = sd(success.rate))
## Success rates relative to rho = 0
baseline = data_year %>% filter(rho == 0) %>% .$success.mean %>% mean
data_year = data_year %>% mutate(success.relative = success.mean / baseline)

save(data_researcher, data_year, file = paste(Sys.Date(), '.Rdata', sep = ''))

# ------------------------------
# Plot results for the entire simulation
# Effect
effect_plot <- ggplot() + 
	aes(x = year, y = effect.mean, 
		color = rho, group = interaction(rho, rep)) +
	scale_color_continuous(guide = FALSE) +
# 	geom_point(data = data_researcher, aes(y = effect),
# 			   alpha = .1, position = position_jitter(height=0)) +
	geom_line(data = data_year, size = 1, alpha = .2) +
	geom_smooth(data = data_year, aes(group = rho), size = 2) +
	ylab('researcher effect')
effect_plot = directlabels::direct.label(effect_plot)
	
effect_sd_plot <- ggplot() +
	aes(x = year, y = effect.sd, 
		color = rho, group = interaction(rho, rep)) +
	scale_color_continuous(guide = FALSE) +
	geom_line(data = data_year, size = 1, alpha = .2) +
	geom_smooth(data = data_year, aes(group = rho), size = 2) +
	ylab('researcher effect (sd)')
effect_sd_plot = directlabels::direct.label(effect_sd_plot)
#plot_grid(effect_plot, effect_sd_plot, align='h', ncol = 1)

# Success rate
success_plot <- ggplot() +
	aes(x = year, y = success.mean, 
		color = rho, group = interaction(rho, rep)) +
	scale_color_continuous(guide = FALSE) +
#  	geom_point(data = data_researcher, aes(y = success.rate), 
#  			   alpha = .1, position = position_jitter(height=0)) +
	geom_line(data = data_year, size = 1, alpha = .2) +
	geom_smooth(data = data_year, aes(group = rho), size = 2) +
#	geom_hline(aes(yintercept = no_selection_mean), 
#			   size = 1, color = 'red', linetype = 'dashed') +
	geom_hline(yintercept = baseline, size = 1, color = 'red', linetype = 'dashed') +
	ylab('success rate') + scale_y_continuous(labels=scales::percent)
success_plot = directlabels::direct.label(success_plot)
success_sd_plot <- ggplot() +
	aes(x = year, y = success.sd, 
		color = rho, group = interaction(rho, rep)) +
	scale_color_continuous(guide = FALSE) +
	geom_line(data = data_year, size = 1, alpha = .2) +
	geom_smooth(data = data_year, aes(group = rho), size = 2) +
	ylab('success rate (sd)')
success_sd_plot = directlabels::direct.label(success_sd_plot)
#plot_grid(success_plot, success_sd_plot, align='h', ncol = 1)

# Relative success rate
success_rel_plot <- ggplot() +
	aes(x = year, y = success.relative, 
		color = rho, group = interaction(rho, rep)) +
	scale_color_continuous(guide = FALSE) +
	#  	geom_point(data = data_researcher, aes(y = success.rate), 
	#  			   alpha = .1, position = position_jitter(height=0)) +
	geom_line(data = data_year, size = 1, alpha = .2) +
	geom_smooth(data = data_year, aes(group = rho), size = 2) +
	#	geom_hline(aes(yintercept = no_selection_mean), 
	#			   size = 1, color = 'red', linetype = 'dashed') +
	geom_hline(yintercept = 1, size = 1, color = 'red', linetype = 'dashed') +
	ylab('relative success rate') + scale_y_continuous(breaks = seq(0, 2, by = .5))
success_rel_plot = directlabels::direct.label(success_rel_plot)

# outputs <- plot_grid(effect_plot, success_plot, effect_sd_plot, success_sd_plot, 
# 					 align = 'hv', labels = c('A', 'B', 'C', 'D'))
outputs <- plot_grid(effect_plot, success_plot, success_rel_plot,
					 align = 'hv', nrow = 1, labels = c('A', 'B', 'C'))

# Uncomment to save as tikz
# tikz(file = 'outputs.tex', height = 3.5, standAlone = TRUE)
outputs
# dev.off()

# Uncomment to save as png
# save_plot('outputs.png', outputs, ncol = 3, nrow = 1, base_aspect_ratio = 1)


# ------------------------------
# Plot results for one run with rho = .4
data_year_filter <- data_year %>% filter(rho == .4, rep == 1)
data_res_filter <- data_researcher %>% filter(rho == .4, rep == 1)
# Effect
effect_plot_filter <- ggplot() + 
	aes(x = year, y = effect.mean, 
		color = rho, group = interaction(rho, rep)) +
	scale_color_continuous(guide = FALSE) +
	geom_point(data = data_res_filter, aes(y = effect),
			   alpha = .1, position = position_jitter(height=0)) +
	geom_line(data = data_year_filter, size = 2, alpha = 1) +
# 	geom_smooth(data = filter(data_year, rho == .25), 
# 				aes(group = rho), size = 2) +
	# Dashed line indicating the mean researcher effect when rho = 0
	geom_hline(aes(yintercept = 
				   	mean(filter(data_year, rho == 0)$effect.mean)), 
			   size = 1, color = 'red', linetype = 'dashed') +
	ylab('researcher effect')

effect_sd_plot_filter <- ggplot() +
	aes(x = year, y = effect.sd, 
		color = rho, group = interaction(rho, rep)) +
	scale_color_continuous(guide = FALSE) +
	geom_line(data = data_year_filter, size = 2, alpha = 1) +
	geom_smooth(data = filter(data_year, rho == .4), 
				aes(group = rho), size = .5) +
	ylab('researcher effect (sd)')
#plot_grid(effect_plot, effect_sd_plot, align='h', ncol = 1)

# Success rate
success_plot_filter <- ggplot() +
	aes(x = year, y = success.mean, 
		color = rho, group = interaction(rho, rep)) +
	scale_color_continuous(guide = FALSE) +
 	geom_point(data = data_res_filter, aes(y = success.rate), 
 			   alpha = .1, position = position_jitter(height=0)) +
	geom_line(data = data_year_filter, size = 2, alpha = 1) +
	#geom_smooth(data = filter(data_year, rho == .25), aes(group = rho), size = 2) +
	# Dashed line indicating the mean researcher effect when rho = 0
	geom_hline(aes(yintercept = baseline), 
			   size = 1, color = 'red', linetype = 'dashed') +
	ylab('success rate') + scale_y_continuous(labels=scales::percent)
success_sd_plot_filter <- ggplot() +
	aes(x = year, y = success.sd, 
		color = rho, group = interaction(rho, rep)) +
	scale_color_continuous(guide = FALSE) +
	geom_line(data = data_year_filter, size = 2, alpha = 1) +
	geom_smooth(data = filter(data_year, rho == .4), 
				aes(group = rho), size = .5) +
	ylab('success rate (sd)')
#plot_grid(success_plot, success_sd_plot, align='h', ncol = 1)

# Relative success rate
success_rel_plot_filter <- ggplot() +
	aes(x = year, y = success.relative, 
		color = rho, group = interaction(rho, rep)) +
	scale_color_continuous(guide = FALSE) +
 	geom_point(data = data_res_filter, aes(y = success.rate/baseline),
 			   alpha = .1, position = position_jitter(height=0)) +
	geom_line(data = data_year_filter, size = 2, alpha = 1) +
	#geom_smooth(data = data_year, aes(group = rho), size = 2) +
	#	geom_hline(aes(yintercept = no_selection_mean), 
	#			   size = 1, color = 'red', linetype = 'dashed') +
	geom_hline(yintercept = 1, size = 1, color = 'red', linetype = 'dashed') +
	ylab('relative success rate') + scale_y_continuous(breaks = seq(0, 2, by = .5))

# outputs_filter <- plot_grid(effect_plot_filter, success_plot_filter, 
# 					 effect_sd_plot_filter, success_sd_plot_filter, 
# 					 align = 'hv', labels = c('A', 'B', 'C', 'D'))
outputs_filter <- plot_grid(effect_plot_filter, success_plot_filter, success_rel_plot_filter, 
							align = 'hv', nrow = 1, labels = 'AUTO')

# Uncomment to save as tikz
# tikz(file = 'outputs-filter.tex', height = 3.5, standAlone = TRUE)
outputs_filter
# dev.off()

# Uncomment to save as png
# save_plot('outputs_filter.png', outputs_filter, ncol = 3, nrow = 1, base_aspect_ratio = 1)

# combined <- plot_grid(effect_plot_filter, success_plot_filter, effect_plot, success_plot, 
# 					  nrow = 2, align = 'hv', labels = c('A', 'B', 'C', 'D'))
# combined
#save_plot('combined.png', combined, ncol = 2, nrow = 2, base_aspect_ratio = 1)


## In each year, how many researchers have success rates >= 80%? 
# ggplot(data = {data_res_filter %>% group_by(year) %>% 
# 	   			summarize(high.success = sum(success.rate >= .8))}, 
# 	   aes(year, high.success)) + 
# 		geom_bar(stat = 'identity') + geom_smooth()

## ----------
## Calculation for footnote 6
r_tot = n_researchers + (perc_replace * n_researchers * n_years)
t_thresh = 1
p_thresh = .01
uniroot(function(rho) {1 - pnorm(t_thresh, sd = rho * sigma_mu)**r_tot - p_thresh}, c(0, 1))
