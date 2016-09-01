library(cowplot)
library(dplyr)
library(lme4)
library(readxl)
library(rstan)
	rstan_options(auto_write = TRUE)
	options(mc.cores = parallel::detectCores())

## Use the Many Labs data to estimate sigma_res

## ----------
## Load data
## If necessary, retrieve data file
datafile = 'ML-_Summary_Statistics.xlsx'
if (!file.exists(datafile)) {
	download.file('https://osf.io/ihane/?action=download&version=1', 
				  destfile = datafile)
}

## Define a function to parse a single Excel sheet
read_study = function(sheet_num) {
	temp_data = read_excel(datafile, sheet = sheet_num)
	names(temp_data) = make.names(names(temp_data), unique = TRUE)
	mean_w = temp_data[[1, 'ES..from.means.']]
	temp_data = temp_data %>% 
		## Remove summary rows
		filter(Site != 'Overall:', Site != 'Mean across samples:', 
			   Site != 'Sum across samples:', 
			   Site != 'Overall (sum of samples)', 
			   Site != 'Overall for US participants:') %>%
		## Give effect size a less clunky name
		select(site = Site, es = ES..from.means.) %>%
		## Include study-level ID and weighted mean
		mutate(study = sheet_num, 
			   mean_w = mean_w)
	return(temp_data)
}
## Apply this to all of the sheets
	## NB #14 uses correlations instead of Cohen's d
dataf = lapply(c(3:13, 15:18), read_study) %>% 
	## Put them together
	do.call('rbind', .)
## Remove rows with NAs
dataf = dataf[complete.cases(dataf),]
## Renumber studies
dataf$study = match(dataf$study, unique(sort(dataf$study)))

## ----------
## Pull apart site and study effects using a random effects model
fit = lmer(es ~ -1 + (1 | site) + (1 | study), dataf)
ggplot(data = data.frame(es = dataf$es, 
						 es_hat = predict(fit, dataf)), 
	   aes(es, es_hat)) + geom_point()
## sigma_site
sd(coef(fit)$site$`(Intercept)`)
## sigma_mu
sd(residuals(fit))
## rho
sd(coef(fit)$site$`(Intercept)`) / sd(residuals(fit))
## Confidence intervals
confint(fit)

## ----------
## A more sophisticated Bayesian model using Stan
dataf$site.code = as.numeric(as.factor(dataf$site))

model_string = '
	data {
		int n_sites;
		int n_studies;
		int N;
		
		int site[N];
		int study[N];
		vector[N] mu;
	}
	parameters {
		vector[n_sites] mu_site;
		vector[n_studies] mu_study;
		
		real<lower = 0> sigma_site;
        real<lower = 0> sigma_mu;
	}
    transformed parameters {
        vector[N] mu_hat;
        real<lower = 0> rho;
        mu_hat = mu_site[site] + mu_study[study];
        rho = sigma_site / sigma_mu;
    }
	model {
        mu ~ normal(mu_hat, sigma_mu);
        mu_site ~ normal(0, sigma_site);
	}
'

stanfit = stan(model_code = model_string, 
			   data = list(n_sites = max(dataf$site.code), 
			   			n_studies = max(dataf$study), 
			   			N = nrow(dataf),
			   			site = dataf$site.code, 
			   			study = dataf$study, 
			   			mu = dataf$es), 
			   chains = 4,
			   iter = 4000,
			   verbose = TRUE, 
			   seed = 1238657472
			   )
# stanfit
## Print the posterior results, w/o all the mu_hats
print(stanfit, include=FALSE, pars = 'mu_hat', probs = c(.2, .5, .8))

## Some diagnostic plots
# stan_dens(stanfit, 'sigma_site') + facet_wrap('chain')
# traceplot(stanfit, 'sigma_site') + facet_wrap('chain')
# stan_ac(stanfit, 'sigma_site')
# 
# stan_dens(stanfit, 'lp__') + facet_wrap('chain')
# traceplot(stanfit, 'lp__') + facet_wrap('chain')
# stan_ac(stanfit, 'lp__')

## Correlations
# as.data.frame(stanfit) %>% select(-starts_with('mu_hat')) %>% cor %>% reshape2::melt() %>% filter(abs(value) > .5, value < 1)
# samples = as.data.frame(stanfit)
# names(samples) = make.names(names(samples))
# GGally::ggpairs(data = samples,
# 				columns = c('sigma_site', 'mu_site.7.', 'mu_site.19.', 'mu_site.29.'))

## Study and site estimates
# plot(stanfit, pars = 'mu_study')
# plot(stanfit, pars = 'mu_site')

## Summaries for the parameters of interest
plot(stanfit, pars = c('sigma_site', 'sigma_mu', 'rho'))
summary(stanfit, pars = c('sigma_site', 'sigma_mu', 'rho'), probs = c(.2, .5, .8)) %>%
	.$summary %>% as.data.frame %>% 
	select(mean, `20%`, `50%`, `80%`) %>%
	knitr::kable(digits = 2)

## ----------
## Compare the lmer and stan fits
##  The two models agree to a high level of precision
##  Point estimates for Bayes model are means of posterior distributions

lmer.site = coef(fit)$site$`(Intercept)`
stan.site = broom::tidy(stanfit) %>% filter(stringr::str_detect(term, 'mu_site')) %>%
			.$estimate
lm(lmer.site ~ stan.site) %>% summary
ggplot(data = data.frame(lmer.site, stan.site), aes(lmer.site, stan.site)) + 
	geom_point()

lmer.study = coef(fit)$study$`(Intercept)`
stan.study = broom::tidy(stanfit) %>% filter(stringr::str_detect(term, 'mu_study')) %>%
			.$estimate
lm(lmer.study ~ stan.study) %>% summary
ggplot(data = data.frame(lmer.study, stan.study), aes(lmer.study, stan.study)) + 
	geom_point()

lmer.mu = predict(fit, dataf)
stan.mu = broom::tidy(stanfit) %>% filter(stringr::str_detect(term, 'mu_hat')) %>%
			.$estimate
lm(lmer.mu ~ stan.mu) %>% summary
ggplot(data = data.frame(lmer.mu, stan.mu), 
	   aes(lmer.mu, stan.mu)) + 
	geom_point()

## Observations vs. predictions
ggplot(data = {data.frame(obs = dataf$es, lmer = lmer.mu, stan = stan.mu) %>% 
		reshape2::melt(id.vars = 'obs', variable.name = 'model', value.name = 'fitted')}, 
		aes(obs, fitted, color = model)) +
	geom_point() + facet_grid(~ model)

