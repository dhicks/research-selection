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

## The simulation uses sigma = .25, so the Cohen's d values need to be rescaled 
##  to get observed values of mu
sigma = .25
dataf$mu = dataf$es / sigma

## ----------
## Pull apart site and study effects using a multilevel linear model
fit = lmer(mu ~ (1 | site) + (1 | study), dataf)
sd(coef(fit)$site$`(Intercept)`)

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
		vector<lower = -5, upper = 15>[n_studies] mu_study;
		
		real<lower = 0> sigma_site;
	}
	model { 
		mu ~ normal(mu_site[site] + mu_study[study], 1);
		mu_site ~ normal(0, sigma_site);
	}
'

stanfit = stan(model_code = model_string, 
			   data = list(n_sites = max(dataf$site.code), 
			   			n_studies = max(dataf$study), 
			   			N = nrow(dataf),
			   			site = dataf$site.code, 
			   			study = dataf$study, 
			   			mu = dataf$mu), 
			   chains = 4, 
			   verbose = TRUE
			   )

stanfit
## Check correlations
# as.data.frame(stanfit) %>% cor %>% reshape2::melt() %>% filter(abs(value) > .3, value < 1)
# samples = as.data.frame(stanfit)
# names(samples) = make.names(names(samples))
# GGally::ggpairs(data = samples,
# 				columns = c('sigma_site', 'mu_site.7.', 'mu_site.19.', 'mu_site.29.'))
# 
# plot(stanfit, pars = 'mu_study')
# plot(stanfit, pars = 'mu_site')


plot(stanfit, pars = 'sigma_site')
# summary(stanfit)$summary['sigma_site',]
sigma_site = as.data.frame(stanfit) %>% .$sigma_site
summary(sigma_site)

## ----------
## Compare the lmer and stan fits
lmer.site = coef(fit)$site$`(Intercept)`
stan.site = broom::tidy(stanfit) %>% filter(stringr::str_detect(term, 'mu_site')) %>%
			.$estimate
lm(lmer.site ~ stan.site) %>% summary
ggplot(data = data.frame(lmer.site = lmer.site, stan.site = stan.site), 
	   aes(lmer.site, stan.site)) + 
	geom_point()

## Perfect correlation, but the lmer values are greater than the Stan values, 
##  and don't vary as widely


lmer.study = coef(fit)$study$`(Intercept)`
stan.study = broom::tidy(stanfit) %>% filter(stringr::str_detect(term, 'mu_study')) %>%
			.$estimate
lm(lmer.study ~ stan.study) %>% summary
ggplot(data = data.frame(lmer.study = lmer.study, stan.study = stan.study), 
	   aes(lmer.study, stan.study)) + 
	geom_point()

## These are also perfectly correlated, with the lmer values also greater than the Stan values
##  But unlike with site effects, the study effects have the same variance

