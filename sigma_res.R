library(cowplot)
library(dplyr)
library(readxl)

## Use the Many Labs data to estimate sigma_res

## If necessary, retrieve data file
datafile = 'ML-_Summary_Statistics.xlsx'
if (!file.exists(datafile)) {
	download.file('https://osf.io/ihane/?action=download&version=1', 
				  destfile = datafile)
}

## Define a function to parse an Excel sheet of interest
read_study = function(sheet_num) {
	temp_data = read_excel(datafile, sheet = sheet_num)
	mean_w = temp_data[[1, 'ES (from means)']]
	temp_data = temp_data[-(1:2), c('Site', 'ES (from means)')]
	temp_data$study = sheet_num
	#print(mean_w)
	temp_data$mean_w = mean_w
	return(temp_data)
}
## Apply this to all of the sheets of interest
	## NB 14 uses correlations instead of Cohen's d
dataf = lapply(c(3:13, 15:18), read_study) %>% 
	## Put them together
	do.call('rbind', .)
## Give effect size a less clunky name
dataf = dataf %>% rename(es = `ES (from means)`)
## Remove rows with NAs
dataf = dataf[complete.cases(dataf),]

# dataf %>% 
# 	## Within each study, calculate z-scores and weighted-mean-adjusted effect sizes
# 	group_by(study) %>%
# 	mutate(es_z = scale(es), 
# 		   es_c = es - mean_w) %>%
# 	## For each site, calculate mean effect score as an estimate of this site's mu_res
# 	ungroup() %>% group_by(Site) %>%
# 	summarize_each_(funs(mean), c('es', 'es_z', 'es_c')) %>% 
# 	## Calculate the standard deviation of these means as an estimate of sigma_res
# 	ungroup() %>% 
# 	select(-Site) %>%
# 	summarize_each(funs(sd)) %>%
# 	rename(`sd(es)` = es, `sd(es_z)` = es_z, `sd(es_c)` = es_c)

## The simulation uses sigma = .25, so the Cohen's d values need to be rescaled 
##  to get observed values of mu
sigma = .25
dataf$mu = dataf$es / sigma

dataf %>% 
	## Within each study, calculate z-scores and weighted-mean-adjusted effect sizes
	group_by(study) %>%
	mutate(mu_z = scale(mu), 
		   mu_c = mu - 4 * mean_w) %>%
	## For each site, calculate mean effect score as an estimate of this site's mu_res
	ungroup() %>% group_by(Site) %>%
	summarize_each_(funs(mean), c('mu', 'mu_z', 'mu_c')) %>% 
	rename(mu_res = mu, mu_res_z = mu_z, mu_res_c = mu_c) %>%
	## Calculate the standard deviation of these means as an estimate of sigma_res
	ungroup() %>% 
	select(-Site) %>%
	summarize_each(funs(sd)) %>%
	rename(sigma_res = mu_res, sigma_res_z = mu_res_z, sigma_res_c = mu_res_c)