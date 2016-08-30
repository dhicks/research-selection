library(dplyr)
library(stringr)

## Annual involuntary employee turnover rate for durable goods manufacturing

## Get data
datafile = 'jt.data.6.LayoffsDischarges'
if (!file.exists(datafile)) {
	download.file(url = 'http://download.bls.gov/pub/time.series/jt/jt.data.6.LayoffsDischarges', 
				  destfile = datafile)
}

## Load data
dataf = read.table(datafile, sep = '\t', header = TRUE)

## Filter down to durable goods manufacturing
dataf %>% filter(str_detect(series_id, '32000000'), 
				 		## rates, not levels
				 		str_detect(series_id, 'LDR'), 
				 		## annual totals
				 		period == 'M13') %>% 
	.$value %>% 
	summary