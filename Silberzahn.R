library(cowplot)

datafile = 'Crowdsourcing Effects in OR.csv'
if (!file.exists(datafile)) {
	download.file('https://osf.io/fa743/?action=download&version=2', 
				  destfile = datafile)
}

dataf.cs = read.csv(datafile)

## Convert odds ratios into Cohen's d
dataf.cs$d = log(dataf.cs$OR) * sqrt(3) / pi
dataf.cs$d_lo = log(dataf.cs$OR_lo) * sqrt(3)/pi
dataf.cs$d_hi = log(dataf.cs$OR_hi) * sqrt(3)/pi

## Plot
ggplot2::ggplot(dataf.cs, aes(x = reorder(Team, d), y = d)) + geom_point() + 
	geom_errorbar(aes(ymin = d_lo, ymax = d_hi))

## sigma_res = sqrt(var(mu) - 1)
sd(dataf.cs$d)
