# Load in the nsw_pscore data from the do data file
# Install the haven library to gain access to the read_dta function
library(haven)
# Load data
nsw_pscore = read_dta("https://github.com/scunning1975/mixtape/raw/master/nsw_mixtape.dta")

h