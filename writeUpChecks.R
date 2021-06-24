
### write up checks

clean_clickbot <- read.csv("clean_clickbot.csv")

colnames(clean_clickbot)

table(clean_clickbot$vax_positive)

# need to go back to timing_analysis.R script to get clean_clickbot$most_time

table(clean_clickbot$vax_positive, clean_clickbot$most_time)

table(clean_clickbot$prolif_check, clean_clickbot$vax_positive)


clickbot_analysis <- read.csv("clickbot_analysis.csv")
colnames(clickbot_analysis)

