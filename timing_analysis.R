
##### Timing Data 

##### go back to the beginning to get raw_clickbot with timing data intact #####

qualtrics_clickbot <- read.csv("anonymised_click_bot_abcd_May_25_2021_14.25.csv")

key <- qualtrics_clickbot[1:3,]

#clean
#remove first two rows of qualtrics rubbish
qualtrics_clickbot <-qualtrics_clickbot[3:992,]

#remove qualtrics column crap (remove unwanted timing columns separately) but keep duration this time
colnames(qualtrics_clickbot)
qual_crap <- c("Status","Progress",
               "Finished","RecordedDate","ResponseId",
               "DistributionChannel","UserLanguage","prolific.ID", 
               "PROLIFIC_PID")

raw_clickbot <- qualtrics_clickbot[, !(colnames(qualtrics_clickbot) %in% qual_crap)]

rm(qualtrics_clickbot)


raw_clickbot <- raw_clickbot[(raw_clickbot$condition=="control"|raw_clickbot$condition=="choice"),]

colnames(raw_clickbot)

raw_clickbot$ID <- 1:(nrow(raw_clickbot))

any_empty <- ifelse((raw_clickbot$ITT_1_A ==""),raw_clickbot$ID,"OK")
table(any_empty)
empties <- any_empty[!any_empty=="OK"]
raw_clickbot <- raw_clickbot[!(raw_clickbot$ID %in% empties),]

colnames(raw_clickbot)
unwanted_timings <- raw_clickbot[,grepl("Last.Click|First.Click|Click.Count", colnames(raw_clickbot))]
colnames(unwanted_timings)

timing_clickbot <- raw_clickbot[, !(colnames(raw_clickbot) %in% colnames(unwanted_timings))]

##### look at overall timing ####

timing_clickbot$Duration..in.seconds. <- as.numeric(timing_clickbot$Duration..in.seconds.)
# lets change the name
colnames(timing_clickbot)[colnames(timing_clickbot) == 'Duration..in.seconds.'] <- 'Duration'

overall_time <- tapply(timing_clickbot$Duration, list(timing_clickbot$condition),mean, na.rm=TRUE)
overall_time

min(timing_clickbot$Duration[timing_clickbot$condition=="choice"])
min(timing_clickbot$Duration[timing_clickbot$condition=="control"])

# NOPE something very wrong here. Duration column starts at 3 secs?!... but that consent page submit has 36 seconds... 
# need to ignore this, maybe compare to the start end times... but for now lets sum the page submits we have, maybe? 


summary(clean_clickbot$Q4)
