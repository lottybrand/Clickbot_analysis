
##### Timing Data 

##### go back to the beginning to get raw_clickbot with timing data intact #####

qualtrics_clickbot <- read.csv("anonymised_click_bot_abcd_May_25_2021_14.25.csv", stringsAsFactors=FALSE)

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

min(timing_clickbot$Duration[timing_clickbot$condition=="choice"])/60
min(timing_clickbot$Duration[timing_clickbot$condition=="control"])/60

max(timing_clickbot$Duration[timing_clickbot$condition=="choice"])/60
max(timing_clickbot$Duration[timing_clickbot$condition=="control"])/60

median(timing_clickbot$Duration[timing_clickbot$condition=="choice"])
median(timing_clickbot$Duration[timing_clickbot$condition=="control"])/60

p <- ggplot(timing_clickbot,aes(x=Duration))
p+ geom_histogram()

colnames(timing_clickbot)
table(timing_clickbot$timing_control_1_Page.Submit)
table(timing_clickbot$timing_cntrl1_Page.Submit)
table(timing_clickbot$timing_cntrl1_Page.Submit.1)

# looks like in the control condition it is just submit3 that recorded... check this adds up so everyone had four recordings for cntrl condition

extra_timings <- timing_clickbot[,grepl("Page.Submit.1|Page.Submit.2|Page.Submit.4", colnames(timing_clickbot))]
colnames(extra_timings)

timing_clickbot <- timing_clickbot[, !(colnames(timing_clickbot) %in% colnames(extra_timings))]

#remove rest manually
timing_clickbot[ ,c('timing_cntrl1_Page.Submit', 'timing_control_1_Page.Submit', 'timer_cntrl2_Page.Submit','timing_control2_Page.Submit','timer_cntrl3_Page.Submit','timing_control3_Page.Submit','timing_control4_Page.Submit','timing_control4_Page.Submit.4')] <- list(NULL)

#okay looks good
timing_clickbot$control_info_time <- (timing_clickbot$timing_cntrl1_Page.Submit.3 + timing_clickbot$timer_cntrl2_Page.Submit.3 + timing_clickbot$timer_cntrl3_Page.Submit.3 + timing_clickbot$timing_control4_Page.Submit.3)

#f*&^ng character class...
fckng_chars <- timing_clickbot[,grepl("timing|timer", colnames(timing_clickbot))]
col.num <- colnames(fckng_chars)
timing_clickbot[col.num] <- sapply(timing_clickbot[col.num],as.numeric)

class(timing_clickbot$timing_info_consent_Page.Submit)
# yipee 

timing_clickbot$control_info_time <- (timing_clickbot$timing_cntrl1_Page.Submit.3 + timing_clickbot$timer_cntrl2_Page.Submit.3 + timing_clickbot$timer_cntrl3_Page.Submit.3 + timing_clickbot$timing_control4_Page.Submit.3)
min(timing_clickbot$control_info_time, na.rm = TRUE)
max(timing_clickbot$control_info_time, na.rm = TRUE)
median(timing_clickbot$control_info_time, na.rm = TRUE)
median(timing_clickbot$control_info_time, na.rm = TRUE)/60

p_control <- ggplot(timing_clickbot,aes(x=control_info_time))
p_control + geom_histogram()

timing_clickbot$choice_info_time <- (timing_clickbot$timing_choice1_Page.Submit + timing_clickbot$timing_choice2_Page.Submit + timing_clickbot$timing_choice3_Page.Submit + timing_clickbot$timing_choice4_Page.Submit)
min(timing_clickbot$choice_info_time, na.rm = TRUE)
max(timing_clickbot$choice_info_time, na.rm = TRUE)
median(timing_clickbot$choice_info_time, na.rm = TRUE)
median(timing_clickbot$choice_info_time, na.rm = TRUE)/60

p_choice <- ggplot(timing_clickbot,aes(x=choice_info_time))
p_choice + geom_histogram()

choice_time <- timing_clickbot[(timing_clickbot$choice_info_time<1000),]
cntrl_time <- timing_clickbot[(timing_clickbot$control_info_time<1000),]

median(choice_time$choice_info_time, na.rm = TRUE)/60
# [1] 2.781217 median mins for choice
median(cntrl_time$control_info_time, na.rm = TRUE)/60
# [1] 3.942783 median mins for control

mean(choice_time$choice_info_time, na.rm = TRUE)/60
#[1] 3.632293 mean choice
mean(cntrl_time$control_info_time, na.rm = TRUE)/60
# [1] 4.305811 mean control

p_choice_a <- ggplot(choice_time,aes(x=choice_info_time))
p_choice_a + geom_histogram()

p_control_a <- ggplot(cntrl_time,aes(x=control_info_time))
p_control_a + geom_histogram()

# merge with clean clickbot from here (run data_processing.R)
clean_clickbot <- timing_clickbot

# okay now subset of above median for cntrl and choice, (167 for choice and 237 for cntrl )
# and minus outliers (above 1000 secs)

subset_time <- clean_clickbot[which(clean_clickbot$choice_info_time<1000 & clean_clickbot$choice_info_time > 167),]
summary(subset_time$choice_info_time)

#ugh using which helps I guess but this feels a dumb way to do it:  
subset_time2 <- clean_clickbot[which(clean_clickbot$control_info_time<1000 & clean_clickbot$control_info_time > 237),]
summary(subset_time$control_info_time)

#now merge subset_time and subset_time2 ... but we don't want this! we just want to add a column in the original dataframe denoting above median time, to compare to the rest, right? 
# now my brain has stopped for the day... continue tomorrow
