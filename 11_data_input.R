
####
#### This file inputs the data from our raw Qualtrics file for analysis of our Clickbot vaccination study ####
#### 

# load qualtrics file (didn't use qualtRics package because of Prolific IDs) so have to anonymise these manually and not upload to github!
# remember to select "more options" and "export viewing order data for randomised surveys" and "recode seen but unanswered questions as -99" in Qualtrics

# load full raw Qualtrics output 
qualtrics_clickbot <- read.csv("data/anonymised_click_bot_abcd_May_25_2021_14.25.csv", stringsAsFactors=FALSE)

# these top three rows provide a key to what the variables are (sort of)
key <- qualtrics_clickbot[1:3,]

# remove first two rows of qualtrics rubbish
qualtrics_clickbot <-qualtrics_clickbot[3:992,]

#remove qualtrics column stuff we're not interested in (remove unwanted timing columns separately)
colnames(qualtrics_clickbot)
qual_crap <- c("Status","Progress",
               "Finished","RecordedDate","ResponseId",
               "DistributionChannel","UserLanguage","prolific.ID", 
               "PROLIFIC_PID")
               
qualtrics_clickbot <- qualtrics_clickbot[, !(colnames(qualtrics_clickbot) %in% qual_crap)]
rm(qual_crap)

# remove all the timing columns we're not interested in: 
unwanted_timings <- qualtrics_clickbot[,grepl("Last.Click|First.Click|Click.Count|Page.Submit.1|Page.Submit.2|Page.Submit.4", colnames(qualtrics_clickbot))]
qualtrics_clickbot <- qualtrics_clickbot[, !(colnames(qualtrics_clickbot) %in% colnames(unwanted_timings))]
rm(unwanted_timings)

# still need to move these ones manually: 
qualtrics_clickbot[ ,c('timing_cntrl1_Page.Submit', 'timing_control_1_Page.Submit', 'timer_cntrl2_Page.Submit','timing_control2_Page.Submit','timer_cntrl3_Page.Submit','timing_control3_Page.Submit','timing_control4_Page.Submit','timing_control4_Page.Submit.4')] <- list(NULL)

##### remove data that is not complete / was not approved in Prolific #####

raw_clickbot <- qualtrics_clickbot[(qualtrics_clickbot$condition=="control"|qualtrics_clickbot$condition=="choice"),]

# remove any that have empty ITT responses (these are people who did not complete the survey properly)
raw_clickbot <- raw_clickbot[!(raw_clickbot$ITT_1_A==""),]

# add IDs for this clean dataset: 
raw_clickbot$ID <- 1:(nrow(raw_clickbot))

rm(qualtrics_clickbot)

# save raw_clickbot for future
#write.csv(raw_clickbot, file="raw_clickbot.csv", row.names=FALSE)

# create clean_clickbot ready for processing in data_processing.R
#clean_clickbot <- raw_clickbot

####
#### save the comments and their demogs in a separate file ##### 
####

# should come back to this

# comment_columns <- c("prolif_check","discuss_1","read_1","age","gender","gender_4_TEXT","edu","anything_else","condition","clicked_t","clicked_f")
# comments_and_demogs <- clean_clickbot[, (colnames(clean_clickbot) %in% comment_columns)]
# comments_and_demogs <- comments_and_demogs[(comments_and_demogs$anything_else!=-99),]
# 
# no_comments <- c("none","None","na","n/a","NA","N/A","NO","no","No","nope","Nope","None")
# nopes <- comments_and_demogs[(comments_and_demogs$anything_else %in% no_comments),]
# 
# comments_and_demogs <- comments_and_demogs[!(comments_and_demogs$anything_else%in%no_comments),]
# write.csv(comments_and_demogs, file="comments_and_demogs.csv", row.names=FALSE)

