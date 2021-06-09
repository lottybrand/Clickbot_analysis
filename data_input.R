

#### Analysis for our Qualtrics Clickbot vaccination study ####
#### 

#### load qualtrics file (didn't use qualtRics package because of Prolific IDs)
# remember to select "more options" and "export viewing order data for randomised surveys" and "recode seen but unanswered questions as -99" in Qualtrics

qualtrics_clickbot <- read.csv("anonymised_click_bot_abcd_May_25_2021_14.25.csv")

key <- qualtrics_clickbot[1:3,]

##### clean ####
#remove first two rows of qualtrics rubbish
qualtrics_clickbot <-qualtrics_clickbot[3:992,]

#remove qualtrics column crap (remove unwanted timing columns separately)
colnames(qualtrics_clickbot)
qual_crap <- c("Start Date","EndDate","Status","Progress",
               "Duration..in.seconds.","Finished","RecordedDate","ResponseId",
               "DistributionChannel","UserLanguage","prolific.ID", 
               "PROLIFIC_PID")
               
raw_clickbot <- qualtrics_clickbot[, !(colnames(qualtrics_clickbot) %in% qual_crap)]

rm(qualtrics_clickbot)


###### remove timing and randomisation data now for ease, need to use grepl #####

colnames(timing_and_randomisation_columns)
# from stackoverflow: data[,grepl("search_string", colnames(data))]
timing_columns <- raw_clickbot[,grepl("tim", colnames(raw_clickbot))]

randomisation_colnames <- c("FL_3_DO","Block1_Cntrl_DO","Block2_Cntrl_DO","Block3_Cntrl_DO","Block4_Cntrl_DO")
randomisation_columns <- raw_clickbot[, (colnames(raw_clickbot) %in% randomisation_colnames)]

timing_colnames <- colnames(timing_columns)

clean_clickbot <- raw_clickbot[, !(colnames(raw_clickbot) %in% timing_colnames)]
clean_clickbot <- clean_clickbot[, !(colnames(clean_clickbot) %in% randomisation_colnames)]

###### remove data that is not complete / was not approved in Prolific #####

clean_clickbot <- clean_clickbot[(clean_clickbot$condition=="control"|clean_clickbot$condition=="choice"),]

table(clean_clickbot$CONSENT)
colnames(clean_clickbot)
table(clean_clickbot$prolif_check)

any_empty <- ifelse((clean_clickbot$ITT_1_A ==""),clean_clickbot$ID,"OK")
table(any_empty)
empties <- c(303,392,394,409,479,527,946,955,962,971,976)
clean_clickbot <- clean_clickbot[!(clean_clickbot$ID%in%empties),]

colnames(clean_clickbot)
#write.csv(clean_clickbot, file="clean_clickbot.csv", row.names=FALSE)

##### while I'm here I'd like to save the comments separately ##### 

comment_columns <- c("prolif_check","discuss_1","read_1","age","gender","gender_4_TEXT","edu","anything_else","condition","clicked_t","clicked_f")
comments_and_demogs <- clean_clickbot[, (colnames(clean_clickbot) %in% comment_columns)]
comments_and_demogs <- comments_and_demogs[(comments_and_demogs$anything_else!=-99),]

no_comments <- c("none","None","na","n/a","NA","N/A","NO","no","No","nope","Nope","None")
nopes <- comments_and_demogs[(comments_and_demogs$anything_else %in% no_comments),]

comments_and_demogs <- comments_and_demogs[!(comments_and_demogs$anything_else%in%no_comments),]
write.csv(comments_and_demogs, file="comments_and_demogs.csv", row.names=FALSE)

