

#### processing the data for analysis ####

# can start here if haven't ran data_input.R first
# clean_clickbot <- read.csv("clean_clickbot.csv", stringsAsFactors = FALSE )


# have they had at least one dose of the vaccine yet? yes/no 1/0 
clean_clickbot$vax_yet_1 <- ifelse((clean_clickbot$vax_yet=="Yes"),1,0)

#combine vax_futureY and vax_futureN (whether they have or haven't had vaccine, would they have a (2nd) in future?)
clean_clickbot$vax_future <- paste(clean_clickbot$vax_futureN,clean_clickbot$vax_futureY)
clean_clickbot$vax_future <- as.character(clean_clickbot$vax_future)

# code vaccine hesitancy as -1 = "No do not want vaccine", 0 = "undecided", 1 = yes. 
clean_clickbot$vax_future_1 <- ifelse(grepl("Yes",clean_clickbot$vax_future),1,
                                ifelse(grepl("Undecided",clean_clickbot$vax_future),0,
                                       ifelse(grepl("No",clean_clickbot$vax_future),-1, 99)))
                                              
# answer after our experiment 
clean_clickbot$vax_future_2 <- ifelse((clean_clickbot$vax_future_2=="Yes"),1,
                                ifelse((clean_clickbot$vax_future_2=="Undecided"),0,
                                       ifelse((clean_clickbot$vax_future_2=="No"),-1,99)))

# calculate whether their decision changed after experiment (positive = positive change, 0 stayed same, neg =neg change) 
clean_clickbot$vax_change <- (clean_clickbot$vax_future_2 - clean_clickbot$vax_future_1)

# column for if positive change, 1, everything else 0
clean_clickbot$vax_positive <- ifelse((clean_clickbot$vax_change >0),1,0)


#check this works properly
#vax_att_subset <- subset(clean_clickbot, select=c("vax_future","vax_future_1","vax_future_2","vax_change","vax_positive"))

#make condition binary 
clean_clickbot$choice_cond <- ifelse((clean_clickbot$condition=="choice"),1,0)

####
#### continue processing nasty timing data here ####
####

#f*&^ng character class...
fckng_chars <- clean_clickbot[,grepl("timing|timer", colnames(clean_clickbot))]
col.num <- colnames(fckng_chars)
clean_clickbot[col.num] <- sapply(clean_clickbot[col.num],as.numeric)

# check
#class(clean_clickbot$timing_info_consent_Page.Submit)
# yipee

# duration column as numeric
clean_clickbot$Duration..in.seconds. <- as.numeric(clean_clickbot$Duration..in.seconds.)
# change its ugly Qualtrics name (what were they thinking?!)
colnames(clean_clickbot)[colnames(clean_clickbot) == 'Duration..in.seconds.'] <- 'Duration'

# calculate time looking at control info and choice info
clean_clickbot$control_info_time <- (clean_clickbot$timing_cntrl1_Page.Submit.3 + clean_clickbot$timer_cntrl2_Page.Submit.3 + clean_clickbot$timer_cntrl3_Page.Submit.3 + clean_clickbot$timing_control4_Page.Submit.3)
clean_clickbot$choice_info_time <- (clean_clickbot$timing_choice1_Page.Submit + clean_clickbot$timing_choice2_Page.Submit + clean_clickbot$timing_choice3_Page.Submit + clean_clickbot$timing_choice4_Page.Submit)

#overwrite now. 
#write.csv(clean_clickbot, file="clean_clickbot.csv", row.names=FALSE)

####
#### make all vax attitude likert ratings wide to long: 5 attitude ratings put into one Pre and one Post item ####
####

colnames(clean_clickbot)
clickbot_analysis <- reshape(clean_clickbot, idvar = "ID", 
                 varying = list(c(4,5,6,7,8),
                                c(17,18,19,20,21)),
                 v.names = c("pre_att", "post_att"), 
                 direction = "long")

clickbot_analysis$time <- ifelse((clickbot_analysis$time==1),"Safe",
                                 ifelse((clickbot_analysis$time==2),"Effective",
                                        ifelse((clickbot_analysis$time==3),"Enough_time",
                                               ifelse((clickbot_analysis$time==4),"Trust",
                                                      ifelse((clickbot_analysis$time==5),"Important",99)))))

#this column tells you what the original attitude type was
colnames(clickbot_analysis)[colnames(clickbot_analysis) == "time"] <- "att_type"

#turning the likert responses into 1-7 for pre and post att
clickbot_analysis$pre_att_1 <- ifelse((clickbot_analysis$pre_att=="Strongly Disagree"),1,
                                 ifelse((clickbot_analysis$pre_att=="Disagree"),2,
                                        ifelse((clickbot_analysis$pre_att=="Somewhat Disagree"),3,
                                               ifelse((clickbot_analysis$pre_att=="Neither Agree nor Disagree"),4,
                                                      ifelse((clickbot_analysis$pre_att=="Somewhat Agree"),5,
                                                             ifelse((clickbot_analysis$pre_att=="Agree"),6,
                                                                    ifelse((clickbot_analysis$pre_att=="Strongly Agree"),7,99)))))))

clickbot_analysis$post_att_2 <- ifelse((clickbot_analysis$post_att=="Strongly Disagree"),1,
                                      ifelse((clickbot_analysis$post_att=="Disagree"),2,
                                             ifelse((clickbot_analysis$post_att=="Somewhat Disagree"),3,
                                                    ifelse((clickbot_analysis$post_att=="Neither Agree nor Disagree"),4,
                                                           ifelse((clickbot_analysis$post_att=="Somewhat Agree"),5,
                                                                  ifelse((clickbot_analysis$post_att=="Agree"),6,
                                                                         ifelse((clickbot_analysis$post_att=="Strongly Agree"),7,99)))))))

#### now merge into one long attitude rating, so can compare pre/post vax atts ####

# remember check col names! These might've changed since the timing_analysis.R script was written. 
colnames(clickbot_analysis)
clickbot_analysis_longer <- reshape(clickbot_analysis,  
                             varying = list(c(73,74)),
                             v.names = c("attitude"), 
                             direction = "long")

#change default 'time' column name to post rating (0 = pre, 1 = post)
colnames(clickbot_analysis_longer)[colnames(clickbot_analysis_longer) == "time"] <- "post_rating"

#make binary (1 = post rating, 0 = pre rating)
clickbot_analysis_longer$post_rating <- ifelse((clickbot_analysis_longer$post_rating ==1),0,1)

#remove the default 'id' column it generates, not needed here
clickbot_analysis_longer$id <- NULL

#### transfer back to clickbot_analysis
clickbot_analysis <- clickbot_analysis_longer
rm(clickbot_analysis_longer)

#### back to clickbot_analysis



#### can jump to here in future ####

#clickbot_analysis <- write.csv(clickbot_analysis, "clickbot_analysis.csv", row.names=FALSE)


#### make engagement measures long (think we need two diff dataframes for this, check later) ####

#need to go back to clean_clickbot for this
colnames(clean_clickbot)

# four engagement measures converted to one long engagement measure
clickbot_engagement <- reshape(clean_clickbot, 
                             varying = list(c(24,25,26,27)),
                             v.names = c("engagement"), 
                             direction = "long")

#change default 'time' column name to engagement type
colnames(clickbot_engagement)[colnames(clickbot_engagement) == "time"] <- "eng_type"


#remove the default 'id' column it generates, not needed here
clickbot_engagement$id <- NULL

# name engagement type
clickbot_engagement$eng_type <- ifelse((clickbot_engagement$eng_type==1),"Engaging",
                                          ifelse((clickbot_engagement$eng_type==2),"Enjoyable",
                                             ifelse((clickbot_engagement$eng_type==3),"Confusing",
                                               ifelse((clickbot_engagement$eng_type==4),"Frustrating",99))))


# oh dear, in Qualtrics there were inconsistent capital letters. For the pilot data I'm going to adjust this here, 
# but in the real thing will need to change these back as I've fixed the Qualtrics error now. 

clickbot_engagement$engagement_1 <- ifelse((clickbot_engagement$engagement=="Strongly Disagree"),1,
                                                    ifelse((clickbot_engagement$engagement=="Disagree"),2,
                                                           ifelse((clickbot_engagement$engagement=="Somewhat Disagree"),3,
                                                                  ifelse((clickbot_engagement$engagement=="Neither Agree nor Disagree"),4,
                                                                         ifelse((clickbot_engagement$engagement=="Somewhat Agree"),5,
                                                                                ifelse((clickbot_engagement$engagement=="Agree"),6,
                                                                                       ifelse((clickbot_engagement$engagement=="Strongly Agree"),7,99)))))))

#make condition binary 
clickbot_engagement$choice_cond <- ifelse((clickbot_engagement$condition=="choice"),1,0)

# WOOPS ! Need to negative code two of them - doh! 
# will now use h_3_data to do this as easier to naviate...
# (created in analysis_script.R) with
# h_3_data <- subset(clickbot_engagement, select=c("ID","engagement_1","eng_type","choice_cond"))


#this is probably the worst way to do it, but, my brain won't do anything else right now. 
# Sorry everyone reading this code, very sorry. 

#reverse code "confusing" 
h_3_data$engagement_1 <- ifelse(((h_3_data$eng_type=="Confusing" & h_3_data$engagement_1==1)),7,
                                ifelse(((h_3_data$eng_type=="Confusing" & h_3_data$engagement_1==2)),6,
                                   ifelse(((h_3_data$eng_type=="Confusing" & h_3_data$engagement_1==3)),5,
                                        ifelse(((h_3_data$eng_type=="Confusing" & h_3_data$engagement_1==4)),4,
                                               ifelse(((h_3_data$eng_type=="Confusing" & h_3_data$engagement_1==5)),3,
                                                      ifelse(((h_3_data$eng_type=="Confusing" & h_3_data$engagement_1==6)),2,
                                                             ifelse(((h_3_data$eng_type=="Confusing" & h_3_data$engagement_1==7)),1,h_3_data$engagement_1)))))))

#reverse code "frustrating"
h_3_data$engagement_1 <- ifelse(((h_3_data$eng_type=="Frustrating" & h_3_data$engagement_1==1)),7,
                                ifelse(((h_3_data$eng_type=="Frustrating" & h_3_data$engagement_1==2)),6,
                                       ifelse(((h_3_data$eng_type=="Frustrating" & h_3_data$engagement_1==3)),5,
                                              ifelse(((h_3_data$eng_type=="Frustrating" & h_3_data$engagement_1==4)),4,
                                                     ifelse(((h_3_data$eng_type=="Frustrating" & h_3_data$engagement_1==5)),3,
                                                            ifelse(((h_3_data$eng_type=="Frustrating" & h_3_data$engagement_1==6)),2,
                                                                   ifelse(((h_3_data$eng_type=="Frustrating" & h_3_data$engagement_1==7)),1,h_3_data$engagement_1)))))))


# write to csv for now 
#clickbot_engagement <- write.csv(clickbot_engagement, "clickbot_engagement.csv", row.names=FALSE)
#clickbot_engagement <- read.csv("clickbot_engagement.csv")



