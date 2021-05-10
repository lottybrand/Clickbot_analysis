#### processing the data for analysis ####


# create neat ppt IDs

raw_clickbot$ID <- 1:(nrow(raw_clickbot))

# transforming words to numbers! 

raw_clickbot$vax_yet_1 <- ifelse((raw_clickbot$vax_yet=="Yes"),1,0)


#combine vax_futureY and vax_futureN
raw_clickbot$vax_future <- paste(raw_clickbot$vax_futureN,raw_clickbot$vax_futureY)
raw_clickbot$vax_future <- as.character(raw_clickbot$vax_future)

# code vaccine hesitancy as -1 = No do not want vaccine, 0 = undecided, 1 = yes. 
 
raw_clickbot$vax_future_1 <- ifelse(grepl("Yes",raw_clickbot$vax_future),1,
                                ifelse(grepl("Undecided",raw_clickbot$vax_future),0,
                                       ifelse(grepl("No",raw_clickbot$vax_future),-1, 99)))
                                              
# answer after experiment 
raw_clickbot$vax_future_2 <- ifelse((raw_clickbot$vax_future_2=="Yes"),1,
                                ifelse((raw_clickbot$vax_future_2=="Undecided"),0,
                                       ifelse((raw_clickbot$vax_future_2=="No"),-1,99)))

# calculate whether their decision changed after experiment  
raw_clickbot$vax_change <- (raw_clickbot$vax_future_2 - raw_clickbot$vax_future_1)

# column for if positive change
raw_clickbot$vax_positive <- ifelse((raw_clickbot$vax_change >0),1,0)

#check this works properly
vax_att_subset <- subset(raw_clickbot, select=c("vax_future","vax_future_1","vax_future_2","vax_change","vax_positive"))

#### make all vax attitude likert ratings wide to long: 5 attitude ratings put into one Pre and one Post item ####

colnames(raw_clickbot)
clickbot_analysis <- reshape(raw_clickbot, idvar = "ID", 
                 varying = list(c(5,6,7,8,9),
                                c(18,19,20,21,22)),
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

colnames(clickbot_analysis)
clickbot_analysis_longer <- reshape(clickbot_analysis,  
                             varying = list(c(52,53)),
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

#make condition binary 
clickbot_analysis$choice_cond <- ifelse((clickbot_analysis$condition=="choice"),1,0)

#### jump to here in future ####

#clickbot_analysis <- write.csv(clickbot_analysis, "clickbot_analysis.csv", row.names=FALSE)
#clickbot_analysis <- read.csv("clickbot_analysis.csv")

#### make engagement measures long (think we need two diff dataframes for this, check later) ####

#need to go back to raw_clickbot for this
colnames(raw_clickbot)

# four engagement measures converted to one long engagement measure
clickbot_engagement <- reshape(raw_clickbot, 
                             varying = list(c(25,26,27,28)),
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
                                                           ifelse((clickbot_engagement$engagement=="Somewhat disagree"),3,
                                                                  ifelse((clickbot_engagement$engagement=="Neither agree nor disagree"),4,
                                                                         ifelse((clickbot_engagement$engagement=="Somewhat agree"),5,
                                                                                ifelse((clickbot_engagement$engagement=="Agree"),6,
                                                                                       ifelse((clickbot_engagement$engagement=="Strongly Agree"),7,99)))))))

#make condition binary 
clickbot_engagement$choice_cond <- ifelse((clickbot_engagement$condition=="choice"),1,0)

# write to csv for now 
#clickbot_engagement <- write.csv(clickbot_engagement, "clickbot_engagement.csv", row.names=FALSE)
#clickbot_engagement <- read.csv("clickbot_engagement.csv")
