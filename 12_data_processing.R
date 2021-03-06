
####
#### processing the data for analysis ####
####

# run this line below if you haven't run data_input.R first
# raw_clickbot <- read.csv("raw_clickbot.csv", stringsAsFactors = FALSE )

clean_clickbot <- raw_clickbot 

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

# sort the f*&^ng character class...
fckng_chars <- clean_clickbot[,grepl("timing|timer", colnames(clean_clickbot))]
fckng_chars <- colnames(fckng_chars)
clean_clickbot[fckng_chars] <- sapply(clean_clickbot[fckng_chars],as.numeric)

#remove unnecessary objects
rm(fckng_chars)

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

#write now. 
#write.csv(clean_clickbot, file="clean_clickbot.csv", row.names=FALSE)

####
#### make all vax attitude likert ratings wide to long: 5 attitude ratings put into one Pre and one Post item ####
####

# make pre and post vax attitudes from wide to long
colnames(clean_clickbot)
long_clickbot <- reshape(clean_clickbot, idvar = "ID", 
                 varying = list(c("vax_att_1","vax_att_2","vax_att_3","vax_att_4","vax_att_5"),
                                c("post_vax_att_1","post_vax_att_2","post_vax_att_3","post_vax_att_4","post_vax_att_5")),
                 v.names = c("pre_att", "post_att"), 
                 direction = "long")

# it automatically codes time as the attitude type, name with actual attitude type:
long_clickbot$time <- ifelse((long_clickbot$time==1),"Safe",
                                 ifelse((long_clickbot$time==2),"Effective",
                                        ifelse((long_clickbot$time==3),"Enough_time",
                                               ifelse((long_clickbot$time==4),"Trust",
                                                      ifelse((long_clickbot$time==5),"Important",99)))))

#rename time as att_type
colnames(long_clickbot)[colnames(long_clickbot) == "time"] <- "att_type"

#turning the likert responses into 1-7 for pre and post att
long_clickbot$pre_att_1 <- ifelse((long_clickbot$pre_att=="Strongly Disagree"),1,
                                 ifelse((long_clickbot$pre_att=="Disagree"),2,
                                        ifelse((long_clickbot$pre_att=="Somewhat Disagree"),3,
                                               ifelse((long_clickbot$pre_att=="Neither Agree nor Disagree"),4,
                                                      ifelse((long_clickbot$pre_att=="Somewhat Agree"),5,
                                                             ifelse((long_clickbot$pre_att=="Agree"),6,
                                                                    ifelse((long_clickbot$pre_att=="Strongly Agree"),7,99)))))))

long_clickbot$post_att_2 <- ifelse((long_clickbot$post_att=="Strongly Disagree"),1,
                                      ifelse((long_clickbot$post_att=="Disagree"),2,
                                             ifelse((long_clickbot$post_att=="Somewhat Disagree"),3,
                                                    ifelse((long_clickbot$post_att=="Neither Agree nor Disagree"),4,
                                                           ifelse((long_clickbot$post_att=="Somewhat Agree"),5,
                                                                  ifelse((long_clickbot$post_att=="Agree"),6,
                                                                         ifelse((long_clickbot$post_att=="Strongly Agree"),7,99)))))))

#### now merge into one long attitude rating, so can compare pre/post vax atts ####

colnames(long_clickbot)
long_clickbot_longer <- reshape(long_clickbot,  
                             varying = list(c("pre_att_1","post_att_2")),
                             v.names = c("attitude"), 
                             direction = "long")

#change default 'time' column name to post rating 
colnames(long_clickbot_longer)[colnames(long_clickbot_longer) == "time"] <- "post_rating"

#make binary (1 = post rating, 0 = pre rating, currently "time" was 1 pre and 2 post.
long_clickbot_longer$post_rating <- ifelse((long_clickbot_longer$post_rating ==1),0,1)

#remove the default 'id' column it generates, not needed here
long_clickbot_longer$id <- NULL

#### transfer back to long_clickbot
long_clickbot <- long_clickbot_longer
rm(long_clickbot_longer)

# perfect now everyone has 10 attitude ratings each, 5 before and 5 after, making 7160 obs. 
# save long_clickbot as csv
#long_clickbot <- write.csv(long_clickbot, "long_clickbot.csv", row.names=FALSE)


####
#### make engagement measures long in separate datafile ####
####

#maybe also change name of this to engagement_clickbot 

#need to go back to clean_clickbot for this
colnames(clean_clickbot)

# four engagement measures converted to one long engagement measure
engagement_clickbot <- reshape(clean_clickbot, 
                             varying = list(c("experience_1","experience_2","experience_3","experience_4")),
                             v.names = c("engagement"), 
                             direction = "long")

#change default 'time' column name to engagement type
colnames(engagement_clickbot)[colnames(engagement_clickbot) == "time"] <- "eng_type"


#remove the default 'id' column it generates, not needed here
engagement_clickbot$id <- NULL

# name engagement type
engagement_clickbot$eng_type <- ifelse((engagement_clickbot$eng_type==1),"Engaging",
                                          ifelse((engagement_clickbot$eng_type==2),"Enjoyable",
                                             ifelse((engagement_clickbot$eng_type==3),"Confusing",
                                               ifelse((engagement_clickbot$eng_type==4),"Frustrating",99))))


# make likert responses numbers

engagement_clickbot$engagement_1 <- ifelse((engagement_clickbot$engagement=="Strongly Disagree"),1,
                                                    ifelse((engagement_clickbot$engagement=="Disagree"),2,
                                                           ifelse((engagement_clickbot$engagement=="Somewhat Disagree"),3,
                                                                  ifelse((engagement_clickbot$engagement=="Neither Agree nor Disagree"),4,
                                                                         ifelse((engagement_clickbot$engagement=="Somewhat Agree"),5,
                                                                                ifelse((engagement_clickbot$engagement=="Agree"),6,
                                                                                       ifelse((engagement_clickbot$engagement=="Strongly Agree"),7,99)))))))




# NEED TO NEGATIVE CODE TWO OF THEM 

# this is probably one of the worsts way to do it, but, my brain won't do anything else right now. 
# Sorry everyone reading this code, very sorry. 

#reverse code "confusing" 
engagement_clickbot$engagement_1 <- ifelse(((engagement_clickbot$eng_type=="Confusing" & engagement_clickbot$engagement_1==1)),7,
                                ifelse(((engagement_clickbot$eng_type=="Confusing" & engagement_clickbot$engagement_1==2)),6,
                                   ifelse(((engagement_clickbot$eng_type=="Confusing" & engagement_clickbot$engagement_1==3)),5,
                                        ifelse(((engagement_clickbot$eng_type=="Confusing" & engagement_clickbot$engagement_1==4)),4,
                                               ifelse(((engagement_clickbot$eng_type=="Confusing" & engagement_clickbot$engagement_1==5)),3,
                                                      ifelse(((engagement_clickbot$eng_type=="Confusing" & engagement_clickbot$engagement_1==6)),2,
                                                             ifelse(((engagement_clickbot$eng_type=="Confusing" & engagement_clickbot$engagement_1==7)),1,engagement_clickbot$engagement_1)))))))

#reverse code "frustrating"
engagement_clickbot$engagement_1 <- ifelse(((engagement_clickbot$eng_type=="Frustrating" & engagement_clickbot$engagement_1==1)),7,
                                ifelse(((engagement_clickbot$eng_type=="Frustrating" & engagement_clickbot$engagement_1==2)),6,
                                       ifelse(((engagement_clickbot$eng_type=="Frustrating" & engagement_clickbot$engagement_1==3)),5,
                                              ifelse(((engagement_clickbot$eng_type=="Frustrating" & engagement_clickbot$engagement_1==4)),4,
                                                     ifelse(((engagement_clickbot$eng_type=="Frustrating" & engagement_clickbot$engagement_1==5)),3,
                                                            ifelse(((engagement_clickbot$eng_type=="Frustrating" & engagement_clickbot$engagement_1==6)),2,
                                                                   ifelse(((engagement_clickbot$eng_type=="Frustrating" & engagement_clickbot$engagement_1==7)),1,engagement_clickbot$engagement_1)))))))



# write to csv
#engagement_clickbot <- write.csv(engagement_clickbot, "engagement_clickbot.csv", row.names=FALSE)

##### PROCESSING DATA FOR PLOTTING #####
##### Converting data to be equivalent to Altay's for plotting #####
####

# to do the violin version, need to create "slope up" etc, and so need to calculate means on our likert data (PUKE!)
# i should really write a bloody function here

# likert_switch <- function(x){
#   mapvalues(clean_clickbot, from = c("Strongly Agree", "Agree", "Somewhat Agree", "Neither Agree nor Disagree",
#                               "Somewhat Disagree", "Disagree", "Strongly Disagree"),
#             to = c(7,6,5,4,3,2,1))
# }

# att_likerts <- clean_clickbot[,grepl("vax_att", colnames(clean_clickbot))]
# att_likerts <- colnames(att_likerts)
# clean_clickbot[att_likerts] <- sapply(clean_clickbot[att_likerts],mapvalues)

## figure out the function later (whaaaaa) repeat five times (SIIIGHH): 

clean_clickbot$vax_att_p1 <- mapvalues(clean_clickbot$vax_att_1, from = c("Strongly Agree", "Agree", "Somewhat Agree", "Neither Agree nor Disagree",
                                                                          "Somewhat Disagree", "Disagree", "Strongly Disagree"),
                                       to = c(7,6,5,4,3,2,1))

clean_clickbot$vax_att_p2 <- mapvalues(clean_clickbot$vax_att_2, from = c("Strongly Agree", "Agree", "Somewhat Agree", "Neither Agree nor Disagree",
                                                                          "Somewhat Disagree", "Disagree", "Strongly Disagree"),
                                       to = c(7,6,5,4,3,2,1))

clean_clickbot$vax_att_p3 <- mapvalues(clean_clickbot$vax_att_3, from = c("Strongly Agree", "Agree", "Somewhat Agree", "Neither Agree nor Disagree",
                                                                          "Somewhat Disagree", "Disagree", "Strongly Disagree"),
                                       to = c(7,6,5,4,3,2,1))

clean_clickbot$vax_att_p4 <- mapvalues(clean_clickbot$vax_att_4, from = c("Strongly Agree", "Agree", "Somewhat Agree", "Neither Agree nor Disagree",
                                                                          "Somewhat Disagree", "Disagree", "Strongly Disagree"),
                                       to = c(7,6,5,4,3,2,1))

clean_clickbot$vax_att_p5 <- mapvalues(clean_clickbot$vax_att_5, from = c("Strongly Agree", "Agree", "Somewhat Agree", "Neither Agree nor Disagree",
                                                                          "Somewhat Disagree", "Disagree", "Strongly Disagree"),
                                       to = c(7,6,5,4,3,2,1))

# post 

clean_clickbot$post_vax_att_p1 <- mapvalues(clean_clickbot$post_vax_att_1, from = c("Strongly Agree", "Agree", "Somewhat Agree", "Neither Agree nor Disagree",
                                                                                    "Somewhat Disagree", "Disagree", "Strongly Disagree"),
                                            to = c(7,6,5,4,3,2,1))

clean_clickbot$post_vax_att_p2 <- mapvalues(clean_clickbot$post_vax_att_2, from = c("Strongly Agree", "Agree", "Somewhat Agree", "Neither Agree nor Disagree",
                                                                                    "Somewhat Disagree", "Disagree", "Strongly Disagree"),
                                            to = c(7,6,5,4,3,2,1))

clean_clickbot$post_vax_att_p3 <- mapvalues(clean_clickbot$post_vax_att_3, from = c("Strongly Agree", "Agree", "Somewhat Agree", "Neither Agree nor Disagree",
                                                                                    "Somewhat Disagree", "Disagree", "Strongly Disagree"),
                                            to = c(7,6,5,4,3,2,1))

clean_clickbot$post_vax_att_p4 <- mapvalues(clean_clickbot$post_vax_att_4, from = c("Strongly Agree", "Agree", "Somewhat Agree", "Neither Agree nor Disagree",
                                                                                    "Somewhat Disagree", "Disagree", "Strongly Disagree"),
                                            to = c(7,6,5,4,3,2,1))

clean_clickbot$post_vax_att_p5 <- mapvalues(clean_clickbot$post_vax_att_5, from = c("Strongly Agree", "Agree", "Somewhat Agree", "Neither Agree nor Disagree",
                                                                                    "Somewhat Disagree", "Disagree", "Strongly Disagree"),
                                            to = c(7,6,5,4,3,2,1))


# character class strikes again... using numeric on its own screws the order, need as.character first too
# haven't checked if same problem occurs in Altay data... will put on to do list...
f_chars <- clean_clickbot[,grepl("vax_att_p", colnames(clean_clickbot))]
f_chars <- colnames(f_chars)
clean_clickbot[f_chars] <- sapply(clean_clickbot[f_chars],as.character)

# Now numeric too..
clean_clickbot[f_chars] <- sapply(clean_clickbot[f_chars],as.numeric)
# thank god 
rm(f_chars)

clean_clickbot$Att_Pre = (clean_clickbot$vax_att_p1 + clean_clickbot$vax_att_p2 + clean_clickbot$vax_att_p3 + clean_clickbot$vax_att_p4 + clean_clickbot$vax_att_p5)/5
clean_clickbot$Att_Post = (clean_clickbot$post_vax_att_p1 + clean_clickbot$post_vax_att_p2 + clean_clickbot$post_vax_att_p3 + clean_clickbot$post_vax_att_p4 + clean_clickbot$post_vax_att_p5)/5
clean_clickbot$attitude_change = clean_clickbot$Att_Post - clean_clickbot$Att_Pre

# make the slopes same as Altay 
clean_clickbot$Slope_Up = ifelse(clean_clickbot$attitude_change >=0.2, "Up", "0") # 0.2 to do 1pts
clean_clickbot$Slope_Down = ifelse(clean_clickbot$attitude_change <=-0.2, "Down", "0")
clean_clickbot$Slope_Neutral = ifelse(clean_clickbot$attitude_change < 0.2&clean_clickbot$attitude_change > -0.2, "Neutral", "0")


# they made "D1" using gather(). I'M GOING TO USE RESHAPE() and call it "attitude" not "Score"...!! (and call it plot_data not D1)

plot_data <- reshape(clean_clickbot,  
                     varying = list(c("Att_Pre","Att_Post")),
                     v.names = c("attitude"), 
                     direction = "long")

plot_data <- subset(plot_data, select=c("ID","attitude","time","choice_cond", "Slope_Up","Slope_Down","Slope_Neutral"))

#write.csv(plot_data, "data/plot_data.csv")

# make long to wide for the raw violin (long already numeric)

wide_clickbot <- reshape(long_clickbot,  
                     timevar = c("post_rating"),
                     v.names = c("attitude"),
                     idvar = c("ID", "att_type"),
                     direction = "wide")


