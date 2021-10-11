
##### Timing Data 

##### go back to the beginning to get raw_clickbot with timing data intact #####

overall_time <- tapply(clean_clickbot$Duration, list(clean_clickbot$condition),mean, na.rm=TRUE)
overall_time/60

min(clean_clickbot$Duration[clean_clickbot$condition=="choice"])/60
min(clean_clickbot$Duration[clean_clickbot$condition=="control"])/60

max(clean_clickbot$Duration[clean_clickbot$condition=="choice"])/60
max(clean_clickbot$Duration[clean_clickbot$condition=="control"])/60

median(clean_clickbot$Duration[clean_clickbot$condition=="choice"])
median(clean_clickbot$Duration[clean_clickbot$condition=="control"])/60

p <- ggplot(clean_clickbot,aes(x=Duration))
p+ geom_histogram()



min(clean_clickbot$control_info_time, na.rm = TRUE)
max(clean_clickbot$control_info_time, na.rm = TRUE)
median(clean_clickbot$control_info_time, na.rm = TRUE)
median(clean_clickbot$control_info_time, na.rm = TRUE)/60

p_control <- ggplot(clean_clickbot,aes(x=control_info_time))
p_control + geom_histogram()

min(clean_clickbot$choice_info_time, na.rm = TRUE)
max(clean_clickbot$choice_info_time, na.rm = TRUE)
median(clean_clickbot$choice_info_time, na.rm = TRUE)
median(clean_clickbot$choice_info_time, na.rm = TRUE)/60

p_choice <- ggplot(clean_clickbot,aes(x=choice_info_time))
p_choice + geom_histogram()

choice_time <- clean_clickbot[(clean_clickbot$choice_info_time<1000),]
cntrl_time <- clean_clickbot[(clean_clickbot$control_info_time<1000),]

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
#clean_clickbot <- timing_clickbot

# okay now subset of above median for cntrl and choice, (167 for choice and 237 for cntrl )
# and minus outliers (above 1000 secs)

subset_time <- clean_clickbot[which(clean_clickbot$choice_info_time<1000 & clean_clickbot$choice_info_time > 167),]
summary(subset_time$choice_info_time)

#ugh using which helps I guess but this feels a dumb way to do it:  
subset_time2 <- clean_clickbot[which(clean_clickbot$control_info_time<1000 & clean_clickbot$control_info_time > 237),]
summary(subset_time$control_info_time)

#now merge subset_time and subset_time2 ... but we don't want this! we just want to add a column in the original dataframe denoting above median time, to compare to the rest, right? 
# now my brain has stopped for the day... continue tomorrow

#maybe we do want both. Want to A) check if there is a condition effect for those spending over 4 minutes (intentions ANd attitudes?) and
# B) check if those spending over 4 minutes are more likely to change intentions/attitudes compared to not - so for this need the originals

# A) first. Intentions: 

subset_time <- rbind(subset_time,subset_time2)

#replicate our intention model (model 1) with this data 

#pick variables you need (include vax_future_1 to remove those already yes at beginning)
subset_time <- subset(subset_time, select=c("ID","vax_positive","choice_cond","vax_future_1","vax_future_2"))

table(subset_time$vax_future_1)

#remove those who already said yes to begin with as they can't change positively anyway (will check if any changed negatively!)
# 145 said they would consider vaccine in future before the exp (hmm maybe wording wasn't great here)
subset_time_analysis <- subset_time[!(subset_time$vax_future_1==1),]

table(subset_time_analysis$vax_positive, subset_time_analysis$choice_cond)
#okay can see from the raw numbers no point checking for condition effect here. Let's run the model for consistency. 

model1.1 <- map2stan(
  alist(
    vax_positive ~ dbinom(1, p),
    logit(p) <- a + b*choice_cond,
    a ~ dnorm(0,1),
    b ~ dnorm(0,1)
  ),
  data=subset_time_analysis, 
  warmup=1000, iter=4000, chains=3, cores=3)

precis(model1.1)
#     mean   sd  5.5% 94.5% n_eff Rhat
# a -1.41 0.20 -1.73 -1.09  3793    1
# b -0.17 0.29 -0.64  0.30  3508    1

saveRDS(model1.1, "model1.1_timing.rds")

# so no effect of condition on intention change just when looking at those spending over 4 minutes 

# need to do same for attitude model 

# now B) 

#####  probably two separate columns then merge? 
clean_clickbot$most_time_ch <- ifelse((clean_clickbot$choice_info_time<1000 & clean_clickbot$choice_info_time > 167),1,0)
table(clean_clickbot$most_time_ch)
clean_clickbot$most_time_cntrl <- ifelse((clean_clickbot$control_info_time<1000 & clean_clickbot$control_info_time > 237),1,0)
table(clean_clickbot$most_time_cntrl)

clean_clickbot$most_time <- ifelse(clean_clickbot$choice_cond==1, clean_clickbot$most_time_ch,
                                   ifelse(clean_clickbot$choice_cond==0, clean_clickbot$most_time_cntrl,99))
table(clean_clickbot$most_time)

subset_time2 <- subset(clean_clickbot, select=c("ID","vax_positive","most_time","vax_future_1","vax_future_2"))
subset_time2 <- subset_time2[!(subset_time2$vax_future_1==1),]

model1.2 <- map2stan(
  alist(
    vax_positive ~ dbinom(1, p),
    logit(p) <- a + b*most_time,
    a ~ dnorm(0,1),
    b ~ dnorm(0,1)
  ),
  data=subset_time2, 
  warmup=1000, iter=4000, chains=3, cores=3)

precis(model1.2)

#   mean   sd  5.5% 94.5% n_eff Rhat
# a -1.73 0.16 -2.00 -1.48  2857    1
# b  0.21 0.22 -0.15  0.57  2813    1

saveRDS(model1.2, "model1.2_timing.rds")

# so spending over 4 minutes doens't increase likelihood of changing intention. 

##### okay now we need to make wide to long again for attitude models #####

# need to rerun lines 44 in the data_processing.R script , pasted here, because column nums change

colnames(clean_clickbot)
clickbot_analysis <- reshape(clean_clickbot, idvar = "ID", 
                             varying = list(c(7,8,9,10,11),
                                            c(28,29,30,31,32)),
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


# now reran lines 80 - 100 in data_processing.R (had to change col nums)

# now using clickbot_analysis 

h_2_data_t <- subset(clickbot_analysis, select=c("ID","attitude","att_type","post_rating","choice_cond", "most_time"))


#coerce index for random effect (think this is done alphabetically so will need to check back with clickbot_analysis)
h_2_data_t$att_type <- coerce_index(h_2_data_t$att_type)
table(h_2_data_t$ID)

h_2_data_t$ID <- coerce_index(h_2_data_t$ID)
check_index(h_2_data_t$ID)

#okay use James' emergency code:
NParticipants = length(unique(h_2_data_t$ID))
OldID <- h_2_data_t$ID
ParticipantID <- array(0,length(h_2_data_t$ID))
for (index in 1:NParticipants){
  ParticipantID[OldID == unique(OldID)[index]] = index
}
h_2_data_t$ID <- ParticipantID

# now try h2_full but also effect of time? (or do we just want timing model? )
# (now remembering maybe we want to just analyse the most_time data like we did for intentions) 
# just try time model for now... just looking at effect of spending most time

table(h_2_data_t$most_time)

h2_time <- map2stan(
  alist(
    attitude ~ dordlogit(phi, cutpoints),
    phi <-  bTime*most_time +
      aR[ID]*sigmaR +
      aItem[att_type]*sigmaItem,
    bTime ~ dnorm(0,1),
    aR[ID] ~ dnorm(0,1),
    aItem[att_type] ~ dnorm(0,1),
    c(sigmaR, sigmaItem) ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=h_2_data_t, 
  constraints = list(sigmaItem = "lower=0", sigmaR = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter=1200)

precis(h2_time)

#           mean   sd 5.5% 94.5% n_eff Rhat
# bTime     0.32 0.15 0.08  0.56   270 1.01
# sigmaR    1.97 0.04 1.90  2.04   808 1.00
# sigmaItem 0.44 0.05 0.37  0.52  1986 1.00

saveRDS(h2_time, "model_h2_time.rds")

h2_int_time <- map2stan(
  alist(
    attitude ~ dordlogit(phi, cutpoints),
    phi <-  bPost_Time*post_rating*most_time +
      aR[ID]*sigmaR +
      aItem[att_type]*sigmaItem,
    bPost_Time ~ dnorm(0,1),
    aR[ID] ~ dnorm(0,1),
    aItem[att_type] ~ dnorm(0,1),
    c(sigmaR, sigmaItem) ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=h_2_data_t, 
  constraints = list(sigmaItem = "lower=0", sigmaR = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter=1200)

precis(h2_int_time)

#             mean   sd 5.5% 94.5% n_eff Rhat
# bPost_Time 0.81 0.06 0.71  0.90  2085 1.00
# sigmaR     1.99 0.05 1.92  2.07   612 1.01
# sigmaItem  0.44 0.05 0.37  0.53  1273 1.00

saveRDS(h2_int_time, "model_h2_int_time.rds")


h2_full_time <- map2stan(
  alist(
    attitude ~ dordlogit(phi, cutpoints),
    phi <-  bPost_Time*post_rating*most_time + bPost*post_rating +
      aR[ID]*sigmaR +
      aItem[att_type]*sigmaItem,
    bPost_Time ~ dnorm(0,1),
    bPost ~ dnorm(0,1),
    aR[ID] ~ dnorm(0,1),
    aItem[att_type] ~ dnorm(0,1),
    c(sigmaR, sigmaItem) ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=h_2_data_t, 
  constraints = list(sigmaItem = "lower=0", sigmaR = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter=1200)

precis(h2_full_time)
saveRDS(h2_full_time, "model_h2_full_time.rds")

compare(h2_full_time,h2_int_time,h2_time)

# full is best : 

#           mean   sd 5.5% 94.5% n_eff Rhat
# bPost_Time 0.48 0.08 0.34  0.61  1281    1
# bPost      0.35 0.06 0.26  0.45  1784    1
# sigmaR     2.00 0.04 1.93  2.06   585    1
# sigmaItem  0.44 0.05 0.38  0.52  1453    1


#### engagement and most time? 

engagement_clickbot$most_time_ch <- ifelse((engagement_clickbot$choice_info_time<1000 & engagement_clickbot$choice_info_time > 167),1,0)
table(engagement_clickbot$most_time_ch)
engagement_clickbot$most_time_cntrl <- ifelse((engagement_clickbot$control_info_time<1000 & engagement_clickbot$control_info_time > 237),1,0)
table(engagement_clickbot$most_time_cntrl)

engagement_clickbot$most_time <- ifelse(engagement_clickbot$choice_cond==1, engagement_clickbot$most_time_ch,
                                   ifelse(engagement_clickbot$choice_cond==0, engagement_clickbot$most_time_cntrl,99))
table(engagement_clickbot$most_time)

h_3_data <- subset(engagement_clickbot, select=c("ID","engagement_1","eng_type","most_time"))

#coerce index for random effect (think this is done alphabetically so will need to check back with long_clickbot
h_3_data$eng_type <- coerce_index(h_3_data$eng_type)



eng_time_model <- map2stan(
  alist(
    engagement_1 ~ dordlogit(phi, cutpoints),
    phi <-  bTime*most_time + 
      aR[ID]*sigmaR +
      aItem[eng_type]*sigmaItem,
    bTime ~ dnorm(0,1),
    aR[ID] ~ dnorm(0,1),
    aItem[eng_type] ~ dnorm(0,1),
    c(sigmaR, sigmaItem) ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=h_3_data, 
  constraints = list(sigmaItem = "lower=0", sigmaR = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter=1200)

precis(eng_time_model)
#           mean   sd 5.5% 94.5% n_eff Rhat
#bTime     0.37 0.12 0.18  0.55   779    1
#sigmaR    1.29 0.05 1.22  1.37   713    1
#sigmaItem 0.34 0.05 0.27  0.42  1500    1

