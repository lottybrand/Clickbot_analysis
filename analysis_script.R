
library(rethinking)

#### dataframe for hypothesis 1 - more likely to vaccinate after choice condition ####

#pick variables you need (include vax_future_1 to remove those already yes at beginning)
h_1_data <- subset(clean_clickbot, select=c("ID","vax_positive","choice_cond","vax_future_1","vax_future_2"))

# h_yes_data <- h_1_data[(h_1_data$vax_future_1==1),] 
h_yes_data <- h_1_data[(h_1_data$vax_future_1==1),] 
table(h_yes_data$vax_future_2)

#check raw nums
table(h_1_data$vax_future_1, h_1_data$choice_cond)
table(h_1_data$vax_future_2, h_1_data$choice_cond)


#remove those who already said yes to begin with as they can't change positively anyway (will check if any changed negatively!)
# 145 said they would consider vaccine in future before the exp (hmm maybe wording wasn't great here)
h_1_data <- h_1_data[!(h_1_data$vax_future_1==1),]

table(h_1_data$vax_positive, h_1_data$choice_cond)

#need to make a table with vax_future_1 and vax_future_2 in each conditions

model1 <- map2stan(
  alist(
    vax_positive ~ dbinom(1, p),
    logit(p) <- a + b*choice_cond,
    a ~ dnorm(0,1),
    b ~ dnorm(0,1)
  ),
  data=h_1_data, 
  warmup=1000, iter=4000, chains=3, cores=3)

precis(model1)
plot(precis(model1))

saveRDS(model1, "model1.rds")

# analysed it same way as Altay did - no effect of condition, but what about in general, what does its intercepts tell us?
# think we actually want to see if there are less 'nos' after compared to before exp. Figure out how to recode this way. 
# so code all No's as 1 and everything else 0 ? then code before vs after? then do 

# no ~ dbinom   a + after exp

# think i need to convert to long format for this

#### dataframe for hypothesis 2 - do post treatment attitudes go up more in choice condition?  ####

h_2_data <- subset(clickbot_analysis, select=c("ID","attitude","att_type","post_rating","choice_cond"))


#coerce index for random effect (think this is done alphabetically so will need to check back with clickbot_analysis)
h_2_data$att_type <- coerce_index(h_2_data$att_type)

# quick look at the means so no nasty shocks: 

theMeans = tapply(clickbot_analysis$attitude, list(clickbot_analysis$post_rating, clickbot_analysis$choice_cond),mean)
#one on the left comes first (post_rating)
theMeans

#null model, just varying intercepts for attitude type and rater (ID)

h2_null <- map2stan(
  alist(
    attitude ~ dordlogit(phi, cutpoints),
    phi <-  aR[ID]*sigmaR +
      aItem[att_type]*sigmaItem,
    aR[ID] ~ dnorm(0,1),
    aItem[att_type] ~ dnorm(0,1),
    c(sigmaR, sigmaItem) ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=h_2_data, 
  constraints = list(sigmaItem = "lower=0", sigmaR = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter=1200)

precis(h2_null)
#model testing just the effect of the experiment in general, ie do attitudes change after both conditions
saveRDS(h2_null, "h2_null.rds")

h2_exp <- map2stan(
  alist(
    attitude ~ dordlogit(phi, cutpoints),
    phi <-  bPost*post_rating +
      aR[ID]*sigmaR +
      aItem[att_type]*sigmaItem,
    bPost ~ dnorm(0,1),
    aR[ID] ~ dnorm(0,1),
    aItem[att_type] ~ dnorm(0,1),
    c(sigmaR, sigmaItem) ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=h_2_data, 
  constraints = list(sigmaItem = "lower=0", sigmaR = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter=1200)

precis(h2_exp)
saveRDS(h2_exp, "h2_exp.rds")

#model testing just the effect of the interaction, ie do attitudes change more after seeing the choice condition compared to after seeing the control condition?

h2_int <- map2stan(
  alist(
    attitude ~ dordlogit(phi, cutpoints),
    phi <-  bPost_Cond*post_rating*choice_cond +
      aR[ID]*sigmaR +
      aItem[att_type]*sigmaItem,
    bPost_Cond ~ dnorm(0,1),
    aR[ID] ~ dnorm(0,1),
    aItem[att_type] ~ dnorm(0,1),
    c(sigmaR, sigmaItem) ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=h_2_data, 
  constraints = list(sigmaItem = "lower=0", sigmaR = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter=1200)

precis(h2_int)
saveRDS(h2_int, "h2_int.rds")

#full model with both interaction and experiment effects

h2_full <- map2stan(
  alist(
    attitude ~ dordlogit(phi, cutpoints),
    phi <-  bPost_Cond*post_rating*choice_cond + bPost*post_rating +
      aR[ID]*sigmaR +
      aItem[att_type]*sigmaItem,
    bPost_Cond ~ dnorm(0,1),
    bPost ~ dnorm(0,1),
    aR[ID] ~ dnorm(0,1),
    aItem[att_type] ~ dnorm(0,1),
    c(sigmaR, sigmaItem) ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=h_2_data, 
  constraints = list(sigmaItem = "lower=0", sigmaR = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter=1200)

precis(h2_full)
saveRDS(h2_full, "h2_full.rds")

#the change in interaction direction confused me, but it's because the effect of condition is smaller than the effect of experiment (= neg interaction effect) 

compare(h2_exp,h2_int,h2_full, h2_null)

# tried a model with both main effects and interaction in here together to help make sense in the pre-reg run. doesn't make sense as we exp no main effect of cond (no diff before)

#h2_full_2 <- map2stan(
  # alist(
  #   attitude ~ dordlogit(phi, cutpoints),
  #   phi <-  bPost_Cond*post_rating*choice_cond + bPost*post_rating +
  #     bCond*choice_cond +
  #     aR[ID]*sigmaR +
  #     aItem[att_type]*sigmaItem,
  #   bPost_Cond ~ dnorm(0,1),
  #   bPost ~ dnorm(0,1),
  #   bCond ~ dnorm(0,1),
  #   aR[ID] ~ dnorm(0,1),
  #   aItem[att_type] ~ dnorm(0,1),
  #   c(sigmaR, sigmaItem) ~ normal(0,0.1),
  #   cutpoints ~ dnorm(0,10)
  # ),
  # data=h_2_data, 
  # constraints = list(sigmaItem = "lower=0", sigmaR = "lower=0"),
  # start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  # control=list(adapt_delta=0.99, max_treedepth=13),
  # chains = 3, cores = 3, iter=1200)

#precis(h2_full_2)

#compare(h2_full,h2_full_2)
# prefers the first model. There is a strong effect of experiment treatment, and neg interaction (goes up more in the control condition!)

theMeans = tapply(clickbot_analysis$attitude, list(clickbot_analysis$post_rating, clickbot_analysis$choice_cond),mean)
#one on the left comes first (post_rating)
theMeans


#### dataframe for hypothesis 3 - choice condition is more engaging ####

h_3_data <- subset(clickbot_engagement, select=c("ID","engagement_1","eng_type","choice_cond"))

#re order to check this worked (can just click the ID heading using the window)
#h3_ordr <- h_3_data[ order(h_3_data$ID), ]
#rm(h3_ordr)

# make sure you reverse-ordered 'confusing' and 'frustrating' eng_types in data_processing first!! 

#coerce index for random effect (think this is done alphabetically so will need to check back with clickbot_analysis)
h_3_data$eng_type <- coerce_index(h_3_data$eng_type)


#model checking effect of condition on engagement, controlling for ppt and engagement type

h3_model <- map2stan(
  alist(
    engagement_1 ~ dordlogit(phi, cutpoints),
    phi <-  bCond*choice_cond + 
      aR[ID]*sigmaR +
      aItem[eng_type]*sigmaItem,
    bCond ~ dnorm(0,1),
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

precis(h3_model)
saveRDS(h3_model, "h3_model.rds")


theMeansEng = tapply(h_3_data$engagement_1, list(h_3_data$choice_cond),mean)
theMeansEng


#### Plotting ####

h_3_data$choice_cond <- as.factor(h_3_data$choice_cond)

DensityPlot_eng <- ggplot(data=h_3_data, aes(x=engagement_1, color=choice_cond)) + 
  geom_density(adjust=1.9, alpha=1)+
  scale_x_continuous(name = "Engagement", breaks = seq(1, 7), limits=c(1, 7))
DensityPlot_eng

h_2_data$choice_cond <- as.factor(h_2_data$choice_cond)
h_2_pre <- h_2_data[h_2_data$post_rating==0,]
h_2_post <- h_2_data[h_2_data$post_rating==1,]

DensityPlot_Pre_att <- ggplot(data=h_2_pre, aes(x=attitude, color=choice_cond)) + 
  geom_density(adjust=1.9, alpha=1)+
  scale_x_continuous(name = "Vax Attitude", breaks = seq(1, 7), limits=c(1, 7))
DensityPlot_Pre_att

DensityPlot_Post_att <- ggplot(data=h_2_post, aes(x=attitude, color=choice_cond)) + 
  geom_density(adjust=1.9, alpha=1)+
  scale_x_continuous(name = "Vax Attitude", breaks = seq(1, 7), limits=c(1, 7))
DensityPlot_Post_att

h_2_data$post_rating <- as.factor(h_2_data$post_rating)

DensityPlot_Post_att <- ggplot(data=h_2_data, aes(x=attitude, color=post_rating)) + 
  geom_density(adjust=1.9, alpha=1, size=2)+
  scale_x_continuous(name = "Vax Attitude", breaks = seq(1, 7), limits=c(1, 7)) +
  theme_bw()
DensityPlot_Post_att

