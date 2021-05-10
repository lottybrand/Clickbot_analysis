
library(rethinking)

#### dataframe for hypothesis 1 - more likely to vaccinate after choice condition ####

#pick variables you need
h_1_data <- subset(clickbot_analysis, select=c("ID","vax_positive","choice_cond"))

#remove duplication from long formatting (will use raw_clickbot for actual analysis)
h_1_data <- h_1_data[1:10,]

#make as large as the real thing will be (this is pilot data of 10, we will require 700, so repeat 70 times)
h_1_data <- h_1_data[rep(seq_len(nrow(h_1_data)), 70),]

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

#### dataframe for hypothesis 2 - do post treatment attitudes go up more in choice condition?  ####

h_2_data <- subset(clickbot_analysis, select=c("ID","attitude","att_type","post_rating","choice_cond"))

h_2_data <- h_2_data[rep(seq_len(nrow(h_2_data)), 70),]

#coerce index for random effect (think this is done alphabetically so will need to check back with clickbot_analysis)
h_2_data$att_type <- coerce_index(h_2_data$att_type)



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


#model testing just the effect of the experiment in general, ie do attitudes change after both conditions

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

#the change in interaction direction confused me, but it's because the effect of condition is smaller than the effect of experiment (= neg interaction effect) 

compare(h2_exp,h2_int,h2_full)

# tried a model with both main effects and interaction in here together to help make sense

h2_full_2 <- map2stan(
  alist(
    attitude ~ dordlogit(phi, cutpoints),
    phi <-  bPost_Cond*post_rating*choice_cond + bPost*post_rating +
      bCond*choice_cond +
      aR[ID]*sigmaR +
      aItem[att_type]*sigmaItem,
    bPost_Cond ~ dnorm(0,1),
    bPost ~ dnorm(0,1),
    bCond ~ dnorm(0,1),
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

precis(h2_full_2)

compare(h2_full,h2_full_2)
# prefers the first model. There is a strong effect of experiment treatment, and neg interaction (goes up more in the control condition!)

theMeans = tapply(clickbot_analysis$attitude, list(clickbot_analysis$post_rating, clickbot_analysis$choice_cond),mean)
#one on the left comes first (post_rating)
theMeans


#### dataframe for hypothesis 3 - choice condition is more engaging ####

h_3_data <- subset(clickbot_engagement, select=c("ID","engagement_1","eng_type","choice_cond"))

#re order to check this worked
h3_ordr <- h_3_data[ order(h_3_data$ID), ]
rm(h3_ordr)

# get big no. of ppts from pilot: 10 in pilot, 700 in real thing, *70  
h_3_data <- h_3_data[rep(seq_len(nrow(h_3_data)), 70),]


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

#control condition also more engaging here? blimey 

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

