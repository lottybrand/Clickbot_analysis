
##### To run our analysis, you must first load the three datasets below, and install rethinking:
##### If you have already run the data_input or data_processing scripts, the below should already be loaded for you, so you can skip loading them here. 
##### ALTERNATIVELY you can load the model objects that correspond to each model below ##### 
##### using the "model_objects" folder to explore the model results yourself. 
model1 <- readRDS("model1.rds")

##### Load data files #####

# load cleaned, processed data file
clean_clickbot <- read.csv("data/clean_clickbot.csv")

# load long version for attitude analyses
long_clickbot <- read.csv("data/long_clickbot.csv")

# load engagement dataframe
engagement_clickbot <- read.csv("data/engagement_clickbot.csv")

library(rethinking)


####
#### dataframe for hypothesis 1 - more likely to vaccinate after choice condition? (Intention Change) ####
####


#pick variables you need (include vax_future_1 to remove those already yes at beginning)
h_1_data <- subset(clean_clickbot, select=c("ID","vax_positive","choice_cond","vax_future_1","vax_future_2"))


#check raw nums
#table(h_1_data$vax_future_1, h_1_data$choice_cond)
#table(h_1_data$vax_future_2, h_1_data$choice_cond)


# remove those who already said yes to begin with as they can't change positively anyway 
h_1_data <- h_1_data[!(h_1_data$vax_future_1==1),]

#table(h_1_data$vax_positive, h_1_data$choice_cond)

# model likelihood of attitude changing positively, and effect of condition on this likelihood: 

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

saveRDS(model1, "model_objects/model1.rds")

# also analysed it same way as Altay did (see altay_rep.R file) to compare - still no effect of condition, but what about in general, what does its intercept tell us?
# altay report similar decrease in "no's" to us. See if there are less 'nos' after compared to before exp. 
# so code all No's as 1 and everything else 0. then code before vs after. then do 
# no ~ dbinom   a + after_exp
# so need to convert to long format for this:
# convert intention to vaccinate (vax_future_1 and vax_future_2) into Nos or not, and predict Nos before and after (wide to long):

colnames(clean_clickbot)
clean_clickbot_NOs <- reshape(clean_clickbot,  
                              varying = list(c("vax_future_1","vax_future_2")),
                              v.names = c("intention"), 
                              direction = "long")

#change default 'time' column name to post intention (1 = vax_future_1, 2 = vax_future_2)
colnames(clean_clickbot_NOs)[colnames(clean_clickbot_NOs) == "time"] <- "post_intention"

#make binary (1 = vax_future_1, = pre-rating, = 0. 2=vax_future_2 = post rating, =1)
clean_clickbot_NOs$post_intention <- ifelse((clean_clickbot_NOs$post_intention ==1),0,1)

#make intention binary (1 = No, 0 = everything else)
clean_clickbot_NOs$intention <- ifelse((clean_clickbot_NOs$intention ==-1),1,0)

# remove that default ids column that reshape makes 
clean_clickbot_NOs$id <- NULL

#### okay now model to check if there is intention change after the experiment in general ####

#just choose the columns it wants
no_data <- subset(clean_clickbot_NOs, select=c("ID","intention","post_intention"))

model_nos <- map2stan(
  alist(
    intention ~ dbinom(1, p),
    logit(p) <- a + b*post_intention,
    a ~ dnorm(0,1),
    b ~ dnorm(0,1)
  ),
  data=no_data, 
  warmup=1000, iter=4000, chains=3, cores=3)

precis(model_nos)
saveRDS(model_nos, "model_objects/model_nos.rds")

# Okay there were less no's after the treatment.

#     mean   sd  5.5% 94.5% n_eff Rhat
# a  0.14 0.07  0.02  0.26  3057    1
# b -0.37 0.11 -0.54 -0.20  3329    1

####
#### dataframe for hypothesis 2 - do post treatment attitudes go up more in choice condition?  ####
####


h_2_data <- subset(long_clickbot, select=c("ID","attitude","att_type","post_rating","choice_cond"))


#coerce index for random effect (think this is done alphabetically so will need to check back with long_clickbot)
h_2_data$att_type <- coerce_index(h_2_data$att_type)

# quick look at the means so no nasty shocks: 
#theMeans = tapply(long_clickbot$attitude, list(long_clickbot$post_rating, long_clickbot$choice_cond),mean)
#one on the left comes first (post_rating)
#theMeans

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
saveRDS(h2_null, "model_objects/h2_null.rds")

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
saveRDS(h2_exp, "model_objects/h2_exp.rds")

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
saveRDS(h2_int, "model_objects/h2_int.rds")

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
saveRDS(h2_full, "model_objects/h2_full.rds")

#the change in interaction direction confused me, but it's because the effect of condition is smaller than the effect of experiment (= neg interaction effect) 

compare(h2_exp,h2_int,h2_full, h2_null)

# tried a model with both main effects and interaction in here together to help make sense in the pre-reg run. doesn't make sense as we exp no main effect of cond (no diff before)
# and was worse fitting model anyway during pilot data

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

# precis(h2_full_2)

# compare(h2_full,h2_full_2)
# prefers the first model in the pilot. There is a strong effect of experiment treatment, and neg interaction (goes up more in the control condition!)

####
#### dataframe for hypothesis 3 - is choice condition is more engaging? ####
####

#engagement_clickbot<-read.csv("../data/engagement_clickbot.csv")

h_3_data <- subset(engagement_clickbot, select=c("ID","engagement_1","eng_type","choice_cond"))

#coerce index for random effect (think this is done alphabetically so will need to check back with long_clickbot
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

