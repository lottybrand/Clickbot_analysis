# - - - - - - - 
# Loads cleaned data and runs the preregistered analysis on the clickbot data
# - - - - - - - 

library(tidyverse)
library(rethinking)
# https://github.com/rmcelreath/rethinking  note the dependencies - 
# you need stan installed # https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
# linux, also this https://github.com/stan-dev/rstan/wiki/Configuring-C-Toolchain-for-Linux
## current status: no focal/20.04 release of https://launchpad.net/~marutter/+archive/ubuntu/c2d4u
## upgrade R to 4 or downgrade ubuntu to 18.04?
# linux, you also need V8 https://github.com/jeroen/V8

#recommended options
options(mc.cores = parallel::detectCores()) #For execution on a local, multicore CPU with excess RAM we recommend calling
rstan_options(auto_write = TRUE) #To avoid recompilation of unchanged Stan programs, we recommend calling
#set_ulam_cmdstan(TRUE)

#load data cleaned by data_processing.R
clean_clickbot <- read.csv('clean_clickbot.csv')

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

model_start <- Sys.time()

model1 <- map2stan(
  alist(
    vax_positive ~ dbinom(1, p),
    logit(p) <- a + b*choice_cond,
    a ~ dnorm(0,1),
    b ~ dnorm(0,1)
  ),
  data=h_1_data, 
  warmup=1000, iter=4000, chains=1, cores=1)

model_stop <- Sys.time()

model_time <- model_stop - model_start

precis(model1)
plot(precis(model1))

saveRDS(model1, "model1.rds")

#### dataframe for hypothesis 2 - do post treatment attitudes go up more in choice condition?  ####

#load data
clickbot_analysis <- read.csv('clickbot_analysis.csv')

#select vars we want
h_2_data <- subset(clickbot_analysis, select=c("ID","attitude","att_type","post_rating","choice_cond"))

#coerce index for random effect (think this is done alphabetically so will need to check back with clickbot_analysis)
h_2_data$att_type <- coerce_index(h_2_data$att_type)

# quick look at the means so no nasty shocks: 

theMeans = tapply(clickbot_analysis$attitude, list(clickbot_analysis$post_rating, clickbot_analysis$choice_cond),mean)
#one on the left comes first (post_rating)
theMeans

#null model, just varying intercepts for attitude type and rater (ID)

model_start <- Sys.time()

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
  chains = 1, cores = 1, iter=1200)

model_stop <- Sys.time()

model_time <- model_stop - model_start


precis(h2_null)
#model testing just the effect of the experiment in general, ie do attitudes change after both conditions
saveRDS(h2_null, "h2_null.rds")
