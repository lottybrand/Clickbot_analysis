
#### Key to Altay Script ####

# A0 = all data 
# A0 Condition = "Consentform(control) and Consentform(test)" 

# Bot = just the "test" (chatbot) condition dataframe 

# Control = just the control condition dataframe 

# D1 = Chatbot condition, made Long instead of Wide, based on pre & post attitude ratings. 

# D1 = *Overwritten and created a second time* using the Control dataframe instead of Bot dataframe. 

##### trying Altay's analysis strategy... #####

# Want to get to this: 
#RR = lm(Beh_post ~ Pre_centered_Beh + Condition, data = A1) 
#summary(RR)

clean_clickbot$vax_future_altay <- ifelse(grepl("Yes",clean_clickbot$vax_future),3,
                                      ifelse(grepl("Undecided",clean_clickbot$vax_future),2,
                                             ifelse(grepl("No",clean_clickbot$vax_future),1, 99)))

table(clean_clickbot$vax_future_1)
table(clean_clickbot$vax_future_altay)

clean_clickbot$vax_future_2_altay <- ifelse((clean_clickbot$vax_future_2==1),3,
                                      ifelse((clean_clickbot$vax_future_2==0),2,
                                             ifelse((clean_clickbot$vax_future_2==-1),1,99)))

table(clean_clickbot$vax_future_2)
table(clean_clickbot$vax_future_2_altay)

#A1$Pre_centered_Beh = A1$Beh_pre - mean(A1$Beh_pre, na.rm=T)

clean_clickbot$centred_beh <- clean_clickbot$vax_future_altay - mean(clean_clickbot$vax_future_altay, na.rm = T)

altay_model <- lm(vax_future_2_altay ~ vax_future_altay + condition, data = clean_clickbot) 
summary(altay_model)

# this makes sense. Still want to model moves away from No to double-check. so N0 =  1 everything else 0, 
# predict No's based on before/after and condition, so make wide -- long (and then varying intercept for ppt?) back to main analysis

# trying to convert intention to vaccinate (vax_future_1 and vax_future_2) into Nos or not, and predict Nos before and after? (did Nos decrease?)
# (is it better to do it this way round than predicting Not Nos and seeing if Not Nos increase? should be symmetrical...?)

colnames(clean_clickbot)
clean_clickbot_NOs <- reshape(clean_clickbot,  
                                    varying = list(c("vax_future_1","vax_future_2")),
                                    v.names = c("intention"), 
                                    direction = "long")

#change default 'time' column name to post intention (0 = pre, 1 = post)
colnames(clean_clickbot_NOs)[colnames(clean_clickbot_NOs) == "time"] <- "post_intention"

#make binary (1 = post rating, 0 = pre rating)
clean_clickbot_NOs$post_intention <- ifelse((clean_clickbot_NOs$post_intention ==1),0,1)

#make intention binary (1 = No, 0 = everything else)
clean_clickbot_NOs$intention <- ifelse((clean_clickbot_NOs$intention ==-1),1,0)

# remove that default ids column that reshape makes 
clean_clickbot_NOs$id <- NULL

#### now model this! ####

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

# yes! There were less no's after the treatment, this makes sense

#     mean   sd  5.5% 94.5% n_eff Rhat
# a  0.14 0.07  0.02  0.26  3057    1
# b -0.37 0.11 -0.54 -0.20  3329    1


