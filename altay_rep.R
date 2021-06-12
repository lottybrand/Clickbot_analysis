

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

altay_model <- lm(vax_future_2_altay ~ centred_beh + condition, data = clean_clickbot) 
summary(altay_model)

# this makes sense. Still want to model moves away from No to double-check. so N0 =  1 everything else 0, 
# predict No's based on before/after and condition, so make wide -- long (and then varying intercept for ppt?)

A1 <- read.csv("A1_altay.csv")

table(A1$Beh_post, A1$Condition)

table(clean_clickbot$vax_future_2_altay, clean_clickbot$condition)
