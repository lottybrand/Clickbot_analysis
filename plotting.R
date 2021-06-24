


#### Plotting ####

#### Converting data to be equivalent to Altay's ####


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


# f*&^ng character class strikes again... using numeric on its own screws the order, need as.character first too
# haven't checked if same problem occurs in Altay delta... will put on to do list...
fckng_chars <- clean_clickbot[,grepl("vax_att_p", colnames(clean_clickbot))]
fckng_chars <- colnames(fckng_chars)
clean_clickbot[fckng_chars] <- sapply(clean_clickbot[fckng_chars],as.character)

# Now numeric too..
clean_clickbot[fckng_chars] <- sapply(clean_clickbot[fckng_chars],as.numeric)
# thank god 
rm(fckng_chars)

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

control_plot <- plot_data[plot_data$choice_cond==0,]

# they do a time jitter thing and don't know what set.seed is for here 
set.seed(300)
control_plot$time <- as.numeric(control_plot$time)
control_plot$AA <- jitter(control_plot$time, amount=.10)

#### Control Violin Plot ####

cntrl_violin <- ggplot(data=control_plot, aes(y=attitude)) +
  geom_point(data = control_plot %>% filter(time =="1"), aes(x = AA), color = 'navajowhite1', size = 1.5, 
             alpha = .75) +
  geom_point(data = control_plot %>% filter(time =="2"), aes(x = AA), color = 'navajowhite1', size = 1.5, 
             alpha = .75) +
  geom_line(data = control_plot %>% filter(Slope_Up =="Up"), aes(x = AA, group = ID), color = 'forestgreen', 
            alpha = .25,  size = 0.25)+
  geom_line(data = control_plot %>% filter(Slope_Neutral =="Neutral"), aes(x = AA, group = ID), color = 'black', 
            alpha = .15,  size = 0.25)+
  geom_line(data = control_plot %>% filter(Slope_Down =="Down"), aes(x = AA, group = ID), color = 'firebrick3', 
            alpha = .25,  size = 0.25)+
  geom_half_boxplot(data = control_plot %>% filter(time=="1"), aes(x=time, y = attitude), 
                    position = position_nudge(x = -.25),
                    side = "r",outlier.shape = NA, center = TRUE, 
                    errorbar.draw = FALSE, width = .2, fill = 'moccasin')+
  geom_half_boxplot(data = control_plot %>% filter(time=="2"), aes(x=time, y = attitude), 
                    position = position_nudge(x = .15), side = "r",outlier.shape = NA, center = TRUE, 
                    errorbar.draw = FALSE, width = .2, fill = 'moccasin')+
  geom_half_violin(data = control_plot %>% filter(time=="1"),aes(x=time, y = attitude), 
                   position = position_nudge(x = -.3), 
                   side = "l", fill = 'moccasin')+
  geom_half_violin(data = control_plot %>% filter(time=="2"),aes(x = time, y = attitude), 
                   position = position_nudge(x = .3), side = "r", fill = "moccasin")+
  xlab("Time") + ylab("Attitudes towards Vaccines")+
  theme_classic()+
  scale_y_continuous(breaks=c(1,2,3,4,5,6,7))+
  ggtitle('Control')
cntrl_violin

#### Choice Violin plot ####

choice_plot <- plot_data[plot_data$choice_cond==1,]

# they do a time jitter thing. 
set.seed(300)
choice_plot$time <- as.numeric(choice_plot$time)
choice_plot$AA <- jitter(choice_plot$time, amount=.10)

choice_violin <- ggplot(data=choice_plot, aes(y=attitude)) +
  geom_point(data = choice_plot %>% filter(time =="1"), aes(x = AA), color = 'lightsteelblue2', size = 1.5, 
             alpha = .75) +
  geom_point(data = choice_plot %>% filter(time =="2"), aes(x = AA), color = 'lightsteelblue2', size = 1.5, 
             alpha = .75) +
  geom_line(data = choice_plot %>% filter(Slope_Up =="Up"), aes(x = AA, group = ID), color = 'forestgreen', 
            alpha = .25,  size = 0.25)+
  geom_line(data = choice_plot %>% filter(Slope_Neutral =="Neutral"), aes(x = AA, group = ID), color = 'black', 
            alpha = .15,  size = 0.25)+
  geom_line(data = choice_plot %>% filter(Slope_Down =="Down"), aes(x = AA, group = ID), color = 'firebrick3', 
            alpha = .25,  size = 0.25)+
  geom_half_boxplot(data = choice_plot %>% filter(time=="1"), aes(x=time, y = attitude), 
                    position = position_nudge(x = -.25),
                    side = "r",outlier.shape = NA, center = TRUE, 
                    errorbar.draw = FALSE, width = .2, fill = 'lightsteelblue2')+
  geom_half_boxplot(data = choice_plot %>% filter(time=="2"), aes(x=time, y = attitude), 
                    position = position_nudge(x = .15), side = "r",outlier.shape = NA, center = TRUE, 
                    errorbar.draw = FALSE, width = .2, fill = 'lightsteelblue2')+
  geom_half_violin(data = choice_plot %>% filter(time=="1"),aes(x=time, y = attitude), 
                   position = position_nudge(x = -.3), 
                   side = "l", fill = 'lightsteelblue2')+
  geom_half_violin(data = choice_plot %>% filter(time=="2"),aes(x = time, y = attitude), 
                   position = position_nudge(x = .3), side = "r", fill = "lightsteelblue2")+
  xlab("Time") + ylab("Attitudes towards Vaccines")+
  theme_classic()+
  scale_y_continuous(breaks=c(1,2,3,4,5,6,7))+
  ggtitle('Choice')
choice_violin


#### I would like to try the above with the raw data not means

#### Their Density Plots (they used means again)  ####

before = filter(plot_data, time == "1")
before$Condition <- ifelse(before$choice_cond==1,"Choice","Control")

after = filter(plot_data, time == "2")
after$Condition <- ifelse(after$choice_cond==1,"Choice","Control")

# before density plot

mu <- ddply(before, "Condition", summarise, grp.mean=mean(attitude))
DensityPlotBefore <- ggplot(data=before, aes(x=attitude, color=Condition)) + 
  geom_density(adjust=1.9, alpha=1, size = 2)+
  scale_x_continuous(name = "Att", breaks = seq(1, 7), limits=c(1, 7))+
  theme_pubr()
DensityPlotBefore
DensityPlotBefore + scale_color_manual(values=c("navajowhite3", "lightsteelblue3"))

# after density plot

mu <- ddply(after, "Condition", summarise, grp.mean=mean(attitude))
DensityPlotAfter <- ggplot(data=after, aes(x=attitude, color=Condition)) + 
  geom_density(adjust=1.9, alpha=1, size = 2)+
  scale_x_continuous(name = "Attitudes towards Covid-19 vaccines", breaks = seq(1, 7), limits=c(1, 7))+
  theme_pubr()
DensityPlotAfter
DensityPlotAfter + scale_color_manual(values=c("navajowhite3", "lightsteelblue3"))

# pre/post in one plot: 

plot_data$time <- ifelse(plot_data$time==1, "Before","After")
plot_data$time <- factor(plot_data$time, levels=c("Before", "After"))

DensityPlot_pre_post <- ggplot(data=plot_data, aes(x=attitude, color=time)) + 
  geom_density(adjust=1.9, alpha=1, size=2)+
  scale_x_continuous(name = "Attitudes towards Covid-19 vaccines", breaks = seq(1, 7), limits=c(1, 7)) +
  theme_pubr() 
DensityPlot_pre_post
DensityPlot_pre_post + scale_color_manual(values=c("navajowhite3", "lightsteelblue3"))


#### I prefer my raw version ####

h_2_data$choice_cond <- as.factor(h_2_data$choice_cond)
h_2_data$time <- ifelse(h_2_data$post_rating==0,"Before","After")
h_2_data$time <- factor(h_2_data$time, levels=c("Before", "After"))

DensityPlot_raw <- ggplot(data=h_2_data, aes(x=attitude, color=time)) + 
  geom_density(adjust=1.9, alpha=1, size=2)+
  scale_x_continuous(name = "Attitudes towards Covid-19 vaccines", breaks = seq(1, 7), limits=c(1, 7)) +
  theme_pubr() 
DensityPlot_raw
DensityPlot_raw + scale_color_manual(values=c("navajowhite3", "lightsteelblue3"))

#### Want to do same for Violins here: ####


#### engagement plot for condition comparison ####

h_3_data$choice_cond <- as.factor(h_3_data$choice_cond)
h_3_data$condition <- ifelse(h_3_data$choice_cond==1,"Choice","Control")

DensityPlot_eng <- ggplot(data=h_3_data, aes(x=engagement_1, color=condition)) + 
  geom_density(adjust=1.9, alpha=1, size=2)+
  scale_x_continuous(name = "Engagement", breaks = seq(1, 7), limits=c(1, 7)) +
  theme_pubr()
DensityPlot_eng
DensityPlot_eng + scale_color_manual(values=c("navajowhite3", "lightsteelblue3"))



