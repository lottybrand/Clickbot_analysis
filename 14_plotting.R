
#### Plotting ####

##### load plot_data (created in data_processing script)
##### unless you've already loaded it in data_processing script ####
plot_data <- read.csv("data/plot_data.csv")


##### Control violin plot #####

control_plot <- plot_data[plot_data$choice_cond==0,]

# they do a time jitter thing and don't know what set.seed is for here 
set.seed(300)
control_plot$time <- as.numeric(control_plot$time)
control_plot$AA <- jitter(control_plot$time, amount=.10)

# plot
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

# time jitter thing. 
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


#### Now try showing before and after for both conditions together (as no diff ) ####

# they do a time jitter thing and don't know what set.seed is for here 
set.seed(300)
plot_data$time <- as.numeric(plot_data$time)
plot_data$AA <- jitter(plot_data$time, amount=.10)

# plot
both_violin <- ggplot(data=plot_data, aes(y=attitude)) +
  geom_point(data = plot_data %>% filter(time =="1"), aes(x = AA), color = 'navajowhite1', size = 1.5, 
             alpha = .75) +
  geom_point(data = plot_data %>% filter(time =="2"), aes(x = AA), color = 'navajowhite1', size = 1.5, 
             alpha = .75) +
  geom_line(data = plot_data %>% filter(Slope_Up =="Up"), aes(x = AA, group = ID), color = 'forestgreen', 
            alpha = .25,  size = 0.25)+
  geom_line(data = plot_data %>% filter(Slope_Neutral =="Neutral"), aes(x = AA, group = ID), color = 'black', 
            alpha = .15,  size = 0.25)+
  geom_line(data = plot_data %>% filter(Slope_Down =="Down"), aes(x = AA, group = ID), color = 'firebrick3', 
            alpha = .25,  size = 0.25)+
  geom_half_boxplot(data = plot_data %>% filter(time=="1"), aes(x=time, y = attitude), 
                    position = position_nudge(x = -.25),
                    side = "r",outlier.shape = NA, center = TRUE, 
                    errorbar.draw = FALSE, width = .2, fill = 'moccasin')+
  geom_half_boxplot(data = plot_data %>% filter(time=="2"), aes(x=time, y = attitude), 
                    position = position_nudge(x = .15), side = "r",outlier.shape = NA, center = TRUE, 
                    errorbar.draw = FALSE, width = .2, fill = 'moccasin')+
  geom_half_violin(data = plot_data %>% filter(time=="1"),aes(x=time, y = attitude), 
                   position = position_nudge(x = -.3), 
                   side = "l", fill = 'moccasin')+
  geom_half_violin(data = plot_data %>% filter(time=="2"),aes(x = time, y = attitude), 
                   position = position_nudge(x = .3), side = "r", fill = "moccasin")+
  xlab("Time") + ylab("Average attitudes towards Covid-19 vaccines")+
  theme_classic()+
  scale_y_continuous(limits = c(1, 7), breaks = seq(1, 7, by = 1))+
  ggtitle('Attitudes before and after dialogue exposure') +
  theme(plot.title = element_text(hjust = 0.5))
both_violin


#### I would like to try the above with the raw data (not means) here #####

# this means using "long clickbot" to make "wide clickbot"

wide_clickbot <- read.csv("data/wide_clickbot.csv", stringsAsFactors = FALSE)

wide_clickbot$attitude_change = wide_clickbot$attitude.1 - wide_clickbot$attitude.0

# make the slopes same as Altay 
wide_clickbot$Slope_Up = ifelse(wide_clickbot$attitude_change >=1, "Up", "0") # 0.2 to do 1pts
wide_clickbot$Slope_Down = ifelse(wide_clickbot$attitude_change <=-1, "Down", "0")
wide_clickbot$Slope_Neutral = ifelse(wide_clickbot$attitude_change < 0 & wide_clickbot$attitude_change > -0.2, "Neutral", "0")

raw_violin_data <- reshape(wide_clickbot,  
                     varying = list(c("attitude.0","attitude.1")),
                     v.names = c("attitude"), 
                     direction = "long")

raw_violin_data <- subset(raw_violin_data, select=c("ID","attitude","time","choice_cond", "Slope_Up","Slope_Down","Slope_Neutral"))


# now need to create the slopes for h2 data. think we said 1 instead of .2 now?

# they do a time jitter thing and don't know what set.seed is for here 
set.seed(300)
raw_violin_data$time <- as.numeric(raw_violin_data$time)
raw_violin_data$AA <- jitter(raw_violin_data$time, amount=.10)

# plot
raw_both_violin <- ggplot(data=raw_violin_data, aes(y=attitude)) +
  geom_point(data = raw_violin_data %>% filter(time =="1"), aes(x = AA), color = 'navajowhite1', size = 1.5, 
             alpha = .75) +
  geom_point(data = raw_violin_data %>% filter(time =="2"), aes(x = AA), color = 'navajowhite1', size = 1.5, 
             alpha = .75) +
  geom_line(data = raw_violin_data %>% filter(Slope_Up =="Up"), aes(x = AA, group = ID), color = 'forestgreen', 
            alpha = .25,  size = 0.25)+
  geom_line(data = raw_violin_data %>% filter(Slope_Neutral =="Neutral"), aes(x = AA, group = ID), color = 'black', 
            alpha = .15,  size = 0.25)+
  geom_line(data = raw_violin_data %>% filter(Slope_Down =="Down"), aes(x = AA, group = ID), color = 'firebrick3', 
            alpha = .25,  size = 0.25)+
  geom_half_boxplot(data = raw_violin_data %>% filter(time=="1"), aes(x=time, y = attitude), 
                    position = position_nudge(x = -.25),
                    side = "r",outlier.shape = NA, center = TRUE, 
                    errorbar.draw = FALSE, width = .2, fill = 'moccasin')+
  geom_half_boxplot(data = raw_violin_data %>% filter(time=="2"), aes(x=time, y = attitude), 
                    position = position_nudge(x = .15), side = "r",outlier.shape = NA, center = TRUE, 
                    errorbar.draw = FALSE, width = .2, fill = 'moccasin')+
  geom_half_violin(data = raw_violin_data %>% filter(time=="1"),aes(x=time, y = attitude), 
                   position = position_nudge(x = -.3), 
                   side = "l", fill = 'moccasin')+
  geom_half_violin(data = raw_violin_data %>% filter(time=="2"),aes(x = time, y = attitude), 
                   position = position_nudge(x = .3), side = "r", fill = "moccasin")+
  xlab("Time") + ylab("Attitudes towards Vaccines")+
  theme_classic()+
  scale_y_continuous(breaks=c(1,2,3,4,5,6,7))+
  ggtitle('Attitudes before and after')
raw_both_violin

# attitude change measure is helpful like this 
ggplot(data = wide_clickbot, aes(x= attitude_change)) + 
  geom_bar(stat="count",fill="aquamarine4") +
  xlab("Change in attitude") + ylab("No. of responses") +
  theme_bw() +
  theme(strip.background =element_rect(fill="aquamarine4")) +
  theme(strip.text = element_text(colour = 'yellow3', size=12))


Change_direction <- c("Increased", "Stayed_The_Same","Decreased", "Increased","Stayed_The_Same","Decreased")  
Position <- c("Against", "Against","Against", "Neutral","Neutral","Neutral")
Proportion <- c(0.268, 0.632, 0.093, 0.342, 0.574, 0.084)

att_change_frame <- (data.frame(Change_direction,Position,Proportion))

ggplot(data = att_change_frame, aes(x= Change_direction)) + 
  geom_bar(stat="count",fill="aquamarine4") +
  xlab("Change in attitude") + ylab("No. of responses") +
  theme_bw() +
  theme(strip.background =element_rect(fill="aquamarine4")) +
  theme(strip.text = element_text(colour = 'yellow3', size=12))

rawPlot <- ggplot(att_change_frame, aes(Change_direction, Proportion, color = Position)) +
  geom_point(size=2.8) +
  theme_bw() + theme(text = element_text(size=12), axis.title.x=element_blank(), axis.title.y=element_text(margin=margin(0,12,0,0))) + 
  ylab("Proportion of Responses")
rawPlot

#### Their Density Plots (they used means again)  #####

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

# load the h_2 dataset from hypothesis 2 analysis
# first load long_clickbot
long_clickbot <- read.csv("data/long_clickbot.csv")
h_2_data <- subset(long_clickbot, select=c("ID","attitude","att_type","post_rating","choice_cond"))

h_2_data$choice_cond <- as.factor(h_2_data$choice_cond)
h_2_data$time <- ifelse(h_2_data$post_rating==0,"Before","After")
h_2_data$time <- factor(h_2_data$time, levels=c("Before", "After"))

DensityPlot_raw <- ggplot(data=h_2_data, aes(x=attitude, color=time)) + 
  geom_density(adjust=1.9, alpha=1, size=2)+
  scale_x_continuous(name = "Attitudes towards Covid-19 vaccines", breaks = seq(1, 7), limits=c(1, 7)) +
  theme_pubr() 
DensityPlot_raw
DensityPlot_raw + scale_color_manual(values=c("navajowhite3", "lightsteelblue3"))

#### Can I get density plots for each item? what does this tell us? 

safe_subset <- h_2_data[h_2_data$att_type=="Safe",]
safe_density <- ggplot(data=safe_subset, aes(x=attitude, color=time)) + 
  geom_density(adjust=1.9, alpha=1, size=2)+
  scale_x_continuous(name = "Attitudes towards Covid-19 vaccines", breaks = seq(1, 7), limits=c(1, 7)) +
  ggtitle("SAFE") +
  theme_pubr() 
safe_density

effective_subset <- h_2_data[h_2_data$att_type=="Effective",]
effective_density <- ggplot(data=effective_subset, aes(x=attitude, color=time)) + 
  geom_density(adjust=1.9, alpha=1, size=2)+
  scale_x_continuous(name = "Attitudes towards Covid-19 vaccines", breaks = seq(1, 7), limits=c(1, 7)) +
  ggtitle("EFFECTIVE") +
  theme_pubr() 
effective_density

Enough_time_sub <- h_2_data[h_2_data$att_type=="Enough_time",]
time_density <- ggplot(data=Enough_time_sub, aes(x=attitude, color=time)) + 
  geom_density(adjust=1.9, alpha=1, size=2)+
  scale_x_continuous(name = "Attitudes towards Covid-19 vaccines", breaks = seq(1, 7), limits=c(1, 7)) +
  ggtitle("Enough Time") +
  theme_pubr() 
time_density

trust_subset <- h_2_data[h_2_data$att_type=="Trust",]
trust_density <- ggplot(data=trust_subset, aes(x=attitude, color=time)) + 
  geom_density(adjust=1.9, alpha=1, size=2)+
  scale_x_continuous(name = "Attitudes towards Covid-19 vaccines", breaks = seq(1, 7), limits=c(1, 7)) +
  ggtitle("Trust") +
  theme_pubr() 
trust_density

important_subset <- h_2_data[h_2_data$att_type=="Important",]
important_density <- ggplot(data=important_subset, aes(x=attitude, color=time)) + 
  geom_density(adjust=1.9, alpha=1, size=2)+
  scale_x_continuous(name = "Attitudes towards Covid-19 vaccines", breaks = seq(1, 7), limits=c(1, 7)) +
  ggtitle("Important") +
  theme_pubr() 
important_density

#### Want to do same for Violins here: ####


#### engagement plot for condition comparison ####

# load h3 data 
engagement_clickbot <- read.csv("data/engagement_clickbot.csv")
h_3_data <- subset(engagement_clickbot, select=c("ID","engagement_1","eng_type","choice_cond"))

h_3_data$choice_cond <- as.factor(h_3_data$choice_cond)
h_3_data$condition <- ifelse(h_3_data$choice_cond==1,"Choice","Control")

DensityPlot_eng <- ggplot(data=h_3_data, aes(x=engagement_1, color=condition)) + 
  geom_density(adjust=1.9, alpha=1, size=2)+
  scale_x_continuous(name = "Engagement", breaks = seq(1, 7), limits=c(1, 7)) +
  theme_pubr()
DensityPlot_eng
DensityPlot_eng + scale_color_manual(values=c("navajowhite3", "lightsteelblue3"))



