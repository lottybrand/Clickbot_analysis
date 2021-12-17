
#### table lesson

table_pre<-table(clean_clickbot$vax_future_1,clean_clickbot$condition)
rownames(table_pre) = c("No (pre)","Undecided (pre)","Yes (pre)")
table_post<-table(clean_clickbot$vax_future_2,clean_clickbot$condition)
rownames(table_post) = c("No (post)","Undecided (post)","Yes (post)")

table1 <- rbind(table_pre,table_post)

table1 %>% arrange(factor(Intention, levels = c('No (pre)', 'No (post)', 'Undecided (pre)', 'Undecided (post)', 'Yes (pre)', 'Yes (post)')))


source("loading_saved_models.R")
h2_waics <-compare(h2_null, h2_full, h2_exp, h2_int)
waicTable <- subset(h2_waics, select = c("WAIC","SE","weight"))

update.packages(ask = FALSE, checkBuilt = TRUE)
tinytex::tlmgr_update()
install.packages('tinytex')
