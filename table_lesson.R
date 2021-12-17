
#### table lesson

table_pre<-table(clean_clickbot$vax_future_1,clean_clickbot$condition)
rownames(table_pre) = c("No (pre)","Undecided (pre)","Yes (pre)")
table_post<-table(clean_clickbot$vax_future_2,clean_clickbot$condition)
rownames(table_post) = c("No (post)","Undecided (post)","Yes (post)")

table1 <- rbind(table_pre,table_post)
table1 <- as.data.frame(table1)
t1 <- transpose(table1)
colnames(t1) <- rownames(table1)
rownames(t1) <- colnames(table1)

t1r <- reshape(t1, varying = list(c(1,4),
                                c(2,5),
                                c(3,6)),
                 v.names = c("No", "Undecided","Yes"), 
                 direction = "long")

t1r$id <- ifelse((t1r$id==1), "Choice","Control")
t1r$time <- ifelse((t1r$time==1),"Pre","Post")
t1r <- t1r[ order(t1r$id), ]
rownames(t1r) = c("Choice (Pre)","Choice (Post)","Control (Pre)","Control (Post)")
t1r$time <- NULL
t1r$id <- NULL
table1 <- t1r

source("loading_saved_models.R")
h2_waics <-compare(h2_null, h2_full, h2_exp, h2_int)
waicTable <- subset(h2_waics, select = c("WAIC","SE","weight"))

update.packages(ask = FALSE, checkBuilt = TRUE)
tinytex::tlmgr_update()
install.packages('tinytex')
