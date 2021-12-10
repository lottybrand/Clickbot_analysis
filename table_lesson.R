#### table lesson

table1<-table(clean_clickbot$vax_future_1,clean_clickbot$condition)
table2<-table(clean_clickbot$vax_future_2,clean_clickbot$condition)


rownames(table1) = c("No","Undecided","Yes")
table1

rownames(table2) = c("No","Undecided","Yes")
table2

source("loading_saved_models.R")
