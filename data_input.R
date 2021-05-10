

#### Analysis for our Qualtrics Clickbot vaccination study ####
#### 

#### load qualtrics file (didn't use qualtRics package because of Prolific IDs)

qualtrics_clickbot <- read.csv("pilot_ten_26April2021.csv")

key <- qualtrics_clickbot[1:3,]

#clean
#remove first two rows of qualtrics rubbish
qualtrics_clickbot <-qualtrics_clickbot[3:12,]

#remove qualtrics column crap (will need to include unwanted timing columns after pilot study)
qual_crap <- c("EndDate","Status","Progress","Finished","RecordedDate","ResponseId","DistributionChannel","UserLanguage","FL_3_DO")
raw_clickbot <- qualtrics_clickbot[, !(colnames(qualtrics_clickbot) %in% qual_crap)]

rm(qualtrics_clickbot)
