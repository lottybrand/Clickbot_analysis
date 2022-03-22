##### exploring comments & demogs

df <- read.csv("comments_and_demogs.csv")


gov <- df$anything_else[(grepl("govern",df$anything_else, ignore.case = TRUE, useBytes = TRUE))]
gov <- as.data.frame(gov)


