##### exploring comments & demogs

comms <- read.csv("comments_and_demogs.csv")

gov <- comms[(grepl("govern",comms$anything_else, ignore.case = TRUE)),]

df$anything_else[(grepl("govern",df$anything_else, ignore.case = TRUE))]

gov <- df$anything_else[(grepl("govern",df$anything_else, ignore.case = TRUE))]


?grepl
