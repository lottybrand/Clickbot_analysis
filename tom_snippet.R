#https://github.com/lottybrand/Clickbot_analysis

library(tidyverse)

#import data
filename <- 'anonymised_click_bot_abcd_May_25_2021_14.25.csv'
df <- read.csv(filename)

df <- df %>% filter( condition == 'control' | condition == 'choice') %>% #drop empty rows
  rename(duration = Duration..in.seconds.) #rename variable while we're here


#min duration correct = 3. WHUT
min(as.numeric(df$duration))

#looksie- values are spread about as we'd hope
p <- ggplot(df,aes(x=ResponseId,y=duration))
p+geom_point()

#this fails, because of the data type
p <- ggplot(df,aes(x=duration)) + geom_histogram()

#fix this at import
#this is useful to read https://stackoverflow.com/questions/2288485/how-to-convert-a-data-frame-column-to-numeric-type

df <- read.csv(filename,stringsAsFactors=FALSE)

df <- df %>% filter( condition == 'control' | condition == 'choice') %>% #drop empty rows
  rename(duration = Duration..in.seconds.) #rename variable while we're here

df$duration <- as.numeric(df$duration) #that's right, we still need to convert to numeric

#min duration correct = 154. Better
min(df$duration)

mean(df$duration) # = 835, makes sense

#looksie
p <- ggplot(df,aes(x=duration))
p+ geom_histogram()

#now let's look at those dataimte objects
#this will help https://lubridate.tidyverse.org/

