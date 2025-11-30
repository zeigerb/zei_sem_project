
library(readxl)
taban_df <- read_excel("taban_Collection_Data.xlsx", sheet = "count_data")

View(taban_df)

#factorize categorical data
taban_df$rep <- as.factor(taban_df)
taban_df$site <- as.factor(taban_df$site)
taban_df$type <- as.factor(taban_df$type)

summary(taban_df)


library(tidyverse)

#means and sd when grouped by study site
taban_df %>% group_by(site) %>%
  summarise(meanC = mean(count),
            sdC = sd(count))

#means and sd when grouped by trap placement type
taban_df %>% group_by(type) %>%
  summarise(meanC = mean(count),
            sdC = sd(count))

taban_df %>% ungroup()


#histogram of all observations
hist(taban_df$count)

#plot of count vs GDD
plot(count ~ GDD, data = taban_df, pch = 19)

#boxplot of count vs trap placement type
boxplot(taban_df$count ~ taban_df$type)




