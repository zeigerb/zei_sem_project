
library(readxl)
fly_df <- read_excel("Fly_Collection_Data.xlsx", sheet = "count_data")

View(fly_df)

summary(fly_df)

library(tidyverse)

fly_df %>% group_by(site) %>%
  summarise(mean(count))

fly_df %>% group_by(type) %>%
  summarise(mean(count))

fly_df %>% ungroup()


