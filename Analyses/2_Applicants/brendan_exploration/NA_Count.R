library(dplyr)
library(ggplot2)
library(purrr)

cat_apps <- readr::read_csv(file = "Data/cat_apps.csv" ) %>%
  mutate(animal_type = "cat")

dog_apps <- readr::read_csv(file = "Data/dog_apps.csv") %>% 
  mutate(animal_type = "dog")

#combine
apps_raw <- rbind(cat_apps, dog_apps)

#count NA
na_check <- purrr::map_df(apps_raw, ~sum(is.na(.))) %>% 
  reshape2::melt(.) %>% 
  filter(variable != "X1") %>%
  filter(value > 0)

ggplot(na_check, aes(x = reorder(variable, value), y = value)) +
  geom_bar(stat="identity") + 
  labs(x = "Application Question", 
       y = "Count of NA's",
       title = "Count of NAs in Applications Data Set"
  ) + 
  coord_flip()

apps_raw %>% filter(is.na(exercise))

unique(apps_raw$allergies)

apps_raw %>% group_by(allergies) %>% summarise(count = n()) %>% arrange(desc(count))

budget <- apps_raw %>% 
  mutate(budget_monthly = gsub("[$]|[(]|[)]|[,]", "", budget_monthly)) %>%
 # mutate(budget_monthly = as.numeric(as.character(gsub("^-", "", budget_monthly)))) %>%
  select(budget_monthly) 
  #group_by(budget_monthly) %>% 
  #summarise(count = n()) %>% 
  ggplot(budget, aes(x = as.numeric(budget_monthly))) + 
  geom_histogram()
