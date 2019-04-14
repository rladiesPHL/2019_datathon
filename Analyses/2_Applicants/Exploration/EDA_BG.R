#load packages
library(readr)
library(dplyr)
library(lubridate)
library(highcharter)
library(ggplot2)
library(zoo)

#load data
cat_actions <- readr::read_csv(file = "Data/cat_actions.csv") %>%  mutate(animal_type = "cat")
cat_apps <- readr::read_csv(file = "Data/cat_apps.csv" ) %>% mutate(animal_type = "cat")
cat_cards <- readr::read_csv(file = "Data/cat_cards.csv") %>% mutate(animal_type = "cat")

dog_actions <- readr::read_csv(file = "Data/dog_actions.csv") %>% mutate(animal_type = "dog")
dog_apps <- readr::read_csv(file = "Data/dog_apps.csv") %>% mutate(animal_type = "dog")
dog_cards <- readr::read_csv(file = "Data/dog_cards.csv") %>% mutate(animal_type = "dog")

pp <-  readr::read_csv(file = "Data/petpoint.csv") 

#combine
apps_raw <- rbind(cat_apps, dog_apps)
cards_raw <- rbind(cat_cards, dog_cards)
actions_raw <- rbind(cat_actions, dog_actions)

#explore
skimr::skim(cards_raw)
skimr::skim(apps_raw)
skimr::skim(actions_raw)

#application volume by month
apps %>% 
  mutate(date_submitted = lubridate::mdy(date_submitted),
         month = lubridate::floor_date(date_submitted, "month")) %>%
  group_by(month, animal_type) %>%
  summarise(count = n()) %>%
  mutate(freq = count/sum(count))  %>%
  ggplot(aes(x = month, y = count, fill = animal_type, label = scales::percent(freq))) + 
  geom_bar(stat = "identity") + 
  labs(title = "monthly application volume") + 
  geom_text(size = 3, position = position_stack(vjust = 0.5))

#applications with no outcome - 276
sum(is.na(apps_raw$outcome_trello_id))

#checking for duplicate apps
dup_app_ids <- apps_raw %>% 
  filter(!is.na(outcome_trello_id)) %>%
  group_by(outcome_trello_id) %>% 
  summarise(count = n()) %>% 
  filter(count > 1)

dup_apps <- apps_raw %>%
  filter(outcome_trello_id %in% dup_app_ids$outcome_trello_id) %>%
  arrange(desc(outcome_trello_id)) %>%
  group_by(outcome_trello_id) %>%
  mutate(id = row_number()) %>%
  select(outcome_trello_id, id, X1:how_heard, STATEFP:animal_type)

#most dups are people applying twice. 3 people have aaplied 3 times
hist(dup_apps$id)

#checking for duplicate cards - seems like there is 1 row per id in cards dataset
cards_raw %>% 
  group_by(id) %>% 
  summarise(count = n()) %>% 
  filter(count > 1) %>%
  nrow()

#checking for duplicate actions - many rows per id, may need to collapse to 1 row
actions_raw %>% 
  group_by(data.card.id) %>% 
  summarise(count = n()) %>% 
  filter(count > 1)

#arrange actionsand collapse into 1 row per id
actions <- actions_raw %>% 
  group_by(data.card.id) %>%
  mutate(first_date = min(date)) %>%
  ungroup() %>%
  group_by(data.card.id) %>% 
  mutate(row_number = row_number()) %>%
  select(-c(checklist_seq_num, row_number)) %>%
  mutate(checklist_ACCT = ifelse(checklist_ACCT == T, as.character(date), NA),
         checklist_CHQ = ifelse(checklist_CHQ == T, as.character(date), NA),
         checklist_LL = ifelse(checklist_LL == T, as.character(date), NA),
         checklist_PP = ifelse(checklist_PP == T, as.character(date), NA),
         checklist_SPCA = ifelse(checklist_SPCA == T, as.character(date), NA),
         checklist_TR = ifelse(checklist_TR == T, as.character(date), NA),
         checklist_VET = ifelse(checklist_VET == T, as.character(date), NA),
         ) %>%
  select(-date) %>%
  mutate_all(funs(na.locf(., na.rm = FALSE, fromLast = FALSE))) %>% 
  filter(row_number()==n())

#check counts of "item states" in actions data set
actions %>% 
  group_by(data.checkItem.state) %>% 
  summarise(count = n()) %>%
  ungroup() %>%
  ggplot(., aes(x = reorder(data.checkItem.state, -count), y = count)) + 
  geom_bar(stat = "identity")


#checking for dups in petpoint file - seems like there are dups, may need to remove
p_dups <- pp %>% 
  filter(!is.na(outcome_trello_id)) %>%
  group_by(outcome_trello_id) %>% 
  summarise(count = n()) %>% 
  filter(count > 1)

pp_dups <- pp %>%
  filter(outcome_trello_id %in% p_dups$outcome_trello_id) %>%
  group_by(outcome_trello_id) %>%
  mutate(id = row_number()) %>%
  arrange(outcome_trello_id) %>%
  ungroup() %>%
  group_by() %>%
  mutate(group_id = group_indices_(., .dots = "outcome_trello_id")) %>%
  select(outcome_trello_id, id, group_id, X1:outcome_sitename, STATEFP:outcome_ZIP)

#checking why dup

my.list <- list()

for(name in colnames(pp_dups)){
  
  result <- pp_dups %>% 
    mutate(change = ifelse(name == lag(name), "no change", "change"))
  
  list.name <- as.character(paste0("change_", name))
  
  my.list[[list.name]] <- result
  
}

test <- pp_dups %>% 
  mutate(change = ifelse(species == lag(species), "no change", "change"))

pp_unique <-  pp %>%
  select(-X1) %>% 
  unique()