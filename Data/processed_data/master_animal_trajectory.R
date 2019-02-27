# script to create master_animal ----

library(tidyverse)

# fix dates = turns "12/31/2018 11:59 AM" to "2018-12-31"
convert_date <-  function(x) {
    mdy_hm(x) %>% as.Date()
}

# base data sets ----
petpoint <-
    read.csv("Data/petpoint.csv") %>%
    select(-c(X, animal_type, markings, dob, 
              STATEFP:INTPTLON)) %>% 
    rename(id = outcome_trello_id) %>% 
    filter(!is.na(id)) %>% 
    filter(species != "Wild Mammal") %>% 
    filter(outcome_type != "Admin Missing") %>% 
    mutate_at(vars(matches("date")), funs(convert_date)) %>% 
    mutate(wait_days = as.integer(difftime(release_date, intake_date, units = "days")),
           wday_intake = wday(intake_date, abbr = TRUE, label = TRUE))

actions_wide <-
    read.csv("Data/processed_data/actions_wide.csv")

apps <-
    read.csv("Data/processed_data/all_apps.csv") %>% 
    select(-c(STATEFP:ZIP)) %>% 
    rename(id = outcome_trello_id) %>% 
    filter(!is.na(id)) %>% 
    mutate(date_submitted = mdy(date_submitted)) %>% 
    mutate_at(vars(contains("alone")),
              funs(as.integer(str_extract(., "\\d+")))) %>% 
    mutate_at(vars(contains("budget")),
              funs(as.integer(str_replace_all(., "[,\\-\\$\\(\\)]|\\..*", ""))))

# function to create indicator columns ----
convert_to_ind <- function(df, field){
    df %>% 
        mutate_(var = field) %>% 
        distinct(id, var) %>% 
        unnest(split = str_split(var, ",")) %>%
        select(-var) %>% 
        filter(!is.na(split)) %>% 
        mutate(n = 1,
               split = 
                   str_replace_all(split, "-", ".") %>% 
                   paste0(field, "_", ., "_ind")) %>%
        distinct() %>% 
        spread(split, n, fill = 0)
}

# demo of difference
apps %>% distinct(id, needs) %>% head()
convert_to_ind(apps, "needs") %>% head()

# apps + new indicator fields
all_indicators <-
    apps %>% 
    distinct(id) %>% 
    left_join(convert_to_ind(apps, "experience")) %>% 
    left_join(convert_to_ind(apps, "pet_kept")) %>% 
    left_join(convert_to_ind(apps, "exercise")) %>% 
    left_join(convert_to_ind(apps, "needs")) %>% 
    left_join(convert_to_ind(apps, "return_pet"))

# master dataset for animal trajectory group ----
master_animal <-
    petpoint %>% 
    left_join(actions_wide) %>% 
    left_join(apps) %>% # has a few dupes
    left_join(all_indicators)

# analysis of all field values ---- 
# suggest fields be collapsed (ex. grew up w/pet has diff wording btw cat & dog apps)
indicator_summary <-
    master_animal %>% 
    select(species, ends_with("_ind")) %>% 
    group_by(species) %>% 
    summarise_all(sum, na.rm = T) %>% 
    ungroup() %>% 
    gather(name, n, -species ) %>% 
    mutate(field = str_extract(name, "[^_]+"),
           value = str_replace_all(name, paste0(field,"_|_ind"), ""),
           value = str_replace_all(value, "[\\._]", " "),
           value = fct_reorder(str_sub(value, 1, 30), n))

ggplot(indicator_summary, aes(value, n, fill = species)) +
    geom_col() +
    facet_wrap(~field, scales = "free_y") +
    scale_fill_manual(values = c("grey60", "orange")) +
    coord_flip()
