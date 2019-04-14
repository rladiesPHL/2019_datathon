# script to create master_animal ----

library(tidyverse)
library(forcats)
library(lubridate)

# FUNCTIONS -----
# fix dates = turns "12/31/2018 11:59 AM" to "2018-12-31"
convert_date <-  function(x) {
    mdy_hm(x) %>% as.Date()
}

# clean the comma-separated fields
clean_experience <- function(x){
    x %>%
        str_replace_all(., "(grew-up-with)(-pet)", "\\1") %>% 
        #str_replace_all(., "(currently)(-pet)", "\\1") %>% 
        str_replace_all(., "(euthanized)[^,]+", "\\1") %>% 
        str_replace_all(., "had-pet-die", "pet-died-in-care") %>% 
        str_replace_all(., "[^,]*(lived-with-housemate|lived-wit-prev)[^,]*", "past-housemates-pet") %>% 
        str_replace_all(., "currently-pets[^,]*", "current-housemates-pet") %>% 
        str_replace_all(., "(never-lived-)[^,]+", "\\1with") %>% 
        str_replace_all(., "(given-)[^,]*shelter", "\\1to-shelter") %>% 
        str_replace_all(., "(given-)(pet-)?(to-another)", "\\1away") %>% 
        str_replace_all(., "(bred-sold)-a-pet", "bred-sold")
}

clean_exercise <- function(x){
    x %>%
        str_replace_all(., "other-pets|other-cats|another-pet", "other-pets") %>% 
        str_replace_all(., "toy-mice|wand-toys", "toys")
}

clean_return_pet <- function(x){
    x %>%
        str_replace_all(., "counters", "furniture") %>% 
        str_replace_all(., "litter-box-issues", "not-housebroken") %>% 
        str_replace_all(., "scratches-furniture", "destructive")
}

compare_fields <- function(df, x, fun){
    df %>% 
        select(orig = x) %>% 
        filter(!is.na(orig)) %>% 
        unnest(orig = str_split(orig, ",")) %>%
        distinct() %>% 
        mutate(clean = do.call(fun, list(orig))) %>% 
        arrange(clean) %>% 
        mutate(`*` = ifelse(clean != orig, "*", ""))
}

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
                   paste0(str_replace_all(field, "_", "."), 
                          "_", ., "_ind")) %>%
        distinct() %>% 
        spread(split, n, fill = 0)
}

# DATA MANIPULATION -----
petpoint <-
  read.csv("Data/petpoint.csv", na.strings = c("", NA)) %>%
  select(-c(
    X, animal_type, markings, dob,
    STATEFP:INTPTLON
  )) %>%
  rename(id = outcome_trello_id) %>%
  filter(!is.na(id)) %>%
  filter(species != "Wild Mammal") %>%
  filter(outcome_type != "Admin Missing") %>%
  mutate_at(vars(matches("date")), funs(convert_date)) %>%
  mutate(
    wait_days = as.integer(difftime(release_date, intake_date, units = "days")),
    wday_intake = wday(intake_date, abbr = TRUE, label = TRUE)
  )
actions_wide <-
    read.csv("Data/processed_data/actions_wide.csv")

apps <-
    read.csv("Data/processed_data/all_apps.csv") %>% 
    select(-c(STATEFP:ZIP)) %>% 
    rename(id = outcome_trello_id) %>% 
    filter(!is.na(id)) %>% 
    mutate(date_submitted = mdy(date_submitted),
           clean_experience = clean_experience(experience),
           clean_exercise = clean_exercise(exercise),
           clean_return = clean_return_pet(return_pet)) %>% 
    mutate_at(vars(contains("alone")),
              funs(as.integer(str_extract(., "\\d+")))) %>% 
    mutate_at(vars(contains("budget")),
              funs(as.integer(str_replace_all(., "[,\\-\\$\\(\\)]|\\..*", ""))))

# EXAMPLE use of functions ----

# create_indicator function
apps %>% distinct(id, needs) %>% head(20)
convert_to_ind(apps, "return_pet") %>% head()

# clean_ . . . functions
compare_fields(apps, "experience", clean_experience)
compare_fields(apps, "exercise", clean_exercise)
compare_fields(apps, "return_pet", clean_return_pet)

# COMBINE apps + new indicator fields ----
all_indicators <-
    apps %>% 
    distinct(id) %>% 
    left_join(convert_to_ind(apps, "reason_for_adoption")) %>% 
    left_join(convert_to_ind(apps, "clean_experience")) %>% 
    left_join(convert_to_ind(apps, "pet_kept")) %>% 
    left_join(convert_to_ind(apps, "clean_exercise")) %>% 
    left_join(convert_to_ind(apps, "needs")) %>% 
    left_join(convert_to_ind(apps, "clean_return")) %>% 
    rename_all(funs(str_replace(., "clean.", ""))) %>% 
    mutate(return_ind = as.numeric(return_none_ind == 0)) %>% 
    select(-return_none_ind)

# MASTER_ANIMAL master dataset for animal trajectory group ----
master_animal <-
    petpoint %>% 
    left_join(actions_wide) %>% 
    left_join(apps) %>% # has a few dupes
    left_join(all_indicators) %>% 
    select(-matches("^(needs|return|reason|experience|exercise|pet_kept)$"),
           -matches("clean_")) %>% 
    select(id, everything())

write.csv(master_animal, "Data/processed_data/master_animal.csv", row.names = FALSE)

# INDICATOR SUMMARY ---- 
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

# PLOT INDICATORS ----
ggplot(indicator_summary, aes(value, n, fill = species)) +
    geom_col() +
    facet_wrap(~field, scales = "free_y") +
    scale_fill_manual(values = c("grey60", "orange")) +
    coord_flip()
