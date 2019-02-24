library(tidyverse) # data manipulation

# vector of dataframes to create
csv_files <-c("actions", "apps", "cards")

# for loop: union "cat_" and "dog_" versions
for (i in seq_along(csv_files)) {
    
    # name of file
    file_name <- csv_files[i]
    
    # union dataframes
    df <-
        rbind(
            read_csv(paste0("./Data/cat_", file_name, ".csv")),
            read_csv(paste0("./Data/dog_", file_name, ".csv"))
        )
    
    # remove odd column in apps
    if (file_name == "apps") {
        df <- df %>% select(-X1)
    }
    
    # create objects
    assign(file_name, df)

    # save files as "all_..."
    write_csv(get(file_name), paste0("./Data/all_", file_name, ".csv"))
    
}

actions_wide <-
    actions %>% 
    gather(item, result, checklist_ACCT:checklist_VET) %>% 
    group_by(id = data.card.id) %>% 
    mutate(date_start = min(date)) %>% 
    filter(result == TRUE) %>%
    group_by(id, item) %>% 
    filter(date == max(date)) %>% 
    ungroup() %>% 
    mutate(wait = difftime(date, date_start, units = "days"),
           wait = round(as.numeric(wait), 2)) %>% 
    select(-c(date, checklist_seq_num, data.checkItem.state, type, result)) %>% 
    distinct() %>% 
    spread(item, wait) %>% 
    mutate(wday_start = wday(date_start, label = TRUE, abbr = TRUE))

write.csv(actions_wide, "./Data/actions_wide.csv")
