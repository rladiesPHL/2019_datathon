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
