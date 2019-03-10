tidy_elements <- function(df, colname) {
  elements <- get_unique_elements(df[colname])
  output_df <- df
  for (i in 1:length(elements)) {
    new_colname <- paste(colname, gsub(" |-", "_", elements[i]), sep = "_")
    output_df[new_colname] <- FALSE
    output_df[new_colname] <- str_detect(output_df[[colname]],
                                         paste0(elements[i], ",")) |
                              str_detect(output_df[[colname]],
                                         paste0(elements[i], "$"))
  }
  new_colnames <- setdiff(colnames(output_df), colnames(df))
  elements_summary <- get_elements_summary(output_df, colname, new_colnames)
  return(list(elements = elements, 
              new_colnames = new_colnames,
              elements_summary = elements_summary,
              output_df = output_df))
}

get_unique_elements <- function(df, colname) {
  elements_string <- do.call(paste, c(as.list(df[colname]), sep = ","))
  elements_list <- unique(trimws(unlist(strsplit(elements_string, c(",")))))
  unique_elements <- elements_list[!elements_list %in% c("","NA")]
  return(unique_elements)
}

get_elements_summary <- function(output_df, colname, new_colnames) {
  subset_df <- output_df[names(output_df) %in% new_colnames]
  elements_summary <- subset_df %>%
    summarise_all(sum, na.rm = TRUE) %>%
    gather(!!colname, "count")
  return(elements_summary)
}