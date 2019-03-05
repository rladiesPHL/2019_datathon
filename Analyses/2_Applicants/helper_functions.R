# function to iterate through unique label names, add a column with for each label name,
# and add TRUE/FALSE value for whether this label included in the labels variable for each row
tidy_labelnames <- function(df, colname) {
  labelnames <- get_unique_labelnames(df[colname])
  output_df <- df
  for (i in 1:length(labelnames)) {
    for (j in 1:nrow(output_df)) {
      output_df[j, paste(colname, gsub(" ", "_", labelnames[i]), sep = "_")] <- FALSE
      if (grepl(paste(labelnames[i],",", sep = ""), output_df[j, colname]) | grepl(paste(labelnames[i],"$", sep = ""), output_df[j, colname])) {
        output_df[j, paste(colname, gsub(" ", "_", labelnames[i]), sep = "_")] <- TRUE
      }
    }
  }
  new_colnames <- setdiff(colnames(output_df), colnames(df))
  labelnames_summary <- get_labelnames_summary(output_df, colname, new_colnames)
  return(list(labelnames = labelnames, 
              new_colnames = new_colnames,
              labelnames_summary = labelnames_summary,
              output_df = output_df))
}

# function to get unique labelnames
get_unique_labelnames <- function(df, colname) {
  labelname_string <- do.call(paste, c(as.list(df[colname]), sep = ","))
  labelname_list <- unlist(strsplit(labelname_string, c(","))) 
  labelname_list <- unique(trimws(labelname_list))
  labelname_list <- labelname_list[labelname_list != ""]
  labelname_list <- labelname_list[labelname_list != "NA"]
  return(labelname_list)
}

# function to get summary of labelnames
get_labelnames_summary <- function(output_df, colname, new_colnames) {
  subset_df <- output_df[names(output_df) %in% new_colnames]
  subset_df_summary <- as.data.frame(colSums(subset_df, na.rm = TRUE))
  subset_df_summary[colname] <- rownames(subset_df_summary)
  colnames(subset_df_summary)[2] <- colname
  colnames(subset_df_summary)[1] <- "count"
  rownames(subset_df_summary) <- NULL
  return(subset_df_summary)
}
