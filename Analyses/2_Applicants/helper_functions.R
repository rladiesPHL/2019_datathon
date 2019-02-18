# tidy_paws_labels function ----
# function to iterate through unique label names, add a column with for each label name,
# and add TRUE/FALSE value for whether this label included in the labels variable for each row
tidy_paws_labels <- function(df, colname, labelnames) {
  output_df <- df
  for (i in 1:length(labelnames)) {
    output_df <- mutate(output_df, !!gsub(" ", "_", labelnames[i]) := FALSE)
    for (j in 1:nrow(output_df)) {
      if (grepl(paste(labelnames[i],",", sep = ""), output_df[j, colname]) | grepl(paste(labelnames[i],"$", sep = ""), output_df[j, colname])) {
        output_df[j, gsub(" ", "_", labelnames[i])] <- TRUE
      }
    }
  }
  return(output_df)
}