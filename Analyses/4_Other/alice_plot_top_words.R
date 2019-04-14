# Return bar plot of top words

plot_top_words <- function(the_tweets){
  
  #regex so that we keep @ and # stuff for twitter
  reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
  
  #remove retweets, remove links, tokenize to words
  tidy_tweets <- the_tweets %>%
    filter(!is_retweet) %>%
    select(screen_name, status_id, text) %>%
    mutate(text = str_replace_all(text, "https?://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
    tidytext::unnest_tokens(word, text, token = "regex", pattern = reg) %>%
    filter(str_detect(word, "[a-z]"))
  
  #remove stop words then plot top frequency words
  tidy_tweets %>%
    filter(!word %in% tidytext::stop_words$word) %>%
    filter(!str_detect(word, "^@")) %>%
    count(word, sort = TRUE) %>%
    head(16) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    coord_flip() +
    labs(y = paste0("Count in dataset"))
}

