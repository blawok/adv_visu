
# ------------------------------------------------------------------- TEXT CLEANING  FUNCTIONS

clean_text <- function(df_path = 'data_scrape/reddit_posts_all.csv',
                       dest_text_file_path = 'clean_text/new_cleaned_text.txt') {
  
  library(tm)
  library(stringr)       
  library(qdapRegex)     
  library(data.table)
  library(readr)
  
  df <- read.csv(df_path, sep="\t")
  text <- str_c(df$body, collapse = "")
  
  text <-
    text %>%
    str_remove("\\n") %>%                   # remove linebreaks
    rm_twitter_url() %>%                    # Remove URLS
    rm_url() %>%
    str_remove_all("#\\S+") %>%             # Remove any hashtags
    str_remove_all("@\\S+") %>%             # Remove any @ mentions
    removeWords(stopwords("english")) %>%   # Remove common words (a, the, it etc.)
    removeNumbers() %>%
    stripWhitespace() %>%
    removeWords(c("amp"))
  
  writeLines(text, dest_text_file_path)
  text <- read_file(dest_text_file_path)
  text_fixed <- str_replace_all(text,"[^[:graph:]]", " ") 
  
  # Convert the data into a summary table
  textCorpus <- 
    Corpus(VectorSource(text_fixed)) %>%
    TermDocumentMatrix() %>%
    as.matrix()
  
  textCorpus <- sort(rowSums(textCorpus), decreasing=TRUE)
  textCorpus <- data.frame(word = names(textCorpus),
                           freq=textCorpus,
                           row.names = NULL)
  
  return(textCorpus)
}


# -------------------------------------------------------------- WORDCLOUD PLOTTING FUNCTION

plot_wordcloud <- function(text = NULL,
                           use_default = TRUE,
                           df_path = NULL,
                           dest_text_file_path = NULL) {
  
  library(wordcloud2)
  
  if (use_default) {
    
    library(tm)
    library(stringr)       
    library(qdapRegex)     
    library(data.table)
    library(readr)
    
    text <- read_file("cleaned_text.txt")
    
    
    text_fixed <- str_replace_all(text,"[^[:graph:]]", " ") 
    
    # Convert the data into a summary table
    textCorpus <- 
      Corpus(VectorSource(text_fixed)) %>%
      TermDocumentMatrix() %>%
      as.matrix()
    
    textCorpus <- sort(rowSums(textCorpus), decreasing=TRUE)
    textCorpus <- data.frame(word = names(textCorpus),
                             freq=textCorpus,
                             row.names = NULL)
  } else {
    
    textCorpus = clean_text(df_path = df_path,
                            dest_text_file_path = dest_text_file_path)
  }
  
  # build wordcloud 
  wordcloud <- wordcloud2(data = textCorpus,
                          backgroundColor="black")
  return(wordcloud)
}


wordcloud <- plot_wordcloud(use_default = TRUE)
wordcloud

