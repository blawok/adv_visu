
# -------------------------------------------------------------- WORDCLOUD PLOTTING FUNCTION

plot_wordcloud <- function(df_path = 'data/clean_reddit_df.csv',
                           thread_id = 'ej95ak',
                           use_filtred_data = TRUE) {
  
  library(wordcloud2)
  library(tm)
  library(stringr)       
  library(qdapRegex)     
  library(data.table)
  library(readr)
  library(dplyr)
  
  df <- read.csv(df_path, sep=",", encoding="UTF-8")
  
  if (use_filtred_data) {
    df <- filter(df, df$thread_id == thread_id)  
  }

  text <- str_c(df$clean_body, collapse = "")
  
  # Convert the data into a summary table
  textCorpus <- 
    Corpus(VectorSource(text)) %>%
    TermDocumentMatrix() %>%
    as.matrix()
  
  textCorpus <- sort(rowSums(textCorpus), decreasing=TRUE)
  textCorpus <- data.frame(word = names(textCorpus),
                           freq=textCorpus,
                           row.names = NULL)
  
  # build wordcloud 
  wordcloud <- wordcloud2(data = textCorpus,
                          backgroundColor="white")
  return(wordcloud)
}


# wordcloud <- plot_wordcloud(use_default = TRUE)
# wordcloud

