
prepare_data_for_wordcloud <- function(df_path = 'data/clean_reddit_df.csv',
                                       thread_id = 'ej95ak',
                                       use_filtred_data = TRUE) {
  
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
  
  save(textCorpus, file = paste("objects/", thread_id, "_text_corpus.RData", sep=""))
  
}


# -------------------------------------------------------------- WORDCLOUD PLOTTING FUNCTION

plot_wordcloud <- function(file_id = 'ej95ak',
                           use_image = FALSE) {
  
  load(paste("objects/",file_id,"_text_corpus.RData", sep=""))
  
  # build wordcloud 
  if (use_image) {
    wordcloud <- wordcloud2(textCorpus, 
                             figPath = "images/iran.png", 
                             size = 1, 
                             color = "skyblue", 
                             backgroundColor="black")
  } else {
    wordcloud <- wordcloud2(data = textCorpus,
                            backgroundColor="white")
  }

  return(wordcloud)
}


# wordcloud <- plot_wordcloud(use_default = TRUE)
# wordcloud


