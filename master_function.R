
library(ggplot2)
library(dplyr)
library(plotly)
library(readr)

source('functions/sentiment_functions.R')
source('functions/wordclouds_functions.R')


  
  library(tm)
  library(stringr)       
  library(qdapRegex)     
  library(data.table)
  library(readr)
  library(dplyr)
  
  df <- read.csv('data/clean_reddit_df.csv', sep=",", encoding="UTF-8")
  
  # if (use_filtred_data) {
  df <- filter(df, df$thread_id == 'c35akk')  
  # }
  
  text <- str_c(df$clean_body, collapse = "")
  
  
  # install.packages("tm")
  library(tm)
  # print(stopwords('english'))
  
  sw <- c('that', 'the', 'trump', 'like','war', 'iran', 'people', 'get', 'also', 'even', 'wars', 'what')
  
  # Convert the data into a summary table
  textCorpus <- 
    Corpus(VectorSource(text)) %>%
    tm_map(., content_transformer(tolower)) %>% 
    tm_map(., removeWords, stopwords("english")) %>% 
    tm_map(., removeWords, sw) %>% 
    TermDocumentMatrix() %>%
    as.matrix()
  
  textCorpus <- sort(rowSums(textCorpus), decreasing=TRUE)
  textCorpus <- data.frame(word = names(textCorpus),
                           freq=textCorpus,
                           row.names = NULL)
  
  save(textCorpus, file = paste("objects/", 'c35akk_clean', "_text_corpus.RData", sep=""))
  



################################################################################
prepare_data_for_wordcloud(df_path = 'data/clean_reddit_df.csv',
                                 thread_id = 'ej95ak',
                                 use_filtred_data = TRUE)

prepare_data_for_wordcloud(df_path = 'data/clean_reddit_df.csv',
                           thread_id = 'full_data',
                           use_filtred_data = FALSE)

prepare_data_for_wordcloud(df_path = 'data/clean_reddit_df.csv',
                           thread_id = 'ej7ykn',
                           use_filtred_data = FALSE)


library(wordcloud2)

# stary
load(paste("objects/",'ccednx',"_text_corpus.RData", sep=""))
wordcloud2(data = textCorpus,backgroundColor="white")

load(paste("objects/",'ccednx_clean',"_text_corpus.RData", sep=""))
wordcloud2(data = textCorpus,backgroundColor="white")

load(paste("objects/",'c35akk_clean',"_text_corpus.RData", sep=""))
wordcloud2(data = textCorpus,backgroundColor="white")

# nowy
load(paste("objects/",'ej95ak',"_text_corpus.RData", sep=""))
wordcloud2(data = textCorpus,backgroundColor="white")

load(paste("objects/",'ej95ak_clean',"_text_corpus.RData", sep=""))
wordcloud2(data = textCorpus,backgroundColor="white")

load(paste("objects/",'ej7ykn_clean',"_text_corpus.RData", sep=""))
wordcloud2(data = textCorpus,backgroundColor="white")

# all
load(paste("objects/",'full_data',"_text_corpus.RData", sep=""))
wordcloud2(data = textCorpus,backgroundColor="white")






# WORDCLOUD FOR ALL DATA
wordcloud <- plot_wordcloud(file_id = 'ccednx',
                            use_image = FALSE)
wordcloud

# WORDCLOUD FOR SPECIFIC ARTICLE
wordcloud_article <- plot_wordcloud(file_id = 'ej95ak',
                                    use_image = FALSE)
wordcloud_article


################################################################################
# PLOTTING SENTIMENT



### timestamp data 
sentiment_n <- read_csv('data/df_with_sentiment_timestamp.csv')

# choosing only posts from specific thread
sentiment_new <- filter(sentiment_n, sentiment_n$thread_id == 'ej7ykn')  

a <- list(
  title = "Sentiment",
  range = c(-1, 1)
)
sentiment_histogram_new <- plot_ly(x=~sentiment_new$ave_sentiment,
             type = "histogram",
             histnorm = "probability",
             alpha = 0.8) %>% layout(xaxis = a)


save(sentiment_n, file = "objects/sentiment_n.RData")
save(sentiment_new, file = "objects/sentiment_timestamp_new.RData")
save(sentiment_histogram_new, file = "objects/sentiment_histogram_new.RData")
load("objects/sentiment_n.RData")
load("objects/sentiment_timestamp_new.RData")
load("objects/sentiment_histogram_new.RData")
sentiment_histogram_new






### timestamp data 
sentiment_o <- read_csv('data/df_with_sentiment_timestamp.csv')

# choosing only posts from specific thread
sentiment_old <- filter(sentiment_o, sentiment_o$thread_id == 'c35akk')  

a <- list(
  title = "Sentiment",
  range = c(-1, 1)
)
sentiment_histogram_old <- plot_ly(x=~sentiment_old$ave_sentiment,
                                   type = "histogram",
                                   histnorm = "probability",
                                   alpha = 0.8) %>% layout(xaxis = a)


save(sentiment_o, file = "objects/sentiment_o.RData")
save(sentiment_old, file = "objects/sentiment_timestamp_old.RData")
save(sentiment_histogram_old, file = "objects/sentiment_histogram_old.RData")

save(sentiment_o,sentiment_old,sentiment_histogram_old, file ='objects/sent_hist_old.RData')
saveRDS(sentiment_o,sentiment_old,sentiment_histogram_old, file = 'objects/sent_hist_old.rds')
# load("objects/sentiment_o.RData")
# load("objects/sentiment_timestamp_old.RData")
# load("objects/sentiment_histogram_old.RData")

load("objects/sent_hist_old.RData")



saveRDS(sentiment_o, file = 'objects/sentiment_o.rds')
saveRDS(sentiment_old, file = 'objects/sentiment_old.rds')
saveRDS(sentiment_histogram_old, file = 'objects/sentiment_hist_old.rds')
sentiment_o <- readRDS('objects/sentiment_o.rds')
sentiment_old <- readRDS('objects/sentiment_old.rds')
sentiment_histogram_old <- readRDS('objects/sentiment_hist_old.rds')
sentiment_histogram_old























### daily data
daily_sentiment <- read_csv('data/daily_sentiment.csv')

a <- list(
  title = "Sentiment",
  range = c(-1, 1)
)
daily_sentiment_histogram <- plot_ly(x=~daily_sentiment$ave_sentiment,
        type = "histogram",
        histnorm = "probability",
        alpha = 0.8) %>% layout(xaxis = a)



################################################################################
# CORRELATION

library(ggplot2)
library(plotly)
load( "objects/corr_plot_data.RData")
load( "objects/corr_plot.RData")
correlation_plot



################################################################################
# TRIPLE UNDERLYING PLOT

load("objects/triple_underlying_plot.RData")
triple_underlying_plot
