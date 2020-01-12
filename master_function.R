
library(ggplot)
library(dplyr)
library(plotly)
library(readr)

source('functions/sentiment_functions.R')
source('functions/wordclouds_functions.R')

################################################################################
prepare_data_for_wordcloud(df_path = 'data/clean_reddit_df.csv',
                                 thread_id = 'ej95ak',
                                 use_filtred_data = TRUE)

prepare_data_for_wordcloud(df_path = 'data/clean_reddit_df.csv',
                           thread_id = 'full_data',
                           use_filtred_data = FALSE)

# WORDCLOUD FOR ALL DATA
wordcloud <- plot_wordcloud(file_id = 'full_data',
                            use_image = FALSE)
wordcloud

# WORDCLOUD FOR SPECIFIC ARTICLE
wordcloud_article <- plot_wordcloud(file_id = 'ej95ak',
                                    use_image = FALSE)
wordcloud_article


################################################################################
# PLOTTING SENTIMENT

### timestamp data 
sentiment <- read_csv('data/df_with_sentiment_timestamp.csv')

# choosing only posts from specific thread
sentiment <- filter(sentiment, sentiment$thread_id == 'ej95ak')  

a <- list(
  title = "Sentiment",
  range = c(-1, 1)
)
sentiment_histogram <- plot_ly(x=~sentiment$ave_sentiment,
             type = "histogram",
             histnorm = "probability",
             alpha = 0.8) %>% layout(xaxis = a)


save(sentiment, file = "objects/sentiment_timestamp.RData")
save(sentiment_histogram, file = "objects/sentiment_histogram.RData")
load("objects/sentiment_timestamp.RData")
load("objects/sentiment_histogram.RData")
sentiment_histogram

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
