
library(ggplot)
library(dplyr)
library(plotly)
library(readr)

source('functions/sentiment_functions.R')
source('functions/wordclouds_functions.R')

################################################################################
# WORDCLOUD FOR ALL DATA
wordcloud <- plot_wordcloud(df_path = 'data/clean_reddit_df.csv',
                            use_filtred_data = FALSE)
wordcloud

# WORDCLOUD FOR SPECIFIC ARTICLE
wordcloud_article <- plot_wordcloud(df_path = 'data/clean_reddit_df.csv',
                                    thread_id = 'ej95ak',
                                    use_filtred_data = TRUE)
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
plot_ly(x=~sentiment$ave_sentiment,
             type = "histogram",
             histnorm = "probability",
             alpha = 0.8) %>% layout(xaxis = a)


### daily data
daily_sentiment <- read_csv('data/daily_sentiment.csv')

a <- list(
  title = "Sentiment",
  range = c(-1, 1)
)
plot_ly(x=~daily_sentiment$ave_sentiment,
        type = "histogram",
        histnorm = "probability",
        alpha = 0.8) %>% layout(xaxis = a)



