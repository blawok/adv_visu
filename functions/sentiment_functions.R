
# ---------------------------------------------------------------- SENTIMENT COMPUTING FUNCTION

compute_sentiment <- function(df,
                              dest_path_timestamp = 'df_with_sentiment_timestamp.csv',
                              dest_path_daily = 'daily_sentiment.csv') {
  
  library(sentimentr)
  library(tidytext)
  library(syuzhet)
  library(fmsb)
  library(dplyr)
  
  # ------------------------------------------- COMPUTE SENTIMENT
  sentiment = sentiment_by(df$body)
  summary(sentiment$ave_sentiment)
  
  # plot histogram of sentiment
  qplot(sentiment$ave_sentiment,
        geom="histogram",
        binwidth=0.1,
        main="Posts Sentiment Histogram")
  
  # add sentiment column to dataframe
  df$ave_sentiment=sentiment$ave_sentiment
  df$sd_sentiment=sentiment$sd
  
  # save df to csv file
  write.csv(df,
            dest_path_timestamp,
            row.names = TRUE)
  
  # create date column without hours
  df$date <- substr(df$timestamp, 1, 10)
  
  # group sentiment by day 
  df_sentiment_by_day <- aggregate(ave_sentiment ~ date, df, mean)
  df_sentiment_by_day$date <- as.Date(df_sentiment_by_day$date)
  # save it to a csv file
  write.csv(df_sentiment_by_day,
            dest_path_daily,
            row.names = TRUE)
  
}


# ---------------------------------------------------------------- SENTIMENT PLOTTING FUNCTION

plot_sentiment <- function(df = NULL, use_default = FALSE) {
  
  library(ggplot2)
  
  if (use_default) {
    df <- read.csv("daily_avg_sentiment.csv")
  }
  
  df$date <- as.Date(df$date)
  df$date <-format(as.Date(strptime(df$timestamp, '%Y-%M-%D %H:%M:%S')), "%b %d %Y")
  
  ggplot(df, aes(x=df$date, y=df$ave_sentiment, group=1)) +
    geom_point()+
    geom_line() +
    scale_x_date() +
    xlab("") +
    ylab("Average Sentiment")
}

# plot_sentiment(df)

# ---------------------------------------------------------------- SENTIMENT HISTOGRAM PLOTTING FUNCTION

plot_hist_sentiment <- function(df, by) {
  
  library(ggplot2)
  
  ggplot(df, aes(x = by)) +
    geom_histogram(aes(y = ..density..)) +
    stat_function(fun = dnorm, colour = "red",
                  arg = list(mean = mean(df$by, na.rm = TRUE),
                             sd = sd(df$by, na.rm = TRUE)))
}

# plot_sentiment(df)