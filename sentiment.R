df <- read.csv('data_scrape/reddit_posts_all.csv',
               sep="\t",
               encoding="UTF-8",
               stringsAsFactors=FALSE)

# install.packages("sentimentr")
install.packages("fmsb")
library(sentimentr)
library(tidytext)
library(syuzhet)
library(fmsb)
library(dplyr)
library(ggplot2)
library(plotly)


# ------------------------------------------- COMPUTE SENTIMENT
sentiment=sentiment_by(df$body)
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
          "df_with_sentiment.csv",
          row.names = TRUE)

# plot sentiment in time (timestamp data)
plot(df$timestamp, df$ave_sentiment)

# create date column without hours
df$date <- substr(df$timestamp, 1, 10)
 

# group sentiment by day 
df_sentiment_by_day <- aggregate(ave_sentiment ~ date, df, mean)
df_sentiment_by_day$date <- as.Date(df_sentiment_by_day$date)
# save it to a csv file
write.csv(df,
          "daily_avg_sentiment.csv",
          row.names = TRUE)

# read csv and plot time series of daily, average sentiment
df_sentiment_by_day <- read.csv("daily_avg_sentiment.csv")
df_sentiment_by_day$date <- as.Date(df_sentiment_by_day$date)
ggplot(df_sentiment_by_day, aes(x=df_sentiment_by_day$date, y=df_sentiment_by_day$ave_sentiment,group=1)) +
  geom_point()+
  geom_line() +
  scale_x_date() +
  xlab("") +
  ylab("Average Sentiment")


# sentiment ploting function
plot_sentiment <- function(df = NULL, use_default = FALSE) {
  library(ggplot2)
  
  if (use_default) {
    df <- read.csv("daily_avg_sentiment.csv")
  }
  
  df$date <- as.Date(df$date)
  
  ggplot(df, aes(x=df$date, y=df$ave_sentiment, group=1)) +
    geom_point()+
    geom_line() +
    scale_x_date() +
    xlab("") +
    ylab("Average Sentiment")
}

plot_sentiment(df)


