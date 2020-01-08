df <- read.csv('data_scrape/reddit_posts_all.csv',
               sep="\t",
               encoding="UTF-8",
               stringsAsFactors=FALSE)

install.packages("sentimentr")
library(sentimentr)


sentiment=sentiment_by(df$body)
summary(sentiment$ave_sentiment)

library(ggplot2)
qplot(sentiment$ave_sentiment,
      geom="histogram",
      binwidth=0.1,
      main="Posts Sentiment Histogram")


df$ave_sentiment=sentiment$ave_sentiment
df$sd_sentiment=sentiment$sd
write.csv(df,
          "df_with_sentiment.csv",
          row.names = TRUE)

plot(df$timestamp, df$ave_sentiment)

df$date <- substr(df$timestamp, 1, 10)
 
df_sentiment_by_day <- aggregate(ave_sentiment ~ date, df, mean)
df_sentiment_by_day$date <- as.Date(df_sentiment_by_day$date)
write.csv(df,
          "daily_avg_sentiment.csv",
          row.names = TRUE)

ggplot(df_sentiment_by_day, aes(x=date, y=ave_sentiment,group=1)) +
  geom_point()+
  geom_line() +
  scale_x_date() +
  xlab("") +
  ylab("Average Sentiment")
