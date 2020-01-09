df <- read.csv('data_scrape/reddit_posts_all.csv',
               sep="\t",
               encoding="UTF-8",
               stringsAsFactors=FALSE)

# install.packages("sentimentr")
install.packages("syuzhet")
install.packages("fmsb")
library(sentimentr)
library(tidytext)
library(syuzhet)
library(fmsb)
library(dplyr)
library(ggplot2)
library(plotly)

sentiment=sentiment_by(df$body)
summary(sentiment$ave_sentiment)


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


# 10-element sentiment analysis
df$sentiment <- get_nrc_sentiment(df$body) 

df1 <- df %>%
      select(sentiment) %>% 
      do.call(data.frame, .)

names(df1) <- gsub("sentiment.", "", names(df1), fixed = TRUE)


sum_df <- df1 %>%
  summarize_if(is.numeric, sum, na.rm=TRUE)

sum_df <- rbind(rep(18000, 10), rep(3000, 10), sum_df)

# 1 sposob na radar chart (bedzie mozna go wrzucic do subplota w plotly, a drugi nie wiem czy sie da)
p <- plot_ly(
            type = 'scatterpolar',
            fill = 'toself',
            mode = "lines"
          ) %>%
            add_trace(
              r = as.numeric(as.vector(sum_df[3,])),
              theta = as.character(as.vector(names(sum_df))),
              name = 'Posts'
            ) %>%
            layout(
              polar = list(
                radialaxis = list(
                  visible = T,
                  range = c(3000,18000)
                )
              )
            )

p

# 2 sposob
radarchart(sum_df  , axistype=1 , 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
            
            #custom labels
            vlcex=0.8 
)
