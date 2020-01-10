library(syuzhet)
library(dplyr)
library(plotly)

df <- read.csv('data_scrape/c35akk.csv',
               sep="\t",
               encoding="UTF-8",
               stringsAsFactors=FALSE)


# 10-element sentiment analysis
df$sentiment <- get_nrc_sentiment(df$body) 

df1 <- df %>%
  select(sentiment)  %>% 
  do.call(data.frame, .) %>% 
  .[, -which(names(.) %in% c("sentiment.positive", "sentiment.negative"))]

names(df1) <- gsub("sentiment.", "", names(df1), fixed = TRUE)

sum_df <- df1 %>%
  summarize_if(is.numeric, sum, na.rm=TRUE)

sum_min <- min(sum_df[1,], na.rm = T)
sum_max <- max(sum_df[1,], na.rm = T)
sum_dim <- ncol(sum_df)

sum_df <- rbind(rep(sum_max + 1000, sum_dim), rep(sum_min - 1000, sum_dim), sum_df)

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
        range = c(sum_min - 1000,sum_max + 1000)
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
