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

summed <- rowSums(sum_df)

sum_df <- sum_df/summed

sum_min <- min(sum_df[1,], na.rm = T)
sum_max <- max(sum_df[1,], na.rm = T)
sum_dim <- ncol(sum_df)

sum_df <- rbind(rep(sum_max, sum_dim), rep(sum_min, sum_dim), sum_df)



# 
# 
# df2 <- read.csv('data_scrape/ekad1u.csv',
#                sep="\t",
#                encoding="UTF-8",
#                stringsAsFactors=FALSE)
# 
# 
# # 10-element sentiment analysis
# df2$sentiment <- get_nrc_sentiment(df2$body) 
# 
# df21 <- df2 %>%
#   select(sentiment)  %>% 
#   do.call(data.frame, .) %>% 
#   .[, -which(names(.) %in% c("sentiment.positive", "sentiment.negative"))]
# 
# names(df21) <- gsub("sentiment.", "", names(df21), fixed = TRUE)
# 
# sum_df2 <- df21 %>%
#   summarize_if(is.numeric, sum, na.rm=TRUE)
# 
# summed2 <- rowSums(sum_df2)
# 
# sum_df2 <- sum_df2/summed2
# 
# sum2_min <- min(sum_df2[1,], na.rm = T)
# sum2_max <- max(sum_df2[1,], na.rm = T)
# sum2_dim <- ncol(sum_df2)
# 
# sum_df2 <- rbind(rep(sum2_max, sum2_dim), rep(sum2_min, sum2_dim), sum_df2)


# 1 sposob na radar chart (bedzie mozna go wrzucic do subplota w plotly, a drugi nie wiem czy sie da)
p <- plot_ly(
  type = 'scatterpolar',
  fill = 'toself',
  mode = "lines"
) %>%
  add_trace(
    r = as.numeric(as.vector(sum_df[3,])),
    theta = as.character(as.vector(names(sum_df))),
    name = 'Irans bombing',
    line = list(color = 'rgba(152, 0, 0, .8)',
                width = 2))  %>%
  # add_trace(
  #   r = as.numeric(as.vector(sum_df2[3,])),
  #   theta = as.character(as.vector(names(sum_df2))),
  #   name = 'People understanding'
  # ) %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(min(sum_min, sum2_min), max(sum_max, sum2_max) + 0.1)
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
