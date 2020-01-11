library(syuzhet)
library(dplyr)
library(plotly)


getSentiment <- function(file_name) {
  
  df <- read.csv(file_name,
                 sep="\t",
                 encoding="UTF-8",
                 stringsAsFactors=FALSE)
  
  df$sentiment <- get_nrc_sentiment(df$body) 
  
  return(df)
} 

createRadar <- function(df, thread_id, title) {

  
  df1 <- df %>%
    filter(thread_id == thread_id) %>% 
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
  
  p <- plot_ly(
    type = 'scatterpolar',
    fill = 'toself',
    mode = "lines+markers"
  ) %>%
    add_trace(
      r = as.numeric(as.vector(sum_df[3,])),
      theta = as.character(as.vector(names(sum_df))),
      name = title,
      line = list(color = 'rgba(51, 40, 155, .8)',
                  width = 0.6),
      fillcolor = 'rgba(51, 40, 155, 0.5)',
      marker = list(
        color = 'rgb(51, 40, 155)',
        size = 5
        ) ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(min(sum_min, sum2_min), max(sum_max, sum2_max) + 0.1)
        )
      )
    )
  
  return (p)
  
  
  
}

getSentiment('data_scrape/c35akk.csv')

createRadar(df_5, "Irans stuff")

