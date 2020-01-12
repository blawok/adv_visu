library(xts)
library(plotly)
library(tidyr)
library(dplyr)
library(reshape2)
library(syuzhet)




prepareReddit <- function(file_name = "./data/daily_sentiment.csv") {
  df_redd <- read.csv(file_name)
  df_redd$date <- as.Date(df_redd$date)
  df_redd$date <- as.POSIXct(strptime(df_redd$date, "%Y-%m-%d"))
  df_redd$ave_sentiment <- as.numeric(as.character(df_redd$ave_sentiment))
  df_redd$X <- NULL
  qxts <- xts(df_redd[,2], order.by=df_redd[,1])
  write.zoo(qxts,file="./data/reddit_sentiment.csv",index.name="date",row.names=FALSE,col.names=TRUE,sep=",")
}

accumulateBy <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

csvToXTS <- function(csv_file){
  
  zoo_file <- read.zoo(csv_file, sep=",", header = TRUE, tz = "")
  xts_file <- as.xts(zoo_file)
  
  return(xts_file)
}

xtsToDF <- function(xts_file) {
  
  df <- data.frame(xts_file)
  df <- cbind(date_time = rownames(df), df)
  df$ID <- seq.int(nrow(df))
  
  names(df) <- gsub(".Open", "", names(df), fixed = TRUE)
  
  return (df)
}

plotMultiple <- function(df_file, index_1, index_2, index_3) {
  p <- df_file %>%
    select(., date_time, index_1, index_2, index_3) %>% 
    pivot_longer(-date_time, names_to = "variable", values_to = "value") %>% 
    transform(id = as.integer(factor(variable))) %>%
    plot_ly(.,
            x = ~date_time, 
            y = ~value,
            color = ~variable,
            colors = "Dark2",
            yaxis = ~paste0("y", id),
            text = ~paste(date_time, "<br>Close: $", value), 
            hoverinfo = 'text'
    ) %>%
    add_lines() %>%
    subplot(nrows = 3, shareX = TRUE) %>% 
    layout(
      title = "Prices of 3 chosen indices since June 2019",
      xaxis = list(
        type = "Date",
        tickformat = "%Y-%m"
      ))
  
  return (p)
}


getLowerTri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}


reorderCormat <- function(cormat){
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}


getSentiment <- function(threadid) {
  
  df <- read.csv("./data/clean_reddit_df.csv",
                 sep=",",
                 encoding="UTF-8",
                 stringsAsFactors=FALSE)
  
  df1 <- df %>% 
    filter(., thread_id == threadid)
  
  df1$sentiment <- get_nrc_sentiment(df1$clean_body) 
  
  return(df1)
} 

createRadar <- function(df, title) {
  
  
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
  
  p <- plot_ly(
    type = 'scatterpolar',
    fill = 'toself',
    mode = "lines+markers"
  ) %>%
    add_trace(
      r = as.numeric(as.vector(sum_df[3,])),
      theta = as.character(as.vector(names(sum_df))),
      name = title,
      line = list(color = 'rgba(3, 71, 72, .8)',
                  width = 0.6),
      fillcolor = 'rgba(3, 71, 72, .5)',
      marker = list(
        color = 'rgb(3, 71, 72)',
        size = 5
      ) ) %>%
    layout(
      polar = list(
        radialaxis = list(
          visible = T,
          range = c(sum_min, sum_max)
        )
      )
    )
  
  return (p)
  
  
  
}