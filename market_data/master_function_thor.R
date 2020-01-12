library(plotly)
library(xts)
library(dplyr)
library(tidyr)
library(zoo)

# Getting Reddit csv to work
prepareReddit <- function(file_name = "../data/daily_sentiment.csv") {
  df_redd <- read.csv(file_name)
  df_redd$date <- as.Date(df_redd$date)
  df_redd$date <- as.POSIXct(strptime(df_redd$date, "%Y-%m-%d"))
  df_redd$ave_sentiment <- as.numeric(as.character(df_redd$ave_sentiment))
  df_redd$X <- NULL
  qxts <- xts(df_redd[,2], order.by=df_redd[,1])
  write.zoo(qxts,file="reddit_sentiment.csv",index.name="date",row.names=FALSE,col.names=TRUE,sep=",")
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

xts1 <- csvToXTS("merged_indices.csv")
index(xts1) <- round(index(xts1), "days")

xts_reddit <- csvToXTS("reddit_sentiment.csv")

xts_reddit <- apply.daily(xts_reddit, sum)
colnames(xts_reddit) <- c("Sentiment")
xts_merged <- merge.xts(xts1, xts_reddit) %>% 
  na.locf(.) %>%
  na.omit()

xts_merged_small <- merge.xts(xts1, xts_reddit) %>% 
  na.omit()

xtsToDF <- function(xts_file) {
  
  df <- data.frame(xts_file)
  df <- cbind(date_time = rownames(df), df)
  df$ID <- seq.int(nrow(df))
  
  names(df) <- gsub(".Open", "", names(df), fixed = TRUE)
  
  return (df)
}

dfm <- xtsToDF(xts_merged)
dfm_small <- xtsToDF(xts_merged_small)

plotThreeLines <- function(df_file, index_1, index_2, index_3) {
  p <- df_file %>%
    select(., date_time, index_1, index_2, index_3) %>% 
    pivot_longer(-date_time, names_to = "variable", values_to = "value") %>% 
    transform(id = as.integer(factor(variable))) %>%
    plot_ly(.,
      x = ~date_time, 
      y = ~value,
      color = ~variable,
      colors = "Dark2",
      yaxis = ~paste0("y", id)
    ) %>%
    add_lines() %>%
    subplot(nrows = 3, shareX = TRUE, shareY = FALSE) %>% 
    layout(
      title = "Prices of 3 chosen indices since June 2019" )
 
  return (p)
}
    
plotThreeLines(dfm, "GE", "ZG", "LMT")

plotThreeLines(dfm_small, "GE", "HAL", "Sentiment")
