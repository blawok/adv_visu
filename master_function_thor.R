library(plotly)
library(xts)
library(dplyr)
library(tidyr)
source("./functions/functions_thor.R")

##############################################################
# I Multiple plots with Sentiment
##############################################################

xts1 <- csvToXTS("./data/merged_indices.csv")
index(xts1) <- round(index(xts1), "days")

xts_reddit <- csvToXTS("./data/reddit_sentiment.csv")

xts_reddit <- apply.daily(xts_reddit, sum)
colnames(xts_reddit) <- c("Sentiment")

# write.zoo(xts_reddit,file="reddit_xts.csv",index.name="date",row.names=FALSE,col.names=TRUE,sep=",")

xts_merged_small <- merge.xts(xts1, xts_reddit) %>% 
  na.omit()

xts_corr <- merge.xts(xts1, xts_reddit) %>% 
  na.locf(.) %>% 
  na.omit()

dfm <- xtsToDF(xts1)
dfm_small <- xtsToDF(xts_merged_small)

df_corr <- xtsToDF(xts_corr)

    
plotMultiple(dfm, "GE", "ZG", "LMT")

plotMultiple(dfm_small, "GE", "HAL", "Sentiment")

##############################################################
# II part - self-drawing plots with plotly
##############################################################

z <- read.zoo("./data/merged_indices.csv", sep=",", header = TRUE, tz = "")
data_x <- as.xts(z)

df_x <- data.frame(data_x)
df_x <- cbind(date_time = rownames(df_x), df_x)
df_x$ID <- seq.int(nrow(df_x))



df_x_acc <- df_x %>%
  accumulateBy(~ID)


# write.zoo(data_x,file="merged_indices.csv",index.name="date",row.names=FALSE,col.names=TRUE,sep=",")

r <- read.zoo("./data/reddit_xts.csv", sep=",", header = TRUE, tz = "")
data_r <- as.xts(r)
df_r <- data.frame(data_r)
df_r <- cbind(date_time = rownames(df_r), df_r)
df_r$ID <- seq.int(nrow(df_r))

df_r_acc <- df_r %>%
  accumulateBy(~ID)

triple_underlying_plot <- df_x_acc %>%
  plot_ly(
    x = ~ID, 
    y = ~XAR.Open, 
    frame = ~frame,
    type = 'scatter', 
    mode = 'lines',
    name="OpenXAR",
    line = list(color = 'rgb(114, 186, 59)'),
    text = ~paste(date_time, "<br>Close: $", XAR.Open), 
    hoverinfo = 'text'
  )  %>% 
  add_trace(
    y = ~ZG.Open, 
    name = 'Open',
    line = list(color = 'rgb(153, 186, 81)'),
    mode = 'lines',
    text = ~paste(date_time, "<br>Open: $", ZG.Open) 
  ) %>%
  add_trace(
    y = ~GE.Open, 
    name = 'Open', 
    mode = 'lines',
    line = list(color = 'rgb(100, 82, 159)'),
    text = ~paste(date_time, "<br>Close: $", GE.Open) 
  ) %>%
  add_trace(
    y = ~EUR.USD, 
    name = 'EUR/USD', 
    mode = 'lines',
    line = list(color = 'rgb(163, 26, 59)'),
    text = ~paste(date_time, "<br>Close: $", EUR.USD) 
  ) %>%
  layout(
    title = "Values of chosen underlyings since July 2019",
    yaxis = list(
      title = "Close", 
      range = c(30,60), 
      zeroline = F,
      tickprefix = "$"
    ),
    xaxis = list(
      title = "Day", 
      range = c(0,130), 
      zeroline = F, 
      showgrid = F
    )
  ) %>% 
  animation_opts(
    frame = 40, 
    transition = 7, 
    redraw = FALSE
  ) %>%
  animation_slider(
    currentvalue = list(
      prefix = "Day "
    )
  )

sentiment_plot <- df_r_acc %>%
  plot_ly(
    x = ~ID, 
    y = ~data_r, 
    frame = ~frame,
    type = 'scatter', 
    mode = 'lines',
    name= "Sentiment",
    line = list(color = 'rgb(114, 186, 59)'),
    text = ~paste(date_time, "<br>Value:", data_r), 
    hoverinfo = 'text'
  ) %>%
  layout(
    title = "Sentiment value since May 2019",
    yaxis = list(
      title = "Sentiment value", 
      range = c(-0.8,0.8), 
      zeroline = F
    ),
    xaxis = list(
      title = "Day",
      range = c(0,72), 
      zeroline = F, 
      showgrid = F
    )
  ) %>% 
  animation_opts(
    frame = 40, 
    transition = 7, 
    redraw = FALSE
  ) %>%
  animation_slider(
    currentvalue = list(
      prefix = "Day "
    )
  )

triple_underlying_plot

sentiment_plot


