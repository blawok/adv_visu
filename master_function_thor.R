
source("./functions/functions_thor.R")

##############################################################
# I part - Multiple plots with Sentiment
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
# II part - Correlation plotting
##############################################################

df_co <- df_corr[, -which (names(df_corr) %in% c("date_time", "ID"))] %>% 
  na.omit()

cormat <- round(cor(df_co),2)


cormat <- reorderCormat(cormat)
lower_tri <- getLowerTri(cormat)

melted_cormat <- melt(lower_tri, na.rm = TRUE)

gg_corr <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed() +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(1.0, 0.5),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

correlation_plot <- ggplotly(gg_corr)
correlation_plot

# save(correlation_plot, file = "./objects/corr_plot.RData")
 
# save(melted_cormat, file = "./objects/corr_plot_data.RData")

##############################################################
# III part - Self-drawing plots with plotly
##############################################################

z <- read.zoo("./data/merged_indices.csv", sep=",", header = TRUE, tz = "")
data_x <- as.xts(z)

df_x <- data.frame(data_x)
df_x <- cbind(date_time = rownames(df_x), df_x)
df_x$ID <- seq.int(nrow(df_x))

names(df_x) <- gsub(".Open", "", names(df_x), fixed = TRUE)


df_x_acc <- df_x %>%
  accumulateBy(~ID)


# write.zoo(data_x,file="merged_indices.csv",index.name="date",row.names=FALSE,col.names=TRUE,sep=",")

r <- read.zoo("./data/reddit_xts.csv", sep=",", header = TRUE, tz = "")

data_r <- as.xts(r)
df_r <- data.frame(data_r)
df_r <- cbind(date_time = rownames(df_r), df_r)
df_r$ID <- seq.int(nrow(df_r))

names(df_r) <- c("date_time", "sentiment", "ID")

df_r_acc <- df_r %>%
  accumulateBy(~ID)

triple_selfdrawing_plot <- df_x_acc %>%
  plot_ly(
    x = ~ID, 
    y = ~XAR, 
    frame = ~frame,
    type = 'scatter', 
    mode = 'lines',
    name = "XAR",
    line = list(color = 'rgb(0, 16, 33)'),
    text = ~paste(date_time, "<br>Value: $", XAR), 
    hoverinfo = 'text'
  )  %>% 
  add_trace(
    y = ~ADM, 
    name = 'ADM',
    line = list(color = 'rgb(17, 181, 228)'),
    mode = 'lines',
    text = ~paste(date_time, "<br>Value: $", ADM) 
  ) %>%
  add_trace(
    y = ~XOM, 
    name = 'XOM', 
    mode = 'lines',
    line = list(color = 'rgb(20, 129, 186)'),
    text = ~paste(date_time, "<br>Value: $", XOM) 
  ) %>%
  layout(
    # title = "Values of chosen underlyings since July 2019",
    yaxis = list(
      title = "Underlying", 
      range = c(35,115), 
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

sentiment_selfdrawing_plot <- df_r_acc %>%
  plot_ly(
    x = ~ID, 
    y = ~sentiment, 
    frame = ~frame,
    type = 'scatter', 
    mode = 'lines',
    name = "Sentiment",
    line = list(color = 'rgb(12, 170, 220)'),
    text = ~paste(date_time, "<br>Value:", sentiment), 
    hoverinfo = 'text'
  ) %>%
  layout(
    # title = "Sentiment value since May 2019",
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

triple_selfdrawing_plot

sentiment_selfdrawing_plot



##############################################################
# IV part - Radar plots
##############################################################

old_usa <- getSentiment("c35akk")

old_usa_radar <- createRadar(old_usa, "USA action reception 6 months ago")

old_iran <- getSentiment("c555x4")

old_iran_radar <- createRadar(old_iran, "Iran action reception 6 months ago")

new_usa <- getSentiment("ej95ak")

new_usa_radar <- createRadar(new_usa, "USA action reception today")

new_iran <- getSentiment("elku7e")

new_iran_radar <- createRadar(new_iran, "Iran action reception today")

