######################################

# Advanced Visualisation Project

# Bartlomiej Kowalczuk 372926
# Michal Thor 361309

######################################


install.packages("xts")
install.packages("quantmod")

library(xts)
library(quantmod)
library(dplyr)
library(plotly)
source("./functions/functions_thor.R")

Sys.setlocale("LC_TIME", "English")

# 
# company_names <- c("GE", "XOM", "ADM", "HAL", "LMT")
# currencies <- c("USD/IRR", "EUR/USD", "GBP/USD")
# commodities <- c("CL", "ZG")
# world_indices <- c("^GSPC", "^DJI", "^FTSE", "XAR")
# 
# actual_date <- Sys.Date()
# 
# 
# 
# getCompanies <- function (company, 
#                            date_from = "2019-01-01", 
#                            date_to = actual_date) {
#   getSymbols(company,
#                          from = date_from,
#                          to = date_to,
#                          auto.assign = FALSE)
# }
# 
# getCurrencies <- function (currency, 
#                           date_from = "2019-01-01", 
#                           date_to = actual_date) {
#   getFX(currency,
#              from = date_from,
#              to = date_to,
#              auto.assign = FALSE)
# }
# 
# getCommodities <- function (commodity, 
#                             date_from = "2019-01-01", 
#                             date_to = actual_date) {
#   getSymbols(commodity,
#         from = date_from,
#         to = date_to,
#         base.currency = "EUR", # For stability despite possible USD fluctuations
#         auto.assign = FALSE)
# }
# 
# getIndices <- function (w_index, 
#                             date_from = "2019-01-01", 
#                             date_to = actual_date) {
#   getSymbols(w_index,
#             from = date_from,
#             to = date_to,
#             auto.assign = FALSE)
# }
# 
# 
# for (company in company_names) {
#   assign(paste("quot_", company, sep = ""), Op(getCompanies(company)))
# }
# 
# for (curr in currencies) {
#   curr2 <- gsub("[[:punct:]]", "", curr)
#   assign(paste("curr_", curr2, sep = ""), getCurrencies(curr))
# }
# 
# for (comm in commodities) {
#   assign(paste("commodity_", comm, sep = ""), Op(getCommodities(comm)))
# }
# 
# for (w_ind in world_indices) {
#   w_ind2 <- gsub("[[:punct:]]", "", w_ind)
#   assign(paste("w_index_", w_ind2, sep = ""), Op(getIndices(w_ind)))
# }
# 


# data_x <- merge.xts(
#   commodity_CL,
#   commodity_ZG,
#   curr_EURUSD,
#   curr_GBPUSD,
#   curr_USDIRR,
#   quot_ADM,
#   quot_GE,
#   quot_HAL,
#   quot_LMT,
#   quot_XOM,
#   w_index_DJI,
#   w_index_FTSE,
#   w_index_GSPC,
#   w_index_XAR
# ) %>% na.omit()

z <- read.zoo("merged_indices.csv", sep=",", header = TRUE, tz = "")
data_x <- as.xts(z)

df_x <- data.frame(data_x)
df_x <- cbind(date_time = rownames(df_x), df_x)
df_x$ID <- seq.int(nrow(df_x))



df_x_acc <- df_x %>%
  accumulateBy(~ID)


# write.zoo(data_x,file="merged_indices.csv",index.name="date",row.names=FALSE,col.names=TRUE,sep=",")

r <- read.zoo("reddit_xts.csv", sep=",", header = TRUE, tz = "")
data_r <- as.xts(r)
df_r <- data.frame(data_r)
df_r <- cbind(date_time = rownames(df_r), df_r)
df_r$ID <- seq.int(nrow(df_r))

df_r_acc <- df_r %>%
  accumulateBy(~ID)

p1 <- df_x_acc %>%
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
    title = "Prices of (...) since June 2019",
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
    frame = 15, 
    transition = 5, 
    redraw = FALSE
  ) %>%
  animation_slider(
    currentvalue = list(
      prefix = "Day "
    )
  )

p2 <- df_r_acc %>%
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
    title = "Sentiment since May 2019",
    yaxis = list(
      title = "Sentiment value", 
      range = c(-0.7,0.7), 
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
    frame = 30, 
    transition = 10, 
    redraw = FALSE
  ) %>%
  animation_slider(
    currentvalue = list(
      prefix = "Day "
    )
  )

p1

p2


# Subplots for in time analysis
# 
# p <- economics %>%
#   tidyr::gather(variable, value, -date) %>%
#   transform(id = as.integer(factor(variable))) %>%
#   plot_ly(x = ~date, y = ~value, color = ~variable, colors = "Dark2",
#           yaxis = ~paste0("y", id)) %>%
#   add_lines() %>%
#   subplot(nrows = 5, shareX = TRUE)
# 
# p

