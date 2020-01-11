library(plotly)
library(xts)
library(dplyr)
library(tidyr)

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

xtsToDF <- function(xts_file) {
  
  df <- data.frame(xts_file)
  df <- cbind(date_time = rownames(df), df)
  df$ID <- seq.int(nrow(df))
  
  names(df) <- gsub(".Open", "", names(df), fixed = TRUE)
  
  return (df)
}

df1 <- xtsToDF(xts1)

plotThreeLines <- function(df_file, index_1, index_2, index_3) {
  
  print(df_file[index_1])
  
  p <- df_file %>%
    select(., index_1, index_2, index_3) %>% 
    tidyr::gather(variable, value, -date_time) %>% 
    plot_ly(.,
      x = ~date_time, 
      y = ~value,
      color = ~variable,
      colors = "Dark2",
      yaxis = ~paste0("y", id),
      type = 'scatter', 
      mode = 'lines',
      name = ~paste(index_1, "Index value"),
      line = list(color = 'rgb(114, 186, 59)'),
      text = ~paste(date_time, "<br>Price: $", index_1), 
      hoverinfo = 'text'
    ) %>%
    layout(
      title = "Prices of 2 chosen indices since June 2019",
      yaxis = list(
        title = "Price",
        range = c(0,60),
        zeroline = F,
        tickprefix = "$"
      ),
      xaxis = list(
        title = "Day",
        range = c(0,130),
        zeroline = F,
        showgrid = F
      ) )
 
  return (p)
}
    
plotThreeLines(df1, "GE", "XAR")

  # Subplots for in time analysis
  # 
  p <- economics %>%
    tidyr::gather(variable, value, -date) %>%
    transform(id = as.integer(factor(variable))) %>%
    plot_ly(x = ~date, y = ~value, color = ~variable, colors = "Dark2",
            yaxis = ~paste0("y", id)) %>%
    add_lines() %>%
    subplot(nrows = 5, shareX = TRUE)

  p



  
  # plot_ly(.,
  #         x = ~ID, 
  #         y = .[index_1],
  #         type = 'scatter', 
  #         mode = 'lines',
  #         name = ~paste(index_1, "Index value"),
  #         line = list(color = 'rgb(114, 186, 59)'),
  #         text = ~paste(date_time, "<br>Price: $", index_1), 
  #         hoverinfo = 'text'
  # ) %>%
  #   layout(
  #     title = "Prices of 2 chosen indices since June 2019",
  #     yaxis = list(
  #       title = "Price",
  #       range = c(0,60),
  #       zeroline = F,
  #       tickprefix = "$"
  #     ),
  #     xaxis = list(
  #       title = "Day",
  #       range = c(0,130),
  #       zeroline = F,
  #       showgrid = F
  #     ) )

