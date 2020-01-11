library(shiny)
library(plotly)
library(wordcloud2)
library(shinydashboard)
source('functions/wordclouds_functions.R')

ui <- dashboardPage(
  dashboardHeader(title = "Iran vs Trump"),
  dashboardSidebar(
    br(),
    strong("Developed by"),
    sidebarMenu(
      menuItem("Michal Thor", icon = icon("dashboard")),
      menuItem("Bartlomiej Kowalczuk", icon = icon("th"))
    )
  ),
  dashboardBody(
    strong("Analysis"),
    br(),
    br(),
    br(),
    br(),
    strong("Wordcloud of an article"),
    wordcloud2Output('wordcloud2'),
    br(),
    br(),
    br(),
    br(),
    strong("Histogram of sentiment"),
    plotlyOutput("plot")
    ),
  )


server <- function(input, output) {
  
  output$wordcloud2 <- renderWordcloud2({
    wordcloud_article <- plot_wordcloud(df_path = 'data/clean_reddit_df.csv',
                                        thread_id = 'ej95ak',
                                        use_filtred_data = TRUE)
    wordcloud_article
  })

  output$plot <- renderPlotly({
    sentiment <- filter(sentiment, sentiment$thread_id == 'ej95ak')  
    
    a <- list(
      title = "Sentiment",
      range = c(-1, 1)
    )
    plot_ly(x=~sentiment$ave_sentiment,
            type = "histogram",
            histnorm = "probability",
            alpha = 0.8) %>% layout(xaxis = a)
  })
  
}

shinyApp(ui, server)