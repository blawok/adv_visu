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
    fluidRow(
      # A static valueBox
      valueBox(104155,
               "Number of posts",
               icon = icon("credit-card"),
               color = "purple"),
      
      # Dynamic valueBoxes
      valueBox("6 months",
               "Analysis period",
               icon = icon("credit-card")),
      
      valueBox(53638,
               "Distinct strings",
               icon = icon("credit-card"),
               color = "orange")
    ),

    br(),
    br(),
    
    fluidRow(
      box(
        title = "Wordcloud",
        background = "light-blue",
        solidHeader = TRUE,
        width = 12,
        "Data from last 6 months - over 100k posts", br(), br(),
        wordcloud2Output('wordcloud2')
      ),
    
      
    ),
    
    
    fluidRow(
      box(
        title = "Histogram of sentiment",
        background = "light-blue",
        solidHeader = TRUE,
        "Data from last 6 months - over 100k posts", br(), br(),
        plotlyOutput("plot")
      )
    )
    
  )
  )


server <- function(input, output) {
  
  output$wordcloud2 <- renderWordcloud2({
    wordcloud <- plot_wordcloud(file_id = 'full_data',
                                use_image = FALSE)
    wordcloud
  })

  output$plot <- renderPlotly({
    sentiment <- read_csv('data/df_with_sentiment_timestamp.csv')
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