library(shiny)
library(ggplot2)
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
    
    h3("Hello its bartek"),
    
    fluidRow(
      box(
        title = "Wordcloud",
        background = "light-blue",
        solidHeader = TRUE,
        width = 12,
        "Data from last 6 months - over 100k posts", br(), br(),
        wordcloud2Output('wordcloud2')
      )
    
      
    ),
    
    
    fluidRow(
      box(
        title = "Histogram of sentiment",
        background = "light-blue",
        solidHeader = TRUE,
        "Data from last 6 months - over 100k posts", br(), br(),
        plotlyOutput("histogram_sentiment")
      ),
      box(
        title = "Histogram of sentiment of given article",
        background = "light-blue",
        solidHeader = TRUE,
        "Data from last 6 months - over 100k posts", br(), br(),
        plotlyOutput("histogram_sentiment_article")
      ),
      
      
    ),
    
    
    fluidRow(
      
      box(
        title = "Correlation between prices and sentiment",
        background = "light-blue",
        solidHeader = TRUE,
        width = 12,
        height = "100%",
        plotlyOutput("corr_plot", height = 700)
        
      )
      
    ),
    
    
    fluidRow(
      
      box(
        title = "Triple underlying plot",
        background = "light-blue",
        solidHeader = TRUE,
        width = 12,
        height = "100%",
        plotlyOutput("triple_underlying_plot", height = 650)
        
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

  output$histogram_sentiment <- renderPlotly({
    
    load("objects/sentiment_timestamp.RData")
    load("objects/sentiment_histogram.RData")
    sentiment_histogram
    
  })
  
  output$histogram_sentiment_article <- renderPlotly({
    
    load("objects/sentiment_timestamp.RData")
    load("objects/sentiment_histogram.RData")
    sentiment_histogram
    
  })
  
  output$corr_plot <- renderPlotly({
    
    load( "objects/corr_plot.RData")
    correlation_plot
    
  })
  
  output$triple_underlying_plot <- renderPlotly({
    
    load("objects/triple_underlying_plot.RData")
    triple_underlying_plot
    
  })
  
}

shinyApp(ui, server)