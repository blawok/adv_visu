library(shiny)
library(ggplot2)
library(plotly)
library(wordcloud2)
library(shinydashboard)
library(shinythemes)
library(xts)
library(tidyr)
library(dplyr)
library(reshape2)
library(syuzhet)
source('functions/wordclouds_functions.R')
load("objects/sentiment_timestamp.RData")

ui <- navbarPage(title ="Iran vs Trump",
                 theme = shinytheme("cerulean"),
                 fluid = TRUE,
                 tabPanel("Map1", fluidRow(
                   valueBox(104155,
                            "Number of posts",
                            icon = icon("user-edit"),
                            color = "purple"),
                   
                   valueBox("6 months",
                            "Analysis period",
                            icon = icon("calendar-alt"),
                            color = "light-blue"),
                   
                   valueBox(53638,
                            "Distinct strings",
                            icon = icon("list-ol"),
                            color = "orange")
                 )),
                 
                
                 tabPanel("Wordcloud",
                          fluidRow(
                            box(
                              title = "Wordcloud",
                              background = "light-blue",
                              solidHeader = TRUE,
                              width = 12,
                              "Data from last 6 months - over 100k posts", br(), br(),
                              wordcloud2Output('wordcloud2')
                            )
                            
                            
                          )),
                 
                 
                 tabPanel("Underlying price changes",
                          fluidRow(
                            
                            box(
                              title = "Triple selfdrawing plot",
                              background = "light-blue",
                              solidHeader = TRUE,
                              width = 12,
                              height = "100%",
                              plotlyOutput("triple_selfdrawing_plot", height = 650)
                              
                            )
                            
                          )),
                 tabPanel("Histograms",
                          fluidRow(
                            box(
                              title = "Histogram of sentiment",
                              background = "light-blue",
                              solidHeader = TRUE,
                              "Data from last 6 months - over 100k posts", br(), br(),
                              plotlyOutput("histogram_sentiment")
                            )
                            
                            
                          )),
                 tabPanel("Correlation",
                          fluidRow(
                            
                            box(
                              title = "Correlation between prices and sentiment",
                              background = "light-blue",
                              solidHeader = TRUE,
                              width = 12,
                              height = "100%",
                              plotlyOutput("corr_plot", height = 700)
                              
                            )
                            
                          )),
                 tabPanel("Bu",
                          fluidRow(
                            
                            box(
                              title = "Triple underlying plot",
                              background = "light-blue",
                              solidHeader = TRUE,
                              width = 12,
                              height = "100%",
                              plotlyOutput("triple_underlying_plot", height = 650)
                              
                            )
                            
                          )),
                 tabPanel("Sentiment vs. prices",
                          fluidRow(
                            
                            box(
                              title = "Triple underlying sentiment plot",
                              background = "light-blue",
                              solidHeader = TRUE,
                              width = 12,
                              height = "100%",
                              plotlyOutput("triple_underlying_sentiment_plot", height = 650)
                              
                            )
                            
                          )),
                 tabPanel("Sentiment in time",
                          fluidRow(
                            
                            box(
                              title = "Sentiment selfdrawing plot",
                              background = "light-blue",
                              solidHeader = TRUE,
                              width = 12,
                              height = "100%",
                              plotlyOutput("sentiment_selfdrawing_plot", height = 650)
                              
                            )
                            
                          )),
                 
                 tabPanel("Moods 6 months ago",
                          fluidRow(width=12,
                            
                            box(
                              title = "Moods on reddit 6 months ago",
                              solidHeader = TRUE,
                              width = 6,
                              height = "100%",
                              plotlyOutput("old_usa_radar", height = 650)
                              
                              
                            ),
                            box(
                              title = "Moods on reddit 6 months ago",
                              solidHeader = TRUE,
                              width = 6,
                              height = "100%",
                              plotlyOutput("old_iran_radar", height = 650)
                              
                              
                            )
                            
                            
                          )),
                 tabPanel("Old LDAVis",
                          fluidRow(width=12,
                                   box(
                                     visOutput('LDAVis_old')
                                     
                                   )
                         )
                 ),
                 
                 tabPanel("Congress LDAVis",
                          fluidRow(width=12,
                                   box(
                                     visOutput('LDAVis_congress')
                                     
                                   )
                          )
                 ),
                 
                 includeCSS(path = "AdminLTE.css"),
                 includeCSS(path = "shinydashboard.css")
                 
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
  
  # output$histogram_sentiment_article <- renderPlotly({
  #   
  #   load("objects/sentiment_timestamp.RData")
  #   load("objects/sentiment_histogram.RData")
  #   sentiment_histogram
  #   
  # })
  
  output$corr_plot <- renderPlotly({
    
    load( "objects/corr_plot.RData")
    correlation_plot
    
  })
  
  output$triple_underlying_plot <- renderPlotly({
    
    load("objects/triple_underlying_plot.RData")
    triple_underlying_plot
    
  })
  
  output$triple_underlying_sentiment_plot <- renderPlotly({
    
    load("objects/triple_underlying_sent_plot.RData")
    triple_underlying_sent_plot
    
  })
  
  output$triple_selfdrawing_plot <- renderPlotly({
    
    load("objects/triple_selfdrawing_plot.RData")
    triple_selfdrawing_plot
    
  })
  
  output$sentiment_selfdrawing_plot <- renderPlotly({
    
    load("objects/sentiment_selfdrawing_plot.RData")
    sentiment_selfdrawing_plot
    
  })  
  
  
  output$old_usa_radar <- renderPlotly({
    
    load("objects/old_usa_radar.RData")
    old_usa_radar
    
  }) 
  output$old_iran_radar <- renderPlotly({
    
    load("objects/old_iran_radar.RData")
    old_iran_radar
    
  }) 
  output$new_usa_radar <- renderPlotly({
    
    load("objects/new_usa_radar.RData")
    new_usa_radar
    
  }) 
  output$snew_iran_radar <- renderPlotly({
    
    load("objects/new_iran_radar.RData")
    snew_iran_radar
    
  }) 
  
  
  output$LDAVis_old <- renderVis({
    load("objects/LDAVis_old.RData")
    createJSON(phi = results$phi, 
               theta = results$theta, 
               doc.length = results$doc.length, 
               vocab = results$vocab, 
               term.frequency = results$term.frequency)
    
    
  }) 
  
  output$LDAVis_congress <- renderVis({
    load("objects/LDAVis_congress.RData")
    createJSON(phi = results$phi, 
               theta = results$theta, 
               doc.length = results$doc.length, 
               vocab = results$vocab, 
               term.frequency = results$term.frequency)
    
    
  }) 
  
  
}


shinyApp(ui, server)