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
library(LDAvis)

source('functions/wordclouds_functions.R')

# load("objects/sentiment_o.RData")
# load("objects/sentiment_timestamp_old.RData")
# load("objects/sentiment_histogram_old.RData")
# 
# load("objects/sentiment_n.RData")
# load("objects/sentiment_timestamp_new.RData")
# load("objects/sentiment_histogram_new.RData")



ui <- fluidPage(tags$style(HTML(".p {
                                    color: white;}
                                    .navbar-nav > li > a, .navbar-brand {
                   padding-top:10px !important; 
                   padding-bottom:10px !important;
                   padding-left:10px !important;
                   padding-right:5px !important;
                   height: 30px;
                 }
                 .navbar {min-height:25px !important;
                                    }")
                ),
                
                includeCSS(path = "AdminLTE.css"),
                includeCSS(path = "shinydashboard.css"),
                
                
  navbarPage(title ="Iran and USA conflict development analysis",
                 theme = shinytheme("spacelab"),
                 fluid = TRUE,
                 tabPanel("Presentation",
                          fluidRow(
                            tags$h1("USA vs Iran conflict - how do the redditors look at it?",
                                    style = "text-align: center"),
                            width = 12,
                            br()
                            ),
                          fluidRow(
                            box(width = 12, 
                                column(4,
                                       align="center",
                                       imageOutput("image_trump"),
                                       style = "transform: rotate(-8deg); padding: 50px 0px 0px 0px"),
                                column(6,
                                       align="right",
                                       imageOutput("image_war"),
                                       style = "transform: rotate(8deg); padding: 50px 0px 0px 50px"),
                                column(2)
                            )
                            ),
                          fluidRow(
                            tags$h5("Made by:"),
                            width = 12
                            ),
                          fluidRow(
                            tags$h4("Bartłomiej Kowalczuk & Michał Thor"),
                            width = 12
                            )
                          ),
                 tabPanel("6 months of conflict", 
                          fluidRow(
                                 valueBox(value = tags$h3(104155, style = "color: white"),
                                          subtitle = tags$p("Number of posts", style = "color: white"),
                                          icon = icon("user-edit"),
                                          color = "purple"),
                                 
                                 valueBox(value = tags$h3("6 months", style = "color: white"),
                                          "Analysis period",
                                          icon = icon("calendar-alt"),
                                          color = "light-blue"),
                                 
                                 valueBox(value = tags$h3(53638, style = "color: white"),
                                          "Distinct strings",
                                          icon = icon("list-ol"),
                                          color = "orange"),
                                 style = "padding: 0px"
                                 ),
                          fluidRow(
                                box(
                                  title = tags$h3("Data from last 6 months - over 100k posts",
                                                  style = "padding: 0px"),
                                  width = 12,
                                   br(), br(),
                                  wordcloud2Output('wordcloud2', height = 400),
                                  style = "padding: 0px"
                                  )
                                )
                          ),
                 tabPanel("Reddit users' attitude 6 months ago",
                            fluidRow(
                              width = 12,
                                box(
                                  width = 6,
                                  title = "Histogram of sentiment",
                                  solidHeader = TRUE,
                                  "Data from last 6 months - over 100k posts", br(), br(),
                                  plotlyOutput("histogram_sentiment_old")
                                  ),
                                box(
                                  width = 6,
                                  title = tags$h3("Data from last 6 months - over 100k posts"),
                                  solidHeader = TRUE,
                                  br(), br(),
                                  wordcloud2Output('wordcloud_old')
                                )
                                )
                    
                          ),
                 tabPanel("Perception 6 months ago",
                          fluidRow(width=12,
                               box(
                                 title = tags$h3("Reaction to USA approving attack on Iran"),
                                 solidHeader = TRUE,
                                 width = 6,
                                 height = "100%",
                                 plotlyOutput("old_usa_radar", height = 650)
                                 ),
                               box(
                                 title = tags$h3("Reaction to Iranian missile shooting down the American drone"),
                                 solidHeader = TRUE,
                                 width = 6,
                                 height = "100%",
                                 plotlyOutput("old_iran_radar", height = 650)
                                 )
                               )
                          ),
                 tabPanel("Congress is decisive",
                         fluidRow(width=12,
                              box(
                                title = tags$h3("Main topics after decision that Trump needs Congress aproval to attack"),
                                visOutput('LDAVis_congress')
                                )
                              )
                          ),
                 tabPanel("Sentiment in time",
                          fluidRow(
                              width = 12,
                              box(
                                title = "Sentiment evolution over time",
                                solidHeader = TRUE,
                                width = 12,
                                height = "100%",
                                plotlyOutput("sentiment_selfdrawing_plot", height = 300)
                                )
                              ),
                          fluidRow(
                            box(
                               title = "Triple underlying plot",
                               solidHeader = TRUE,
                               width = 12,
                               height = "100%",
                               plotlyOutput("triple_underlying_plot", height = 500)
                               )
                            )
                          ),
                 tabPanel("Correlation with sentiment",
                          fluidRow(
                              width = 12,
                              box(
                                title = "Correlation between prices and sentiment",
                                solidHeader = TRUE,
                                width = 6,
                                height = "100%",
                                plotlyOutput("corr_plot", height = 600)
                                ),
                              box(
                                title = "Triple underlying sentiment plot",
                                solidHeader = TRUE,
                                width = 6,
                                height = "100%",
                                plotlyOutput("triple_underlying_sentiment_plot", height = 600)
                                )
                              ),
                          fluidRow(
                            width = 12,
                            height = "100%",
                            tags$h4("GE - ; HAL - ", style = "text-align: center")
                            )
                          ),
                   tabPanel("Reddit users' attitude now",
                            fluidRow(
                              width = 12,
                              box(
                                width = 6,
                                title = "Histogram of sentiment",
                                solidHeader = TRUE,
                                "Data from last 6 months - over 100k posts", br(), br(),
                                plotlyOutput("histogram_sentiment_new")
                              ),
                              box(
                                width = 6,
                                title = tags$h2("Data from last 6 months - over 100k posts"),
                                solidHeader = TRUE,
                                br(), br(),
                                imageOutput("image2")
                              )
                            )
                            ),
                   tabPanel("Perception now",
                            fluidRow(width=12,
                                box(
                                  title = tags$h3("Reaction to USA killing of Iranian General"),
                                  solidHeader = TRUE,
                                  width = 6,
                                  height = "100%",
                                  plotlyOutput("new_usa_radar", height = 650)
                                ),
                                box(
                                  title = tags$h3("Reaction to Iranian missile destryoing USA military base"),
                                  solidHeader = TRUE,
                                  width = 6,
                                  height = "100%",
                                  plotlyOutput("new_iran_radar", height = 650)
                                  )
                                )
                          )
                 # tabPanel("Development of the situation",
                 #          fluidRow(
                 #              width = 12,
                 #              box(
                 #                title = "Prices of underlyings throughout half year",
                 #                solidHeader = TRUE,
                 #                width = 12,
                 #                height = "100%",
                 #                plotlyOutput("triple_selfdrawing_plot", height = 650)
                 #                )
                 #              ),
                 #            fluidRow(
                 #              box(
                 #                width = 12,
                 #                height = "100%",
                 #                tags$h4("XAR - ; ADM - ; XOM - ")
                 #                )
                 #              )
                 #          ),
                 # tabPanel("Bu",
                 #          fluidRow(
                 #            
                 #            box(
                 #              title = "Triple underlying plot",
                 #              background = "light-blue",
                 #              solidHeader = TRUE,
                 #              width = 12,
                 #              height = "100%",
                 #              plotlyOutput("triple_underlying_plot", height = 650)
                 #              
                 #            )
                 #            
                 #          )),
                 # tabPanel("Sentiment vs. prices",
                 #          fluidRow(
                 #            
                 #            box(
                 #              title = "Triple underlying sentiment plot",
                 #              background = "light-blue",
                 #              solidHeader = TRUE,
                 #              width = 12,
                 #              height = "100%",
                 #              plotlyOutput("triple_underlying_sentiment_plot", height = 650)
                 #              
                 #            )
                 #            
                 #          )),
                 # 
                 # 
                 # 
                 # tabPanel("Old LDAVis",
                 #          fluidRow(width=12,
                 #                   box(
                 #                     visOutput('LDAVis_old')
                 #                     
                 #                   )
                 #         )
                 # ),
                 # 
                 # tabPanel("Congress LDAVis",
                 #          fluidRow(width=12,
                 #                   box(
                 #                     visOutput('LDAVis_congress')
                 #                     
                 #                   )
                 #          )
                 )
             
             
             
             # tabPanel("Wordcloud",
             #          fluidRow(
             #            box(
             #              title = "Wordcloud",
             #              background = "light-blue",
             #              solidHeader = TRUE,
             #              width = 12,
             #              "Data from last 6 months - over 100k posts", br(), br(),
             #              wordcloud2Output('wordcloud2')
             #            )
             #            
             #            
             #          )),
                 # )
)




server <- function(input, output) {


  
  output$wordcloud2 <- renderWordcloud2({
    wordcloud <- plot_wordcloud(file_id = 'full_data',
                                use_image = FALSE)
    wordcloud
  })
  
  output$histogram_sentiment_old <- renderPlotly({

    sentiment_histogram_old
    
  })
  
  output$wordcloud_old <- renderWordcloud2({
    wordcloud_old <- plot_wordcloud(file_id = 'ccednx',
                                use_image = FALSE)
    wordcloud_old
  })
  
  output$histogram_sentiment_new <- renderPlotly({

    sentiment_histogram_new

  })
  
  output$wordcloud_new <- renderWordcloud2({
    wordcloud_new <- plot_wordcloud(file_id = 'ej7ykn',
                                    use_image = FALSE)
    wordcloud_new
  })
  
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
  output$new_iran_radar <- renderPlotly({
    
    load("objects/new_iran_radar.RData")
    new_iran_radar
    
  }) 
  
  
  # output$LDAVis_old <- renderVis({
  #   load("objects/LDAVis_old.RData")
  #   createJSON(phi = results$phi, 
  #              theta = results$theta, 
  #              doc.length = results$doc.length, 
  #              vocab = results$vocab, 
  #              term.frequency = results$term.frequency)
  #   
  #   
  # }) 
  
  output$LDAVis_congress <- renderVis({
    load("objects/LDAVis_congress.RData")
    createJSON(phi = results2$phi, 
               theta = results2$theta, 
               doc.length = results2$doc.length, 
               vocab = results2$vocab, 
               term.frequency = results2$term.frequency)
    
    
  })
  
  
  output$image2 <- renderImage({
    return(list(
        src = "scrn_new_wc.JPG",
        height="400px", 
        width="600px")
      )
    
  }, deleteFile = FALSE)
  
  output$image_trump <- renderImage({
    return(list(
      src = "trump_meme.jpg",
      height="90%")
    )
    
  }, deleteFile = FALSE)
  
  output$image_war <- renderImage({
    return(list(
      src = "trump_war.png",
      height="70%")
    )
    
  }, deleteFile = FALSE)
  
  
}


shinyApp(ui, server)