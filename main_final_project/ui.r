library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(maps)
library(tibble)
library(leaflet)
library(shinyWidgets)
source("global.r")
shinyUI(
  fluidPage(
    tags$head(tags$style("
              .jhr{
              display: inline;
              vertical-align: middle;
              padding-left: 10px;
    }")),
    list(tags$head(HTML('<link rel="icon", href="MyIcon.png", 
                                  type="image/png" />'))),
    div(style="padding: 1px 0px; width: '100%'",
        titlePanel(
                title="", windowTitle="My Window Title"
        )
    ),
    navbarPage(
      title=div(img(src="https://static01.nyt.com/images/2012/07/15/magazine/15wmt/15wmt-jumbo.jpg", height = "25", style = "margin-right: 5px;"), "Olympic"),

      
      tabPanel("地圖1",
          sidebarLayout(
            sidebarPanel(
              sliderInput("year", "Choose a Year:", 1896, 2016, 1896,step = 4, animate= animationOptions(interval=2000, loop=TRUE))
            ),    
            mainPanel(
              plotOutput("Year_Map_Plot")
            )
        )
      ),
      tabPanel("地圖2",
          mainPanel(      
          leafletOutput("map", width = "200%", height = "800px")  # 你可以调整高度
        )
      ),

      ### 運動強國 ###

      tabPanel("運動強國",
          fluidRow(
            column(3,

              pickerInput(
                inputId = "Sports", 
                label = "運動", 
                choices =  ALLSPORT,
                choicesOpt = list(
                  content = 
                    SPORT_HTML
                  )
                ,
                selected = "All"
              )

              # selectInput("Sports", "運動", c("All Sports"="",data$Sport), multiple=TRUE,selected="Summer")
            ),
            column(3,
              #顯示名次 使用拉桿
              sliderInput("Rank", "顯示名次", min=1, max=50, value=1)
            ),

            
          ),
          uiOutput("dynamic_html")
      ),
    ),
  )
)
# shinyUI(fluidPage( 
#   div(style = "padding: 10px; background-color: #f7f7f7; display: flex; align-items: center;",
#       img(src = "https://static01.nyt.com/images/2012/07/15/magazine/15wmt/15wmt-jumbo.jpg", height = "50", style = "margin-right: 20px;"), 
#       h1("Olympic", style = "color: #333; margin: 0;")  
#   ),
#   tabsetPanel(
#     tabPanel("地圖1",
#      sidebarLayout(
#        sidebarPanel(
#          sliderInput("year", "Choose a Year:", 1896, 2016, 1896,step = 4, animate= animationOptions(interval=2000, loop=TRUE))
#        ),    
#        mainPanel(
#          plotOutput("Year_Map_Plot")
#        )
#     )
#    ),
#    tabPanel("地圖2",
#       mainPanel(      
#           leafletOutput("map", width = "200%", height = "800px")  # 你可以调整高度
#         )
#    ),
#   )
# ))

