library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(maps)
library(tibble)
library(leaflet)
source("global.r")
shinyUI(fluidPage( 
  div(style = "padding: 10px; background-color: #f7f7f7; display: flex; align-items: center;",
      img(src = "https://static01.nyt.com/images/2012/07/15/magazine/15wmt/15wmt-jumbo.jpg", height = "50", style = "margin-right: 20px;"), 
      h1("Olympic", style = "color: #333; margin: 0;")  
  ),
  tabsetPanel(
    tabPanel("地圖1",
     sidebarLayout(
       sidebarPanel(
         sliderInput("Map1_year", "Choose a Year:",  min = min(Year_sel),max = max(Year_sel), value = min(Year_sel),step = 1, animate= animationOptions(interval=2500, loop=TRUE)) # 0616更新
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
  )
))

