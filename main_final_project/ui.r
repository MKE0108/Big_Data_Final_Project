library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(maps)
library(tibble)
library(leaflet)
library(shinyWidgets)
library(shinythemes)
source("global.r")
shinyUI(
  fluidPage(
      tags$head(
    tags$style(HTML("
      #floatWindow {
        position: fixed;
        top: 100px;
        left: 50px;
        z-index: 999;
        width: 300px;
        height: auto;
        background-color: #fff;
        border: 1px solid #ddd;
        padding: 10px;
        box-shadow: 0px 0px 10px rgba(0,0,0,0.5);
      }
      .handle {
        cursor: move;
        background-color: #eee;
        padding: 5px 10px;
        text-align: center;
      }
    ")),
  ),

    tags$head(
        tags$style(HTML("
            #floating-sidebar {
                position: fixed;
                top: 70px;
                right: 20px;
                width: 250px;
                z-index: 9999;
                background-color: #ffa73b;
                padding: 10px;
                box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2);
            }
            .content-table-full-page {
              border-collapse: collapse;
              margin: 25px 0;
              font-size: 0.9em;
              min-width: 500px;
              border-radius: 5px 5px 0 0;
              overflow: hidden;
              box-shadow: 0 0 20px rgba(0, 0, 0, 0.15);
          }

          .content-table-full-page thead tr {
              background-color: #009879;
              color: #ffffff;
              text-align: left;
              font-weight: bold;
          }

          .content-table-full-page th {
            padding: 13px 14px;
            font-size: 25px;
          }

          .content-table-full-page td {
            padding: 6px 7px;
            font-size: 20px;
          }

          .content-table-full-page tbody tr {
              border-bottom: 1px solid #dddddd;
          }

          .content-table-full-page tbody tr:nth-of-type(even) {
              background-color: #f3f3f3;
          }

          .content-table-full-page tbody tr:last-of-type {
              border-bottom: 2px solid #009879;
          }
                    .content-table {
              border-collapse: collapse;
              margin: 5px 0;
              font-size: 0.9em;
              min-width: 60px;
              border-radius: 5px 5px 0 0;
              overflow: hidden;
              box-shadow: 0 0 20px rgba(0, 0, 0, 0.15);
          }

          .content-table thead tr {
              background-color: #009879;
              color: #ffffff;
              text-align: left;
              font-weight: bold;
          }

          .content-table th {
              padding: 6px 7px;
          }

          .content-table td {
              padding: 4px 6px;
          }

          .content-table tbody tr {
              border-bottom: 1px solid #dddddd;
          }

          .content-table tbody tr:nth-of-type(even) {
              background-color: #f3f3f3;
          }

          .content-table tbody tr:last-of-type {
              border-bottom: 2px solid #009879;
          }
        "))
    ),
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
      theme = shinytheme("united"),
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

      tabPanel("運動強國",
          fluidRow(
            align = "center",
            column(3),
            column(3,
              pickerInput(
                inputId = "rank_Sports", 
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
              sliderInput("rank_Rank", "顯示名次", min=1, max=50, value=1)
            ),
            column(3)
          ),
          fluidRow(
            align = "center",
            column(4),
             column(4,plotOutput("rank_wordCloudPlot")),
            column(4),
          ),
          fluidRow(
            align = "center",
            column(12,uiOutput("rank_dynamic_html")),
            
          ),

      ),
      tabPanel("歷史回顧",
              mainPanel(

                fluidRow(
                    column(12, plotlyOutput("history_plot")),      # pic1
                    column(12, plotlyOutput("history_height_weight")), # pic2
                    column(12, plotlyOutput("history_gold_age")),  # pic3
                ),

                uiOutput("history_floating_sidebar"),
                
            )
          
      
      
      )
    ),
  )
)