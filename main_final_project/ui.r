library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(maps)
library(tibble)
library(leaflet)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
source("global.r")
shinyUI(
  fluidPage(
    shinyWidgets::useShinydashboard(),
    tags$head(
       tags$script(src = "http://d3js.org/d3.v3.min.js"),
        tags$style(HTML("
            .content-table-full-page {
              border-collapse: collapse;
              margin: 25px 0;
              font-size: 0.9em;
              min-width: 600px;
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
    #下拉選單圖片＋文字組合時，文字的template
    tags$head(tags$style("
              .jhr{
              display: inline;
              vertical-align: middle;
              padding-left: 10px;
    }")),
    #導覽頁padding
    div(style="padding: 1px 0px; width: '100%'",
        titlePanel(
                title="", windowTitle="My Window Title"
        )
    ),
    navbarPage(
      theme = shinytheme("paper"),
      title=div(img(src="https://static01.nyt.com/images/2012/07/15/magazine/15wmt/15wmt-jumbo.jpg", height = "25", style = "margin-right: 5px;"), "Olympic"),
      tabPanel("各國參賽人數",
      mainPanel(width = 12,
        fluidRow(
          column(width = 12,offset = 1,
              box(width = 5,height = 150,
                title = "解釋", status = "warning",
                p("這是一個地圖，顯示了各國家在不同年份的奧運獲獎情況。")
              ),
              box(width = 5,height = 150,
                title = "年份", status = "warning",
                sliderInput("Map1_year", NULL,  min = min(Map1_Year_sel),max = max(Map1_Year_sel), value = min(Map1_Year_sel),step = 1, animate= animationOptions(interval=2500, loop=TRUE)) # 0616更新
                
              )
          )
        ),
        fluidRow(
          column(width = 12,offset = 1,
            box(width = 10,
              title = "地圖", status = "warning", solidHeader = TRUE,
              plotOutput("Year_Map_Plot")
            ),
          ),
        ),

      )
    #  sidebarLayout(
    #    sidebarPanel(
    #      sliderInput("Map1_year", "Choose a Year:",  min = min(Map1_Year_sel),max = max(Map1_Year_sel), value = min(Map1_Year_sel),step = 1, animate= animationOptions(interval=2500, loop=TRUE)) # 0616更新
    #    ),    
    #    mainPanel(
    #      plotOutput("Year_Map_Plot")
    #    )
    # )
      ),

      tabPanel("探索國家",
        dashboardPage(
          dashboardHeader(disable=TRUE),
          dashboardSidebar(
            sidebarMenu(
              menuItem("總覽", tabName = "overview", icon = icon("globe")),
              menuItem("運動強國", tabName = "topcountry", icon = icon("trophy")),
              menuItem("各國資訊", tabName = "countryInfo", icon = icon("info-circle"))

            )
          ),
          dashboardBody(
              fluidPage(
                tabItems(
                  tabItem(tabName = "overview",
                    leafletOutput("overview_map", width = "100%", height = "800px")
                  ),
                  tabItem(tabName = "topcountry",
                        box(width = 12,title = "運動", status ="info", solidHeader = TRUE,
                            pickerInput(
                              inputId = "rank_Sports", 
                              label = NULL, 
                              choices =  ALLSPORT,
                              choicesOpt = list(
                                content = 
                                  SPORT_HTML
                                )
                              ,
                              selected = "All"
                            ),
                        ),
                        box(width = 12,title = "排行榜", status ="info", solidHeader = TRUE,collapsible=TRUE,
                            #output DT
                            DT::dataTableOutput("global_rank_table")
                            # uiOutput("rank_dynamic_html")
                        ),
          
                  ),
                  tabItem(tabName = "countryInfo",
                    # fluidRow(
                      column(6,
                            box(height = 110,width = NULL,title = "區域", status ="info", solidHeader = TRUE,
                                    pickerInput(
                                      inputId = "explore_Country", 
                                      label = NULL, 
                                      choices =  ALL_NOC,
                                      choicesOpt = list(
                                        content = 
                                          NOC_HTML
                                        )
                                      ,
                                      selected = "TPE"
                                    ),
                            ),
                             box(height = 236,width = NULL,title = "簡介", status ="info", solidHeader = TRUE,
                                  p("這是一個地圖，顯示了各國家在不同年份的奧運獲獎情況。")
                             
                             )
                            
                      ),
                      box(width = 6,title = "地理資訊", status ="info", solidHeader = TRUE,
                          leafletOutput("ex_country_map", width = "100%", height = "300px")
                      ),

                      box(width = 12,title = "排行榜", status ="info", solidHeader = TRUE,
                          DT::dataTableOutput("country_rank_table")
                      )

                  )
              )
              
            )
          )
        )
      ),




      tabPanel("歷史回顧",

          mainPanel(width = 12,
            fluidRow(
              column(12,
                column(4,
                  box(width = NULL,height = 220,
                      title = "季節", status = "warning",
                      radioButtons("history_season", NULL,
                          choices = list("Summer Olympics" = "Summer",
                                                          "Winter Twitter" = "Winter"),
                          selected = "Summer")
                  ),                
                  box(width = NULL,height = 220,
                      title = "解釋", status = "warning",
                      p("這是一個地圖，顯示了各國家在不同年份的奧運獲獎情況。")
                  ),
                ),
                column(8,
                    box(width = NULL,
                          title = "運動/體重/身高", status = "warning", solidHeader = TRUE,
                          plotlyOutput("history_height_weight")
                    ),
                )
              )
            ),
            fluidRow(
              column(12,
                box(width = 6,
                    title = "歷史女性得牌數量", status = "warning", solidHeader = TRUE,
                    plotlyOutput("history_plot")
                ),                
                box(width = 6,
                     title = "歷史金牌獲獎者年紀", status = "warning", solidHeader = TRUE,
                     plotlyOutput("history_gold_age")
                ),
              ),
            ),
          )
      ),
  )
)
)

