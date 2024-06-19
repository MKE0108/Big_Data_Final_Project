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


### page 1 ####
global_participation_ui<-fluidPage(
    div(class = "mytitle", "🌍️各國參賽人數"),
    fluidPage( id = "show_body",
        #div padding
        div(style="padding: 15px 0px; width: '100%'",),
        fluidRow(
                column(width = 12,offset = 0,
                    box(width = 6,height = 150,
                    title = "年份", status = "warning", solidHeader = TRUE,
                    sliderInput("Map1_year", NULL,  min = min(Map1_Year_sel),max = max(Map1_Year_sel), value = min(Map1_Year_sel),step = 1, animate= animationOptions(interval=2500, loop=TRUE)) # 0616更新
                    ),
                    box(width = 6,height = 150,
                    title = "解釋", status = "info", solidHeader = TRUE,
                    p("來自不同國家的運動員數量隨著時間的變化",
                     style="font-size: 18px;padding: 10px 10px;")
                    ),

                )
        ),
        fluidRow(
            column(width = 12,offset = 0,
                box(width = 12,
                        title = "地圖", status = "success", solidHeader = TRUE,
                        plotOutput("Year_Map_Plot")
                ),
            )
        ),

    ) 
)



### page 3 ####
history_ui <- fluidPage(
    div(class = "mytitle", "🏛️歷史回顧"),
    fluidPage(id = "show_body",
        div(style="padding: 15px 0px; width: '100%'",),
        fluidRow(
            column(12,
                    column(4,
                        box(width = NULL,height = 140,  
                            title = "季節", status = "warning",solidHeader = TRUE,
                            radioButtons("history_season", NULL,
                                choices = list("⛱️Summer" = "Summer",
                                                "❄️Winter" = "Winter"),
                                selected = "Summer")
                        ),                
                        box(width = NULL,height = 305,
                            title = "解釋", status = "info",solidHeader = TRUE,
                            p("這是一個地圖，顯示了各國家在不同年份的奧運獲獎情況。")
                        ),
                    ),
                    column(8,
                        box(width = NULL,
                                title = "運動/體重/身高", status = "success", solidHeader = TRUE,
                                plotlyOutput("history_height_weight")
                        ),
                    )
                )
            ),
        fluidRow(
            column(12,
                    box(width = 6,
                        title = "歷史女性得牌數量", status = "success", solidHeader = TRUE,
                        plotlyOutput("history_plot")
                    ),                
                    box(width = 6,
                        title = "歷史金牌獲獎者年紀", status = "success", solidHeader = TRUE,
                        plotlyOutput("history_gold_age")
                    ),
                ),
            ),
    )
)


### page 2 ####
ex_country_map_ui <- fluidPage(
    div(class = "mytitle", "🗺️地圖總覽"),
    fluidPage(id = "show_body",
        div(style="padding: 15px 0px; width: '100%'",),
        box(width = 12,title = NULL, status ="success", solidHeader = TRUE,
            leafletOutput("overview_map", width = "100%",height = "600px")
        )
    )
)
sport_country_ui <- fluidPage(
    div(class = "mytitle", "🏆️運動強國"),
    fluidPage(id = "show_body",
        div(style="padding: 15px 0px; width: '100%'",),
            box(width = 12,title = "運動", status ="warning", solidHeader = TRUE,
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
            box(width = 12,title = "排行榜", status ="success", solidHeader = TRUE,
                dataTableOutput("global_rank_table")
            )
    )
)


country_info_ui<- fluidPage(
    div(class = "mytitle", "🚩各國資訊"),
    fluidPage(id = "show_body",
    div(style="padding: 15px 0px; width: '100%'",),
        column(6,
                box(height = 110,width = NULL,title = "區域", status ="warning", solidHeader = TRUE,
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
                box(height = 236,width = NULL,title = "解釋", status ="info", solidHeader = TRUE,
                    p("本頁面提供了單個國家的地理資訊和各項運動的獲獎情況，使用者可以在上方選擇國家，查看該國家的相關資訊。",
                        style="font-size: 18px;padding: 10px 10px;")
                )
            ),
            box(width = 6,title = "地理資訊", status ="info", solidHeader = TRUE,
                leafletOutput("ex_country_map", width = "100%", height = "300px")
            ),
            box(width = 12,title = "排行榜", status ="success", solidHeader = TRUE,
                dataTableOutput("country_rank_table")
            )
    )

)
### page 4 ####
Full_season_ui<-fluidPage(
    div(class = "mytitle", "👟全季運動"),
    fluidPage(id = "show_body",
        div(style="padding: 15px 0px; width: '100%'",),
        fluidRow(
            column(12,
                  box(width = 6,height = 150,
                              title = "季節", status = "warning", solidHeader = TRUE,
                              radioButtons("Full_season_season", NULL,
                        choices = list("⛱️Summer" = "Summer",
                                        "❄️Winter" = "Winter"),
                                  selected = "Summer")
                    ),
                    box(width = 6,height = 150,
                      title = "解釋", status = "info", solidHeader = TRUE,
                      p("這是一個地圖，顯示了各國家在不同年份的奧運獲獎情況。")
                    ),

                    box(width = 12, title = "性別比例圓餅圖", status = "success", solidHeader = TRUE,
                        plotlyOutput("Full_season_pie")
                    ),
                    box(width = 12, title = "各選手身高四分位數圖", status = "success", solidHeader = TRUE,
                        plotlyOutput("Full_season_height")
                    ),
                    box(width = 12, title = "各選手體重四分位數圖", status = "success", solidHeader = TRUE,
                        plotlyOutput("Full_season_weight")
                    )
              )
        )
    )
)
Top5_Bot5_ui<-fluidPage(
    div(class = "mytitle", "🏋️‍♂️前五後五"),
    fluidPage(id="show_body",
    div(style="padding: 15px 0px; width: '100%'",),
        fluidRow(
            column(12,
                  box(width = 6,height = 150, solidHeader = TRUE,
                              title = "季節", status = "warning",
                              radioButtons("Top5_Bot5_season", NULL,
                        choices = list("⛱️Summer" = "Summer",
                                        "❄️Winter" = "Winter"),
                                  selected = "Summer")
                    ),
                    box(width = 6,height = 150, solidHeader = TRUE,
                      title = "解釋", status = "info",
                      p("這是一個地圖，顯示了各國家在不同年份的奧運獲獎情況。")
                    ),  
                    box(width = 6, title = "前五後五選手四分位數圖", status = "success", solidHeader = TRUE,
                        plotlyOutput("Top5_Bot5_height")
                    ),
                    box(width = 6, title = "前五後五選手四分位數圖", status = "success", solidHeader = TRUE,
                        plotlyOutput("Top5_Bot5_weight")
                    )
            )
     )
    )
)
selectSport_ui<-fluidPage(
    div(class = "mytitle", "🫵指定運動"),
    fluidPage(id="show_body",
        div(style="padding: 15px 0px; width: '100%'",),
        fluidRow(
            column(12,
                box(width = 12,title = "運動", status ="warning", solidHeader = TRUE,
                    pickerInput(
                            inputId = "selectSport_sport", 
                            label = NULL, 
                            choices =  ALLSPORT[2:length(ALLSPORT)],
                            choicesOpt = list(
                            content = 
                                SPORT_HTML[2:length(SPORT_HTML)]
                            )
                            ,
                            selected = "Basketball"
                    ),
                ),
                box(width = 6, title = "性別比例圓餅圖", status = "success", solidHeader = TRUE,
                    plotlyOutput("selectSport_pie")
                ),
                box(width = 6, title = "身高和體重的四分位數圖", status = "success", solidHeader = TRUE,
                    plotlyOutput("selectSport_height_weight")
                ),
            )
        )
    )
                    
)

### start UI ####
shinyUI(
  fluidPage(id="background",
    shinyWidgets::useShinydashboard(),
    tags$head(tags$link(rel="shortcut icon", href="main_icon_1.png")),
    tags$link(rel = "stylesheet", type = "text/css", href = "myStyle.css"),
    tags$head(
       tags$script(src = "https://cdn.jsdelivr.net/npm/d3@7"),
    ),

    # 導覽頁padding
    div(style="padding: 5px 0px; width: '100%'",),

    navbarPage(
      theme = shinytheme("paper"),
      title=div(img(src="main_icon_1.png", height = "25", style = "margin-right: 5px;"), "Olympic Analysis"),
      
      
      navbarMenu("🔍️探索國家",
        tabPanel("🗺️地圖總覽",
            ex_country_map_ui
        ),
        tabPanel("🏆️運動強國",
            sport_country_ui
        ),
        tabPanel("🚩各國資訊",
           country_info_ui
        )
      ),
      navbarMenu("👨‍👩‍👧身高體重性別",
        tabPanel("👟全季運動",
            Full_season_ui
        ),

        tabPanel("🏋️‍♂️前五後五",
            Top5_Bot5_ui
        ),
        tabPanel("🫵指定運動",
            selectSport_ui
        )

      ),
      
      tabPanel("🌍️各國參賽人數",
        global_participation_ui
      ),


      tabPanel("🏛️歷史回顧",
        history_ui
      ),


  )
)
)

