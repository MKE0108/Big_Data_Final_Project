#tabPannel
library(shiny)
library(shinydashboard)
library(shinyWidgets)
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("全季運動", tabName = "Full_season"),
      menuItem("前五後五", tabName = "Top5_Bot5"),
      menuItem("指定運動", tabName = "selectSport")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Full_season",
        fluidRow(
            box(width = 12,height = 220,
                      title = "季節", status = "warning",
                      radioButtons("Full_season_season", NULL,
                          choices = list("Summer" = "Summer",
                                         "Winter" = "Winter"),
                          selected = "Summer")
            ),
            box(width = 12, title = "性別比例圓餅圖", status = "info", solidHeader = TRUE,
                plotlyOutput("Full_season_pie")
            ),
            box(width = 12, title = "各選手身高四分位數圖", status = "info", solidHeader = TRUE,
                plotlyOutput("Full_season_height")
            ),
            box(width = 12, title = "各選手體重四分位數圖", status = "info", solidHeader = TRUE,
                plotlyOutput("Full_season_weight")
            )
        )
      ),
      tabItem(tabName = "Top5_Bot5",
            fluidRow(
                box(width = 12,height = 220,
                      title = "季節", status = "warning",
                      radioButtons("Top5_Bot5_season", NULL,
                          choices = list("Summer Olympics" = "Summer",
                                         "Winter Twitter" = "Winter"),
                          selected = "Summer")
                ),  
                box(width = 12, title = "前五後五選手四分位數圖", status = "info", solidHeader = TRUE,
                    plotlyOutput("Top5_Bot5_height")
                ),
                box(width = 12, title = "前五後五選手四分位數圖", status = "info", solidHeader = TRUE,
                    plotlyOutput("Top5_Bot5_weight")
                )
            )
      ),
      tabItem(tabName = "selectSport",
        fluidRow(
            box(width = 12,title = "運動", status ="info", solidHeader = TRUE,
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
            box(width = 12, title = "性別比例圓餅圖", status = "info", solidHeader = TRUE,
                plotlyOutput("selectSport_pie")
            ),
            box(width = 12, title = "身高和體重的四分位數圖", status = "info", solidHeader = TRUE,
                plotlyOutput("selectSport_height_weight")
            ),
        )
      )

    )

  )
)

