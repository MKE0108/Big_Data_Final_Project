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