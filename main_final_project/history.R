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