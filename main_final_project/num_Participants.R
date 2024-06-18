mainPanel(width = 12,
    #建立一個空白padding

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