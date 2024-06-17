server <- function(input, output) { 
output$Full_season_pie <- renderPlotly({
    filtered_data <- data %>% filter(Season == input$Full_season_season) %>%
        group_by(ID) %>%
        # filter(Year == max(Year)) %>%
        slice(1) %>%
        ungroup() 
      data <- filtered_data
      gender_counts <- table(data$Sex)
      
      gender_pie <- plot_ly(labels = names(gender_counts), values = as.numeric(gender_counts), type = 'pie',
                            marker = list(colors = c('#FF9999','#66b3ff')), # 自訂顏色，例如粉色和藍色
                            textinfo = 'label+percent',
                            insidetextorientation = 'radial')
      gender_pie
})
output$Full_season_height <- renderPlotly({

Hfiltered_data <- data %>% filter(!is.na(Height), Season == input$Full_season_season) %>%
  group_by(ID) %>%
  # filter(Year == max(Year)) %>%
  slice(1) %>%
  ungroup()

Hfiltered_data <- Hfiltered_data %>%
  group_by(Sport) %>%
  mutate(HeightQ1 = quantile(Height, 0.25),
         HeightQ3 = quantile(Height, 0.75),
         HeightIQR = HeightQ3 - HeightQ1) %>%
  filter(Height >= (HeightQ1 - 1.5 * HeightIQR) & Height <= (HeightQ3 + 1.5 * HeightIQR)) %>%
  ungroup()

plot_ly(Hfiltered_data, y = ~Height, color = ~Sport, type = "box") %>%
  layout(
         yaxis = list(title = "身高 (cm)"),
         xaxis = list(title = "運動類型"))



})
output$Full_season_weight <- renderPlotly({


Wfiltered_data <- data %>% filter(!is.na(Weight), Season == input$Full_season_season) %>%
  group_by(ID) %>%
  # filter(Year == max(Year)) %>%
  slice(1) %>%
  ungroup()

Wfiltered_data <- Wfiltered_data %>%
  group_by(Sport) %>%
  mutate(WeightQ1 = quantile(Weight, 0.25),
         WeightQ3 = quantile(Weight, 0.75),
         WeightIQR = WeightQ3 - WeightQ1) %>%
  filter(Weight >= (WeightQ1 - 1.5 * WeightIQR) & Weight <= (WeightQ3 + 1.5 * WeightIQR)) %>%
  ungroup()


plot_ly(Wfiltered_data, y = ~Weight, color = ~Sport, type = "box") %>%
  layout(yaxis = list(title = "體重 (kg)"),
         xaxis = list(title = "運動類型"))



})
output$Top5_Bot5_height <- renderPlotly({

filtered_data <- data %>%
  filter(!is.na(Height), Season == input$Top5_Bot5_season) %>%
  group_by(ID) %>%
  # filter(Year == max(Year)) %>%
  slice(1) %>%
  ungroup()

filtered_data <- filtered_data %>%
  group_by(Sport) %>%
  mutate(HeightQ1 = quantile(Height, 0.25),
         HeightQ3 = quantile(Height, 0.75),
         HeightIQR = HeightQ3 - HeightQ1) %>%
  filter(Height >= (HeightQ1 - 1.5 * HeightIQR) & Height <= (HeightQ3 + 1.5 * HeightIQR)) %>%
  ungroup()

FFtop_sports_by_height <- filtered_data %>%
  group_by(Sport) %>%
  summarize(Average_Height = mean(Height, na.rm = TRUE)) %>%
  arrange(desc(Average_Height)) %>%
  slice_head(n = 5) 

FFtop_sports_data <- filtered_data %>%
  filter(Sport %in% FFtop_sports_by_height$Sport)



LFtop_sports_by_height <- filtered_data %>%
  group_by(Sport) %>%
  summarize(Average_Height = mean(Height, na.rm = TRUE)) %>%
  arrange(Average_Height) %>%
  slice_head(n = 5)

LFtop_sports_data <- filtered_data %>%
  filter(Sport %in% LFtop_sports_by_height$Sport)

all_sports <- unique(c(as.character(FFtop_sports_data$Sport), as.character(LFtop_sports_data$Sport)))

colors <- setNames(rainbow(length(all_sports)), all_sports)

FFplot <- plot_ly(FFtop_sports_data, y = ~Height, color = ~Sport, colors = colors, type = "box") %>%
  layout(
         yaxis = list(title = "身高 (cm)"),
         xaxis = list(title = "運動類型"))

LFplot <- plot_ly(LFtop_sports_data, y = ~Height, color = ~Sport, colors = colors, type = "box") %>%
  layout(
         yaxis = list(title = "身高 (cm)"),
         xaxis = list(title = "運動類型"))


Hcombined_plot <- subplot(FFplot, LFplot, 
                           nrows = 1, 
                           shareY = TRUE, 
                           titleX = FALSE) 

Hcombined_plot <- layout(Hcombined_plot,
                          xaxis = list(title = "前五"),
                          xaxis2 = list(title = "後五"),
                          yaxis = list(title = "身高 (cm)"))
Hcombined_plot

})
output$Top5_Bot5_weight <- renderPlotly({

Wfiltered_data <- data %>%
  filter(!is.na(Weight), Season == input$Top5_Bot5_season) %>%
  group_by(ID) %>%
  # filter(Year == max(Year)) %>%
  slice(1) %>%
  ungroup()

Wfiltered_data <- Wfiltered_data %>%
  group_by(Sport) %>%
  mutate(WeightQ1 = quantile(Weight, 0.25),
         WeightQ3 = quantile(Weight, 0.75),
         WeightIQR = WeightQ3 - WeightQ1) %>%
  filter(Weight >= (WeightQ1 - 1.5 * WeightIQR) & Weight <= (WeightQ3 + 1.5 * WeightIQR)) %>%
  ungroup()

FFtop_sports_by_Weight <- Wfiltered_data %>%
  group_by(Sport) %>%
  summarize(Average_Weight = mean(Weight, na.rm = TRUE)) %>%
  arrange(desc(Average_Weight)) %>%
  slice_head(n = 5) 

FFtop_sports_data <- Wfiltered_data %>%
  filter(Sport %in% FFtop_sports_by_Weight$Sport)


LFtop_sports_by_Weight <- Wfiltered_data %>%
  group_by(Sport) %>%
  summarize(Average_Weight = mean(Weight, na.rm = TRUE)) %>%
  arrange(Average_Weight) %>%
  slice_head(n = 5)

LFtop_sports_data <- Wfiltered_data %>%
  filter(Sport %in% LFtop_sports_by_Weight$Sport)

all_sports <- unique(c(as.character(FFtop_sports_data$Sport), as.character(LFtop_sports_data$Sport)))

colors <- setNames(rainbow(length(all_sports)), all_sports)

FFWplot <- plot_ly(FFtop_sports_data, y = ~Weight, color = ~Sport, colors = colors, type = "box") %>%
  layout(
         yaxis = list(title = "體重 (kg)"),
         xaxis = list(title = "運動類型"))

LFWplot <- plot_ly(LFtop_sports_data, y = ~Weight, color = ~Sport, colors = colors, type = "box") %>%
  layout(
         yaxis = list(title = "體重 (kg)"),
         xaxis = list(title = "運動類型"))

Wcombined_plot <- subplot(FFWplot, LFWplot, 
                           nrows = 1, # 一行
                           shareY = TRUE, # 共享Y轴
                           titleX = FALSE) # 不显示单独的x轴标题

Wcombined_plot <- layout(Wcombined_plot,
                         
                          xaxis = list(title = "前五"),
                          xaxis2 = list(title = "後五"),
                          yaxis = list(title = "體重 (kg)"))

})

output$selectSport_pie <- renderPlotly({
    filtered_data <- data %>% filter( Sport == input$selectSport_sport) %>%
        group_by(ID) %>%
        # filter(Year == max(Year)) %>%
        slice(1) %>%
        ungroup() 
    data <- filtered_data
    gender_counts <- table(data$Sex)
      
    gender_pie <- plot_ly(labels = names(gender_counts), values = as.numeric(gender_counts), type = 'pie',
                            marker = list(colors = c('#FF9999','#66b3ff')), # 自訂顏色，例如粉色和藍色
                            textinfo = 'label+percent',
                            insidetextorientation = 'radial')
    gender_pie
})
output$selectSport_height_weight <- renderPlotly({
    filtered_data <- data %>%
      filter(Sport == input$selectSport_sport, !is.na(Height), !is.na(Weight)) %>%
      group_by(ID) %>%
      filter(Year == max(Year)) %>%
      slice(1) %>%
      ungroup() %>%
      select(Name, Sex, Age, Height, Weight) %>%
      # 計算IQR並去除極端值
      mutate(HeightQ1 = quantile(Height, 0.25),
             HeightQ3 = quantile(Height, 0.75),
             HeightIQR = HeightQ3 - HeightQ1,
             WeightQ1 = quantile(Weight, 0.25),
             WeightQ3 = quantile(Weight, 0.75),
             WeightIQR = WeightQ3 - WeightQ1) %>%
      filter(Height >= (HeightQ1 - 1.5 * HeightIQR) & Height <= (HeightQ3 + 1.5 * HeightIQR),
             Weight >= (WeightQ1 - 1.5 * WeightIQR) & Weight <= (WeightQ3 + 1.5 * WeightIQR))
    data <- filtered_data
    # 身高四分位數圖
    height_plot <- plot_ly(data, y = ~Height, type = "box", name = "身高") %>%
      layout(title = "身高")
    
    # 體重四分位數圖
    weight_plot <- plot_ly(data, y = ~Weight, type = "box", name = "體重") %>%
      layout(title = "體重")
    # 合併圖表
    combined_plot <- subplot(height_plot, weight_plot, nrows = 1, shareX = TRUE, shareY = FALSE) %>%
      layout()
    combined_plot
})

}