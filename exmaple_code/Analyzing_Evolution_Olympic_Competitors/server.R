library(shiny)
library(plotly)
source("global.R") 

function(input, output) {
  
  filterDataBySeason <- function(data, season) {
    filtered <- data %>%
      filter(Season == season)
    return(filtered)
  }
  
  # Reactive expression to filter data based on selected season for multiple datasets
  filteredData <- reactive({
    list(
      womenInOlympicsFiltered = filterDataBySeason(womenInOlympics, input$season),
      notNullMedalsFiltered = filterDataBySeason(notNullMedals, input$season),
      gold_medals_filter = filterDataBySeason(gold_medals, input$season)
    )
  })
  
  # Render Plotly plot based on filtered data
  output$plot <- renderPlotly({
    tmp <- filteredData()$womenInOlympicsFiltered    # Get the filtered data
    result <- createColorScaleAndNormalize(tmp, "Count")
    
    normalizedData <- result$data
    colorscale <- result$colorscale
   
    p <- plot_ly(data = tmp, x = ~as.factor(Year), y = ~Count, type = 'bar',
                 marker = list(color = ~Count, colorscale = colorscale, showscale = TRUE)) %>%
      layout(title = paste("Olympic Women Medals Per Edition -", input$season),
             xaxis = list(title = "年份"),
             yaxis = list(title = "數量"),
             bargap = 0.2)  # 設定條形之間的間隔
    p
  })
  
  output$height_weight <- renderPlotly({
    tmp <- filteredData()$notNullMedalsFiltered  
    
    # 创建事件到颜色的映射
    events <- unique(tmp$Event)
    colors <- RColorBrewer::brewer.pal(min(length(events), 9), "Paired")  
    if (length(events) > 9) {
      colors <- colorRampPalette(colors)(length(events)) 
    }
    color_map <- setNames(colors, events)  

    p <- plot_ly(tmp, x = ~Height, y = ~Weight, type = 'scatter', mode = 'Paired',
                 marker = list(color = ~color_map[Event], opacity = 0.9), 
                 text = ~paste("Event:", Event, "Sport:", Sport),  #text on scatter
                 hoverinfo = 'text') %>%
      layout(title = paste("Height vs Weight of Olympic Medalists -", input$season),
             xaxis = list(title = "Height"),
             yaxis = list(title = "Weight"))
    p
  })
  
  display.brewer.all()  
  
  output$gold_age <- renderPlotly({
    tmp <- filteredData()$gold_medals_filter  
    tmp <- tmp %>%
      group_by(Age) %>%
      summarize(Count = n())
    
    #print(tmp)  # 检查tmp数据
    
    result <- createColorScaleAndNormalize(tmp, "Count")
    tmp <- result$data
    colorscale <- result$colorscale
    #print(tmp)  # 检查tmp数据
    # 确保颜色映射正确
    p <- plot_ly(data = tmp, x = ~Age, type = "bar",
                 y = ~Count, autobinx = FALSE,
                 xbins = list(start = min(tmp$Age) - 0.5, end = max(tmp$Age) + 0.5, size = 1),
                 marker = list(color = ~Count, colorscale = colorscale, showscale = TRUE)) %>%
      layout(title = paste("Age Distribution of Olympic Gold Medalists", input$season),
             xaxis = list(title = "Age"),
             yaxis = list(title = "Frequency of Gold Medals"),
             bargap = 0.1)
    p
  })
  
  
}
