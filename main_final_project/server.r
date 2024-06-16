library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(maps)
library(tibble)
library(leaflet)
library(dplyr)
library(purrr)
source("global.r")

shinyServer(function(input, output) {
### map ####
  datasetInput_for_map1 <- reactive({
    data_one_year <- data_regions %>% 
      filter(Year == input$year) %>%  
      group_by(region) %>%
      summarize(athlete_num = length(unique(ID)))
    data_one_year
  })
  output$Year_Map_Plot <- renderPlot({
    data_one_year=datasetInput_for_map1()
  
    world <- map_data("world")
    
    mapdat <- tibble(region=unique(world$region))
    mapdat <- mapdat %>% left_join(data_one_year, by="region") 
    mapdat$athlete_num[is.na(mapdat$athlete_num)] <- 0
    
    world <- left_join(world, mapdat, by="region")
    
    ggplot(world, aes(x = long, y = lat, group = group)) +
      geom_polygon(aes(fill = athlete_num)) + 
      labs(title = input$year, x = NULL, y = NULL) +
      theme(axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.background = element_rect(fill = "navy"),
            plot.title = element_text(hjust = 0.5)) +
      guides(fill = guide_colourbar(title = "Athletes")) +
      scale_fill_gradient(low = "white", high = "red")
  })

  output$map <- renderLeaflet({
    unique_data=NOC_summary_with_map
    # top3_res=datasetInput_for_map2()[2:11]
    
    iconUrl=paste0("Country_image/", unique_data$NOC, ".png")
    for (i in 1:length(iconUrl)){
      if (!file.exists(iconUrl[i])){
        iconUrl[i] <- "Country_image/default.png"
      }
    }

    UserIcon <- icons(
      iconUrl = iconUrl,
      iconWidth = 22, iconHeight = 11
    )


    m <- leaflet(data = unique_data) %>%
      addTiles() %>%
      addMarkers(~long, ~lat, icon = UserIcon, 
                 popup = ~paste0(

"<div style='font-size: 15px; text-align: center;font-weight: bold;'>",
    "<span style='text-decoration: underline;'>", region,"( ALL + TOP3 )", "</span>",
"</div>",

  "<div style='font-size: 12px;'>",
    "<table class='content-table'>",
      "<thead>",
      "<tr>",
      "<th style='text-align: center;'>Sport</th>",
      "<th style='text-align: center; color: gold;'>Gold</th>",
      "<th style='text-align: center; color: seashell;'>Silver</th>",
      "<th style='text-align: center; color: brown;'>Bronze</th>",
      "</tr>",
      "</thead>",
      "<tbody>",
      "<tr>",
      "<td style='text-align: center;'>
        <p style='font-size: 12px;margin-top: 3px ;margin-bottom:3px;font-weight: bold;'>","ALL","</p>
      </td>",
      "<td style='text-align: center;font-weight: bold;'>",Gold,"</td>",
      "<td style='text-align: center;font-weight: bold;'>",Silver,"</td>",
      "<td style='text-align: center;font-weight: bold;'>",Bronze,"</td>",
      "</tr>",



      "<tr>",
      "<td style='text-align: center;'>
        <img src='","http://127.0.0.1:5500/Sports_image/",gsub(" ","_",Sport1), "_pictogram.png' onerror='this.onerror=null;this.src=\"http://127.0.0.1:5500/Sports_image/default.png\"' width='30px'>
        <p style='font-size: 10px; margin-top: 3px ;margin-bottom:3px;font-weight: bold;'>",Sport1,"</p>
      </td>",
      "<td style='text-align: center;font-weight: bold;'>",Gold1,"</td>",
      "<td style='text-align: center;font-weight: bold;'>",Silver1,"</td>",
      "<td style='text-align: center;font-weight: bold;'>",Bronze1,"</td>",
      "</tr>",
      "<tr>",
      "<td style='text-align: center;'>
         <img src='","http://127.0.0.1:5500/Sports_image/",gsub(" ","_",Sport2), "_pictogram.png' onerror='this.onerror=null;this.src=\"http://127.0.0.1:5500/Sports_image/default.png\"' width='30px'>
         <p style='font-size: 10px; margin-top: 3px ;margin-bottom:3px;font-weight: bold;'>",Sport2,"</p>",
      "</td>",
      "<td style='text-align: center;font-weight: bold;'>",Gold2,"</td>",
      "<td style='text-align: center;font-weight: bold;'>",Silver2,"</td>",
      "<td style='text-align: center;font-weight: bold;'>",Bronze2,"</td>",
      "</tr>",
      "<tr>",
      "<td style='text-align: center;'>
         <img src='","http://127.0.0.1:5500/Sports_image/",gsub(" ","_",Sport3), "_pictogram.png' onerror='this.onerror=null;this.src=\"http://127.0.0.1:5500/Sports_image/default.png\"' width='30px'>
         <p style='font-size: 10px; margin-top: 3px ;margin-bottom:3px;font-weight: bold;'>",Sport3,"</p>",
      "</td>",
      "<td style='text-align: center;font-weight: bold;'>",Gold3,"</td>",
      "<td style='text-align: center;font-weight: bold;'>",Silver3,"</td>",
      "<td style='text-align: center;font-weight: bold;'>",Bronze3,"</td>",
      "</tr>",
      "</tbody>",
    "</table>",
  "</div>"
    ),
      label = ~region,
      labelOptions = labelOptions(noHide = FALSE, direction = 'auto', style = list('color' = 'black', 'font-size' = '16px')))%>%
      setView(lng = 100, lat = 0, zoom = 2)
    m
  })
### sport rank ####
  output$rank_dynamic_html <- renderUI({
    req(input$rank_Sports)  # 確保有選擇運動
    req(input$rank_Rank)  # 確保有選擇名次
    Sport=input$rank_Sports
    topNth=input$rank_Rank
    if(Sport=="All"){
      D=data[!is.na(data),]
    }else{
      D=data[which(data$Sport==Sport),]
    }
     D=D%>%group_by(Year,NOC,Sport,Event,Medal)%>%filter(!is.na(Medal))%>%summarise(n=1)%>%group_by(NOC)%>%summarise(Gold=sum(Medal=="Gold"),Silver=sum(Medal=="Silver"),Bronze=sum(Medal=="Bronze"),Weight=sum(Medal=="Bronze")+1.5*sum(Medal=="Silver")+2*sum(Medal=="Gold"))%>%arrange(desc(Weight))%>%head(topNth)
    #生成文字雲
    
    words=D$NOC
    freqs=as.numeric(D$Weight)
    output$rank_wordCloudPlot <- renderPlot({
      wordcloud(words, freqs, scale=c(5,0.5), colors=brewer.pal(8, "Dark2"))
      
    })
   
    HTML(
      paste0(
        "<table class='content-table-full-page'>",
        "<thead>",
        "<tr>",
        "<th style='text-align: center;'>NOC</th>",
        "<th style='text-align: center; color: gold;'>Gold</th>",
        "<th style='text-align: center; color: seashell;'>Silver</th>",
        "<th style='text-align: center; color: brown;'>Bronze</th>",
        "</tr>",
        "</thead>",
        "<tbody>",
        paste(apply(D, 1, function(row) {
            paste0(
            "<tr>",
            "<td style='text-align: center;'>
              <img src='","http://127.0.0.1:5500/Country_image/",row[1], ".png' onerror='this.onerror=null;this.src=\"http://127.0.0.1:5500/Country_image/default.png\"' style='margin-top: 10px;' width='50px'>
              <p style='font-size: 15px; margin-top: 3px ;margin-bottom:3px;font-weight: bold;'>",noc[which(noc$NOC==row[1]),2],"</p>
            </td>",
            "<td style='text-align: center;'>", row[2], "</td>",
            "<td style='text-align: center;'>", row[3], "</td>",
            "<td style='text-align: center;'>", row[4], "</td>",
            "</tr>"
            )
        }), collapse = ""),
        "</tbody>",
        "</table>"
      
      )
    )

  })



### history ####
output$history_floating_sidebar <- renderUI({
        sidebarPanel(id = "floating-sidebar",
            radioButtons("history_season", "Choose Season:",
                        choices = list("Summer Olympics" = "Summer",
                                        "Winter Twitter" = "Winter"),
                        selected = "Summer")
        )
})


filterDataBySeason <- function(data, season) {
    filtered <- data %>%
      filter(Season == season)
    return(filtered)
  }
  
  # Reactive expression to filter data based on selected season for multiple datasets
  filteredData <- reactive({
    list(
      womenInOlympicsFiltered = filterDataBySeason(womenInOlympics, input$history_season),
      notNullMedalsFiltered = filterDataBySeason(notNullMedals, input$history_season),
      gold_medals_filter = filterDataBySeason(gold_medals, input$history_season)
    )
  })
  
  # Render Plotly plot based on filtered data
  output$history_plot <- renderPlotly({
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
  
  output$history_height_weight <- renderPlotly({
    tmp <- filteredData()$notNullMedalsFiltered  
    p <- plot_ly(tmp, x = ~Height, y = ~Weight, type = 'scatter', mode = 'markers',
                 marker = list(color = 'rgba(135, 206, 250, 0.8)')) %>%
      layout(title = paste("Height vs Weight of Olympic Medalists -", input$season),
             xaxis = list(title = "Height"),
             yaxis = list(title = "Weight"))
    p
  })
  
  output$history_gold_age <- renderPlotly({
    tmp <- filteredData()$gold_medals_filter  
    tmp <- tmp %>%
      group_by(Age) %>%
      summarize(Count = n())
    
    print(tmp)  # 检查tmp数据
    
    result <- createColorScaleAndNormalize(tmp, "Count")
    tmp <- result$data
    colorscale <- result$colorscale
    print(tmp)  # 检查tmp数据
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
})


