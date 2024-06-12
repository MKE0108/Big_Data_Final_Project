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
  
  datasetInput_for_map2 <- reactive({
    # 數據合併並過濾
    data_regions <- data %>%
      left_join(noc, by="NOC") %>%
      filter(!is.na(region), Medal %in% c("Gold", "Silver", "Bronze"))
    
    # 統計獎牌數量
    medal_counts <- data_regions %>%
      group_by(Year, Event, NOC,region, Medal) %>%
      summarise(count = 1, .groups = 'drop') %>%
      pivot_wider(names_from = Medal, values_from = count, values_fill = list(count = 0))
    
    medal_counts <- medal_counts %>%
      group_by(NOC, region) %>%
      summarise(
        Gold = sum(Gold, na.rm = TRUE),
        Silver = sum(Silver, na.rm = TRUE),
        Bronze = sum(Bronze, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # 合併並準備世界地圖數據
    world_map <- map_data("world")[, c(1, 2, 5)]  # 簡化讀取方式
    map_data_final <- merge(world_map, medal_counts, by = "region", all.x = TRUE)
    
    # 處理 NA 值
    map_data_final[is.na(map_data_final)] <- 0
    
    # 去除重複、NA數據和NOC=0的數據
    unique_data <- distinct(map_data_final, region, .keep_all = TRUE) %>%
      filter(!is.na(NOC), NOC != "0")
    
    region_centers <- world_map %>%
      group_by(region) %>%
      summarise(
        center_long = mean(long, na.rm = TRUE),
        center_lat = mean(lat, na.rm = TRUE)
      )
    unique_data <- unique_data %>%
      left_join(region_centers, by = "region") %>%
      mutate(long = center_long, lat = center_lat) %>%
      select(-center_long, -center_lat)  # 移除暫存的中心位置列
    #ADD top 3 sports COLUMN
    unique_data[, c("Sport1", "Gold1", "Silver1", "Bronze1", "Sport2", "Gold2", "Silver2", "Bronze2", "Sport3", "Gold3", "Silver3", "Bronze3")] <- NA

    # add top 3 sports
    for(NOC in unique_data$NOC){
        A=data[which(data$NOC==NOC),]%>%group_by(Year,Sport,Event,Medal)%>%filter(!is.na(Medal))%>%summarise(n=1)
        #統計每項運動的總獎牌數
        weighted_A=A
        weighted_A[which(weighted_A$Medal=="Gold"),]$n=weighted_A[which(weighted_A$Medal=="Gold"),]$n*2
        weighted_A[which(weighted_A$Medal=="Silver"),]$n=weighted_A[which(weighted_A$Medal=="Silver"),]$n*1.5
        weighted_A[which(weighted_A$Medal=="Bronze"),]$n=weighted_A[which(weighted_A$Medal=="Bronze"),]$n*1
        weighted_A=weighted_A%>%group_by(Sport)%>%summarise(n=sum(n))
        weighted_A=weighted_A%>%arrange(desc(n))
        #get top3
        top3=weighted_A[1:3,]$Sport
        top3_res=c()
        #top3 得牌數
        for (topSport in top3){
            if(is.na(topSport)==FALSE){
                Gold=sum(A[which(A$Sport==topSport & A$Medal=="Gold"),]$n)
                Silver=sum(A[which(A$Sport==topSport & A$Medal=="Silver"),]$n)
                Bronze=sum(A[which(A$Sport==topSport & A$Medal=="Bronze"),]$n)
                top3_res=c(top3_res,topSport,Gold,Silver,Bronze)
            }else{
                top3_res=c(top3_res,topSport,0,0,0)
            }
        }

        unique_data[which(unique_data$NOC==NOC),8:19]=top3_res

    }



    unique_data
  })
  
  output$map <- renderLeaflet({
    unique_data=datasetInput_for_map2()
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
tags$style("
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

          "),
  
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
        <img src='","http://127.0.0.1:5500/Sports_image/",gsub(" ","_",Sport1),"_pictogram.png","' width='30'>
        <p style='font-size: 10px; margin-top: 3px ;margin-bottom:3px;font-weight: bold;'>",Sport1,"</p>
      </td>",
      "<td style='text-align: center;font-weight: bold;'>",Gold1,"</td>",
      "<td style='text-align: center;font-weight: bold;'>",Silver1,"</td>",
      "<td style='text-align: center;font-weight: bold;'>",Bronze1,"</td>",
      "</tr>",
      "<tr>",
      "<td style='text-align: center;'>
        <img src='","http://127.0.0.1:5500/Sports_image/",gsub(" ","_",Sport2),"_pictogram.png","' width='30'>
         <p style='font-size: 10px; margin-top: 3px ;margin-bottom:3px;font-weight: bold;'>",Sport2,"</p>",
      "</td>",
      "<td style='text-align: center;font-weight: bold;'>",Gold2,"</td>",
      "<td style='text-align: center;font-weight: bold;'>",Silver2,"</td>",
      "<td style='text-align: center;font-weight: bold;'>",Bronze2,"</td>",
      "</tr>",
      "<tr>",
      "<td style='text-align: center;'>
        <img src='","http://127.0.0.1:5500/Sports_image/",gsub(" ","_",Sport3),"_pictogram.png","' width='30'>
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
})
