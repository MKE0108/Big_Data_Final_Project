library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(maps)
library(tibble)
library(leaflet)
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
    unique_data
  })
  
  output$map <- renderLeaflet({
    unique_data=datasetInput_for_map2()
    
    UserIcon <- icons(
      iconUrl = paste0("Country_image/", unique_data$NOC, ".png"),
      iconWidth = 22, iconHeight = 11
    )
    
    m <- leaflet(data = unique_data) %>%
      addTiles() %>%
      addMarkers(~long, ~lat, icon = UserIcon, 
                 popup = ~paste("<div style='font-size: 20px;'>",
                                "<span style='text-decoration: underline;'>", region, "</span>",
                                "<br><span style='color: gold;'>Gold: ", Gold, "</span>",
                                "<br><span style='color: silver;'>Silver: ", Silver, "</span>",
                                "<br>Bronze: ", Bronze,
                                "</div>"),
                 label = ~region,
                 labelOptions = labelOptions(noHide = FALSE, direction = 'auto', style = list('color' = 'black', 'font-size' = '16px')))%>%
      setView(lng = 100, lat = 0, zoom = 2)
    m
  })
})
