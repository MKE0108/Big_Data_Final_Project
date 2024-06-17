library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(maps)
library(tibble)
library(leaflet)
library(dplyr)
library(purrr)
library(wordcloud)
source("global.r")

shinyServer(function(input, output, session) {
### map ####
  observe({ # 0616更新
    current_year <- input$Map1_year
    next_year <- Map1_Year_sel[match(current_year, Map1_Year_sel) + 1]
    if (!is.na(next_year)) {
      updateSliderInput(session, "Map1_year", value = next_year)
    }
  })
  
  datasetInput_for_map1 <- reactive({
    data_one_year <- data_regions %>% 
      filter(Year == input$Map1_year) %>%  
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
      labs(title = input$Map1_year, x = NULL, y = NULL) +
      theme(axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.background = element_rect(fill = "navy"),
            plot.title = element_text(hjust = 0.5)) +
      guides(fill = guide_colourbar(title = "Athletes")) +
      scale_fill_gradient(low = "white", high = "red")
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
      labs(title = input$Map1_year, x = NULL, y = NULL) +
      theme(axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.background = element_rect(fill = "navy"),
            plot.title = element_text(hjust = 0.5)) +
      guides(fill = guide_colourbar(title = "Athletes")) +
      scale_fill_gradient(low = "white", high = "red")
  })

  output$overview_map <- renderLeaflet({
    unique_data=NOC_summary_with_map
    # top3_res=datasetInput_for_map2()[2:11]

    iconUrl=c()
    for(n in unique_data$NOC){
        region_name=noc[which(noc$NOC==n),2]
        if(length(region_name)==0){
          iconUrl <- c(iconUrl, "Country_image/default.png")
        }else {
          flag=0
          for(possible_noc in noc$NOC[which(noc$region==region_name)]){
            if(file.exists(paste0("Country_image/",possible_noc,".png"))){
              iconUrl <- c(iconUrl, paste0("Country_image/", possible_noc, ".png"))
              flag=1
              break
            }
          }
          if(flag==0){
            iconUrl <- c(iconUrl, "Country_image/default.png")
          }
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
        "<th style='text-align: center; color: gold;'>🥇Gold</th>",
        "<th style='text-align: center; color: seashell;'>🥈Silver</th>",
        "<th style='text-align: center; color: brown;'>🥉Bronze</th>",
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
        <img src='","https://raw.githubusercontent.com/MKE0108/Big_Data_Final_Project/main/main_final_project/Sports_image/",gsub(" ","_",tolower(Sport1)), "_pictogram.png' onerror='this.onerror=null;this.src=\"https://raw.githubusercontent.com/MKE0108/Big_Data_Final_Project/main/main_final_project/Sports_image/default.png\"' width='30px'>
        <p style='font-size: 10px; margin-top: 3px ;margin-bottom:3px;font-weight: bold;'>",Sport1,"</p>
      </td>",
      "<td style='text-align: center;font-weight: bold;'>",Gold1,"</td>",
      "<td style='text-align: center;font-weight: bold;'>",Silver1,"</td>",
      "<td style='text-align: center;font-weight: bold;'>",Bronze1,"</td>",
      "</tr>",
      "<tr>",
      "<td style='text-align: center;'>
         <img src='","https://raw.githubusercontent.com/MKE0108/Big_Data_Final_Project/main/main_final_project/Sports_image/",gsub(" ","_",tolower(Sport2)), "_pictogram.png' onerror='this.onerror=null;this.src=\"https://raw.githubusercontent.com/MKE0108/Big_Data_Final_Project/main/main_final_project/Sports_image/default.png\"' width='30px'>
         <p style='font-size: 10px; margin-top: 3px ;margin-bottom:3px;font-weight: bold;'>",Sport2,"</p>",
      "</td>",
      "<td style='text-align: center;font-weight: bold;'>",Gold2,"</td>",
      "<td style='text-align: center;font-weight: bold;'>",Silver2,"</td>",
      "<td style='text-align: center;font-weight: bold;'>",Bronze2,"</td>",
      "</tr>",
      "<tr>",
      "<td style='text-align: center;'>
         <img src='","https://raw.githubusercontent.com/MKE0108/Big_Data_Final_Project/main/main_final_project/Sports_image/",gsub(" ","_",tolower(Sport3)), "_pictogram.png' onerror='this.onerror=null;this.src=\"https://raw.githubusercontent.com/MKE0108/Big_Data_Final_Project/main/main_final_project/Sports_image/default.png\"' width='30px'>
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
      setView(lng = 0, lat = 0, zoom = 2)
    m
  })
### sport rank ####
output$global_rank_table <- renderDataTable({
    req(input$rank_Sports)  # 確保有選擇運動
    Sport=input$rank_Sports
    D=SPORTS_RANK[[Sport]]

    for (i in 1:nrow(D)){
      
      n=as.character(D[i,1])
      index=which(ALL_NOC==n)
      if(length(index)==0){
        D[i,1]=""
        next
      }
      D[i,1]=NOC_HTML[index]
 
    }
    colnames(D)[1:4]=c("Region","🥇Gold","🥈Silver","🥉Bronze")
    DT::datatable(D,escape = FALSE)
})

output$ex_country_map<-renderLeaflet({
    req(input$explore_Country)  # 確保有選擇國家
    target_noc=input$explore_Country
    index=which(NOC_summary_with_map$NOC==target_noc)
    if(length(index)==0){
        leaflet() %>%
          addTiles()  # 添加基本地圖圖層
    }else{
      unique_data=NOC_summary_with_map[index,]

      iconUrl=paste0("Country_image/", unique_data$NOC, ".png")
      for (i in 1:length(iconUrl)){
        if (!file.exists(iconUrl[i])){
          iconUrl[i] <- "Country_image/default.png"
        }
      }
      target_lng=unique_data$long
      target_lat=unique_data$lat

      
      UserIcon <- icons(
        iconUrl = iconUrl,
        iconWidth = 22, iconHeight = 11
      )


      m <- leaflet(data = unique_data) %>%
        addTiles() %>%
        addMarkers(~long, ~lat, icon = UserIcon, 
                  
        label = ~region,
        labelOptions = labelOptions(noHide = FALSE, direction = 'auto', style = list('color' = 'black', 'font-size' = '16px')))%>%
        setView(lng = target_lng, lat = target_lat, zoom = 3)
      m
    }
  
})
output$country_rank_table <- renderDataTable({
    req(input$explore_Country)  # 確保有選擇國家
    target_noc=input$explore_Country
    region_name=noc[which(noc$NOC==target_noc),2]

    #建立一個空的dataframe，放置運動的排名  
    df=data.frame(global_rank=numeric(),Sport=character(),Sport_html=character(),Gold=numeric(),Silver=numeric(),Bronze=numeric())
    
    for(s in ALLSPORT){
      D=SPORTS_RANK[[s]]
      #找到該國家的排名
     

      index <- which(D$NOC==target_noc)
      if (length(index) == 1) {
        df=rbind(df,data.frame(global_rank=index,Sport=s,Sport_html=SPORT_HTML[which(ALLSPORT==s)]
                  ,Gold=D[index,2],Silver=D[index,3],Bronze=D[index,4]))
      }
    }
    if(nrow(df)==0){
      html_df=df[,c(1,3,4,5,6)]
      #改column名稱
      colnames(html_df)=c("🏆rank","Sport","🥇Gold","🥈Silver","🥉Bronze")
      DT::datatable(html_df,escape = FALSE,rownames= FALSE)
    }else{
      all=df[which(df$Sport=="All"),]
      not_all=df[which(df$Sport!="All"),]
      #sort by Golbal_rank
      not_all=not_all[order(not_all$global_rank),]
      df=rbind(all,not_all)
      html_df=df[,c(1,3,4,5,6)]
      #改column名稱
      colnames(html_df)=c("🏆rank","Sport","🥇Gold","🥈Silver","🥉Bronze")
      DT::datatable(html_df,escape = FALSE,rownames= FALSE)
    }

})
# output$rank_dynamic_html <- renderUI({
#     req(input$rank_Sports)  # 確保有選擇運動
#     Sport=input$rank_Sports
#     if(Sport=="All"){
#       D=data[!is.na(data),]
#     }else{
#       D=data[which(data$Sport==Sport),]
#     }
#     D=D%>%group_by(Year,NOC,Sport,Event,Medal)%>%filter(!is.na(Medal))%>%summarise(n=1)%>%group_by(NOC)%>%summarise(Gold=sum(Medal=="Gold"),Silver=sum(Medal=="Silver"),Bronze=sum(Medal=="Bronze"),Weight=sum(Medal=="Bronze")+1.5*sum(Medal=="Silver")+2*sum(Medal=="Gold"))%>%arrange(desc(Weight))

#     HTML(
#       paste0(
#         "<table class='content-table-full-page'>",
#         "<thead>",
#         "<tr>",
#         "<th style='text-align: center;'>NOC</th>",
#         "<th style='text-align: center; color: gold;'>🥇Gold</th>",
#         "<th style='text-align: center; color: seashell;'>🥈Silver</th>",
#         "<th style='text-align: center; color: brown;'>🥉Bronze</th>",
#         "</tr>",
#         "</thead>",
#         "<tbody>",
#         paste(apply(D, 1, function(row) {
#             paste0(
#             "<tr>",
#             "<td style='text-align: center;'>
#               <img src='","https://raw.githubusercontent.com/MKE0108/Big_Data_Final_Project/main/main_final_project/Country_image/",row[1], ".png' onerror='this.onerror=null;this.src=\"https://raw.githubusercontent.com/MKE0108/Big_Data_Final_Project/main/main_final_project/Country_image/default.png\"' style='margin-top: 10px;' width='50px'>
#               <p style='font-size: 15px; margin-top: 3px ;margin-bottom:3px;font-weight: bold;'>",noc[which(noc$NOC==row[1]),2],"</p>
#             </td>",
#             "<td style='text-align: center;'>", row[2], "</td>",
#             "<td style='text-align: center;'>", row[3], "</td>",
#             "<td style='text-align: center;'>", row[4], "</td>",
#             "</tr>"
#             )
#         }), collapse = ""),
#         "</tbody>",
#         "</table>"
      
#       )
#     )

#   })
 


### history ####

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
      layout(title = paste("Olympic Women Medals Per Edition -", input$history_season),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Number of Medals"),
             bargap = 0.2)  # 設定條形之間的間隔
    p
  })
  
  output$history_height_weight <- renderPlotly({

    tmp <- filteredData()$notNullMedalsFiltered  
    
    # 创建事件到颜色的映射
    events <- unique(tmp$Event)
    colors <- RColorBrewer::brewer.pal(min(length(events), 9), "Paired")  
    if (length(events) > 9) {
      colors <- colorRampPalette(colors)(length(events)) 
    }
    color_map <- setNames(colors, events)  


    p <- plot_ly(tmp, x = ~Height, y = ~Weight, customdata = ~Sport_url[Sport],type = 'scatter', mode = 'Paired',
                 marker = list(color = ~color_map[Event], opacity = 0.9), 
                text = ~paste("Event:", Event, "<br>Height:", Height,"cm", "<br>Weight:", Weight,"kg"),
                 hoverinfo = 'text') 

    p=p%>% htmlwidgets::onRender(readLines("hover_tooltip.js"))%>%
                 layout(title = paste("Height vs Weight of Olympic Medalists -", input$history_season),
                  xaxis = list(title = "Height"),
                  yaxis = list(title = "Weight"))
    
    p
  })
  
  
  output$history_gold_age <- renderPlotly({
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
                 y = ~Count, 
                #  autobinx = FALSE,
                #  xbins = list(start = min(tmp$Age) - 0.5, end = max(tmp$Age) + 0.5, size = 1),
                 marker = list(color = ~Count, colorscale = colorscale, showscale = TRUE)) %>%
      layout(title = paste("Age Distribution of Olympic Gold Medalists", input$history_season),
             xaxis = list(title = "Age"),
             yaxis = list(title = "Frequency of Gold Medals"),
             bargap = 0.1)
    p
  })
})


