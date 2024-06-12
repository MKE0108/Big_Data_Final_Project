library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
# set.seed(100)
# zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# # By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# # will be drawn last and thus be easier to see
# zipdata <- zipdata[order(zipdata$centile),]

function(input, output, session) {
  ## 獎牌統計 ###########################################

  observeEvent(input$A.update, {
      # NOC="USA"
      # Season="Summer"
      # Sport=c("Shooting")
      # Begin_Year=1800
      # End_Year=2016
      # Medals=c("Silver","Gold","Bronze")
      NOC = input$NOC
      Season = input$Season
      Sport = input$Sport
      Begin_Year = input$Year_range[1]
      End_Year = input$Year_range[2]
      if(End_Year<Begin_Year){
          End_Year=Begin_Year
      }
      Medals = input$Medal
      # filter data
      Target_INFO=dataBase[which(dataBase$NOC==NOC),]
      Target_INFO=Target_INFO[which(Target_INFO$Season==Season & Target_INFO$Sport==Sport),]
      #統計相同的Event且相同的Medal的合併和相同年分的總數
      Result=Target_INFO%>%filter(Medal %in% Medals)%>%group_by(Year,Event,Medal) %>% summarise(Count = 1)
      Result <- Result[which(Result$Year>=Begin_Year & Result$Year<=End_Year),]
      #統計每年的Gold和Silver所選的獎牌數
      Result=Result%>% filter(Medal %in% Medals)%>%group_by(Year,Medal) %>% summarise(Count = sum(Count))
      #plot
      colors <- c("Gold" = "#FFD700", "Silver" = "#C0C0C0", "Bronze" = "#CD7F32")
      print(length(Result))
      if(length(Sport)<=4){
          Sport_text=paste(Sport,collapse=",")
      }else{
          Sport_text="Multiple sports"
      }
      if(nrow(Result)==0){
          output$A.text <- renderText(paste("No medals in", NOC ,"(",Sport_text,")"))
          p1 <- plot_ly()
          p2 <- plot_ly()
      }else{
          output$A.text <- renderText("")
          first=TRUE
          for (medal in Medals){
              tmpResult=Result[which(Result$Medal==medal),]
              if(first){
                  first=FALSE
                  p1 <- plot_ly(name = medal,x = tmpResult$Year, y = tmpResult$Count, type = 'scatter',mode = 'lines+markers',line=list(color=colors[medal]),marker=list(color=colors[medal]))                  
              }
              else{
                  p1 <- add_trace(p1, x = tmpResult$Year, y = tmpResult$Count, type = 'scatter',mode = 'lines+markers',name = medal,line=list(color=colors[medal]),marker=list(color=colors[medal]))
                  # ,line=list(color=colors[medal]),marker=list(color=colors[medal])
                  # )
              }
          }
          p1=layout(p1,title = paste("\nCount of Medals", "in", NOC ,"(",Sport_text,")"),
                  xaxis = list(title = 'Year',
                        tickvals=sort(unique(Result$Year))
                  ),
                  yaxis = list(
                      title = 'Count',
                      tickvals = seq(0, max(max(Result$Count)) + 1, by = 1)
                      
                  )
          )
          p2 <- plot_ly(Result, x = ~Year, y = ~Count, color = ~Medal, colors = colors, type = 'bar') %>%
              layout(title = paste("\nCount of Medals", "in", NOC ,"(",Sport_text,")"),
                  xaxis = list(title = 'Year',
                        tickvals=sort(unique(Result$Year))
                  ),
                  yaxis = list(
                      title = 'Count',
                      tickvals = seq(0, max(max(Result$Count)) + 1, by = 1)
                      
                  )
              )
          
      }
      output$A1.plot <- renderPlotly(p1)
      output$A2.plot <- renderPlotly(p2)
      

  })

  ## Interactive Map ###########################################

  # Create the map
  # output$map <- renderLeaflet({
  #   leaflet() %>%
  #     addTiles() %>%
  #     setView(lng = -93.85, lat = 37.45, zoom = 4)
  # })

  # # A reactive expression that returns the set of zips that are
  # # in bounds right now
  # zipsInBounds <- reactive({
  #   if (is.null(input$map_bounds))
  #     return(zipdata[FALSE,])
  #   bounds <- input$map_bounds
  #   latRng <- range(bounds$north, bounds$south)
  #   lngRng <- range(bounds$east, bounds$west)

  #   subset(zipdata,
  #     latitude >= latRng[1] & latitude <= latRng[2] &
  #       longitude >= lngRng[1] & longitude <= lngRng[2])
  # })

  # # Precalculate the breaks we'll need for the two histograms
  # centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks

  # output$histCentile <- renderPlot({
  #   # If no zipcodes are in view, don't plot
  #   if (nrow(zipsInBounds()) == 0)
  #     return(NULL)

  #   hist(zipsInBounds()$centile,
  #     breaks = centileBreaks,
  #     main = "SuperZIP score (visible zips)",
  #     xlab = "Percentile",
  #     xlim = range(allzips$centile),
  #     col = '#00DD00',
  #     border = 'white')
  # })

  # output$scatterCollegeIncome <- renderPlot({
  #   # If no zipcodes are in view, don't plot
  #   if (nrow(zipsInBounds()) == 0)
  #     return(NULL)

  #   print(xyplot(income ~ college, data = zipsInBounds(), xlim = range(allzips$college), ylim = range(allzips$income)))
  # })

  # # This observer is responsible for maintaining the circles and legend,
  # # according to the variables the user has chosen to map to color and size.
  # observe({
  #   colorBy <- input$color
  #   sizeBy <- input$size

  #   if (colorBy == "superzip") {
  #     # Color and palette are treated specially in the "superzip" case, because
  #     # the values are categorical instead of continuous.
  #     colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
  #     pal <- colorFactor("viridis", colorData)
  #   } else {
  #     colorData <- zipdata[[colorBy]]
  #     pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
  #   }

  #   if (sizeBy == "superzip") {
  #     # Radius is treated specially in the "superzip" case.
  #     radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
  #   } else {
  #     radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
  #   }

  #   leafletProxy("map", data = zipdata) %>%
  #     clearShapes() %>%
  #     addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
  #       stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
  #     addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
  #       layerId="colorLegend")
  # })

  # # Show a popup at the given location
  # showZipcodePopup <- function(zipcode, lat, lng) {
  #   selectedZip <- allzips[allzips$zipcode == zipcode,]
  #   content <- as.character(tagList(
  #     tags$h4("Score:", as.integer(selectedZip$centile)),
  #     tags$strong(HTML(sprintf("%s, %s %s",
  #       selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
  #     ))), tags$br(),
  #     sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
  #     sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
  #     sprintf("Adult population: %s", selectedZip$adultpop)
  #   ))
  #   leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  # }

  # # When map is clicked, show a popup with city info
  # observe({
  #   leafletProxy("map") %>% clearPopups()
  #   event <- input$map_shape_click
  #   if (is.null(event))
  #     return()

  #   isolate({
  #     showZipcodePopup(event$id, event$lat, event$lng)
  #   })
  # })


  ## Data Explorer ###########################################

  # observe({
  #   cities <- if (is.null(input$states)) character(0) else {
  #     filter(cleantable, State %in% input$states) %>%
  #       `$`('City') %>%
  #       unique() %>%
  #       sort()
  #   }
  #   stillSelected <- isolate(input$cities[input$cities %in% cities])
  #   updateSelectizeInput(session, "cities", choices = cities,
  #     selected = stillSelected, server = TRUE)
  # })

  # observe({
  #   zipcodes <- if (is.null(input$states)) character(0) else {
  #     cleantable %>%
  #       filter(State %in% input$states,
  #         is.null(input$cities) | City %in% input$cities) %>%
  #       `$`('Zipcode') %>%
  #       unique() %>%
  #       sort()
  #   }
  #   stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
  #   updateSelectizeInput(session, "zipcodes", choices = zipcodes,
  #     selected = stillSelected, server = TRUE)
  # })

  # observe({
  #   if (is.null(input$goto))
  #     return()
  #   isolate({
  #     map <- leafletProxy("map")
  #     map %>% clearPopups()
  #     dist <- 0.5
  #     zip <- input$goto$zip
  #     lat <- input$goto$lat
  #     lng <- input$goto$lng
  #     showZipcodePopup(zip, lat, lng)
  #     map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
  #   })
  # })

  # output$ziptable <- DT::renderDataTable({
  #   df <- cleantable %>%
  #     filter(
  #       Score >= input$minScore,
  #       Score <= input$maxScore,
  #       is.null(input$states) | State %in% input$states,
  #       is.null(input$cities) | City %in% input$cities,
  #       is.null(input$zipcodes) | Zipcode %in% input$zipcodes
  #     ) %>%
  #     mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
  #   action <- DT::dataTableAjax(session, df, outputId = "ziptable")

  #   DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  # })
}
