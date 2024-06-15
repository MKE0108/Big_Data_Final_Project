library(shiny)
source("global.R")
shinyUI(fluidPage(
  titlePanel("Analyzing the Evolution of Olympic Competitors"),
  sidebarLayout(
    sidebarPanel(
      # Radio buttons for selecting 'Summer' or 'Winter' Olympics
      radioButtons("season", "Choose Season:",
                   choices = list("Summer Olympics" = "Summer",
                                  "Winter Olympics" = "Winter"),
                   selected = "Summer")
    ),
    mainPanel(
      plotlyOutput("plot"),
      plotlyOutput("height_weight"),
      plotlyOutput("gold_age")
    )
  )
))
