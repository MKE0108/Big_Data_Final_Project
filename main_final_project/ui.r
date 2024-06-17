library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(maps)
library(tibble)
library(leaflet)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
source("global.r")
shinyUI(
  fluidPage(
    shinyWidgets::useShinydashboard(),
    tags$head(
       tags$script(src = "http://d3js.org/d3.v3.min.js"),
        tags$style(HTML("
            .content-table-full-page {
              border-collapse: collapse;
              margin: 25px 0;
              font-size: 0.9em;
              min-width: 600px;
              border-radius: 5px 5px 0 0;
              overflow: hidden;
              box-shadow: 0 0 20px rgba(0, 0, 0, 0.15);
          }

          .content-table-full-page thead tr {
              background-color: #009879;
              color: #ffffff;
              text-align: left;
              font-weight: bold;
          }

          .content-table-full-page th {
            padding: 13px 14px;
            font-size: 25px;
          }

          .content-table-full-page td {
            padding: 6px 7px;
            font-size: 20px;
          }

          .content-table-full-page tbody tr {
              border-bottom: 1px solid #dddddd;
          }

          .content-table-full-page tbody tr:nth-of-type(even) {
              background-color: #f3f3f3;
          }

          .content-table-full-page tbody tr:last-of-type {
              border-bottom: 2px solid #009879;
          }
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
        "))
    ),
    #下拉選單圖片＋文字組合時，文字的template
    tags$head(tags$style("
              .jhr{
              display: inline;
              vertical-align: middle;
              padding-left: 10px;
    }")),
    #導覽頁padding
    div(style="padding: 1px 0px; width: '100%'",
        titlePanel(
                title="", windowTitle="My Window Title"
        )
    ),
    navbarPage(
      theme = shinytheme("paper"),
      title=div(img(src="https://static01.nyt.com/images/2012/07/15/magazine/15wmt/15wmt-jumbo.jpg", height = "25", style = "margin-right: 5px;"), "Olympic"),
      tabPanel("各國參賽人數",
        source("num_Participants.R")
      ),

      tabPanel("探索國家",
        source("ex_country.R")
      ),

      tabPanel("歷史回顧",
        source("history.R")
      ),

      tabPanel("身高體重性別",
        source("h_w_s_ui.R")
      )
  )
)
)

