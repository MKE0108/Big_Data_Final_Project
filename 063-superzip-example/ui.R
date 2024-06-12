library(leaflet)
library(shiny)
library(plotly)
# Choices for drop-downs
# vars <- c(
#   "Is SuperZIP?" = "superzip",
#   "Centile score" = "centile",
#   "College education" = "college",
#   "Median income" = "income",
#   "Population" = "adultpop"
# )


navbarPage("Superzip", id="nav",

  # tabPanel("Interactive map",
  #   div(class="outer",

  #     tags$head(
  #       # Include our custom CSS
  #       includeCSS("styles.css"),
  #       includeScript("gomap.js")
  #     ),

  #     # If not using custom CSS, set height of leafletOutput to a number instead of percent
  #     leafletOutput("map", width="100%", height="100%"),

  #     # Shiny versions prior to 0.11 should use class = "modal" instead.
  #     absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
  #       draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
  #       width = 330, height = "auto",

  #       h2("ZIP explorer"),

  #       selectInput("color", "Color", vars),
  #       selectInput("size", "Size", vars, selected = "adultpop"),
  #       conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
  #         # Only prompt for threshold when coloring or sizing by superzip
  #         numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
  #       ),

  #       plotOutput("histCentile", height = 200),
  #       plotOutput("scatterCollegeIncome", height = 250)
  #     ),

  #     tags$div(id="cite",
  #       'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
  #     )
  #   )
  # ),


  tags$head(tags$style("
                      .jhr{
                      display: inline;
                      vertical-align: middle;
                      padding-left: 10px;
            }")),
  ### A. 獎牌統計
  tabPanel("獎牌統計",
      fluidRow(
        column(3,
          selectInput("Season", "Season", c("All seasons"="", A.Season_sel), multiple=TRUE,selected="Summer")
        ),
        column(3,
          # selectInput("NOC", "NOC", c("All NOCs"="", A.NOC_sel), multiple=FALSE,selected="TPE")
          pickerInput(
            inputId = "NOC", 
            label = "NOC", 
            choices =  (A.NOC_sel),
            choicesOpt = list(
              content = 
               A.NOC_sel_path
              )
            ,
            selected = "TPE"
          )


        ),
        column(3,
          selectInput("Sport", "Sport", c("All sports"="", A.Sport_sel), multiple=TRUE,selected="Baseball")
        )
      ),
      fluidRow(
        column(3,
          sliderInput("Year_range", "Year range", min = min(A.Begin_Year_sel), max = max(A.Begin_Year_sel), value = c(1896, 2016))
        ),
        column(3,
          selectInput("Medal", "Medal", c("All medals"="", A.Medal_sel), multiple=TRUE,selected=c("Gold","Silver","Bronze"))
        )
      ),
      hr(),
      actionButton("A.update", "提交"),
      tags$style(HTML("
        #A.text {
          font-size: 24px;
        }
      ")),
      textOutput("A.text"),
      fluidRow(
        column(6, plotlyOutput("A1.plot")),
        column(6, plotlyOutput("A2.plot"))
      )

      
      


  )
  
  # tabPanel("Data explorer",
  #   fluidRow(
  #     column(3,
  #       selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
  #     ),
  #     column(3,
  #       conditionalPanel("input.states",
  #         selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
  #       )
  #     ),
  #     column(3,
  #       conditionalPanel("input.states",
  #         selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
  #       )
  #     )
  #   ),
  #   fluidRow(
  #     column(1,
  #       numericInput("minScore", "Min score", min=0, max=100, value=0)
  #     ),
  #     column(1,
  #       numericInput("maxScore", "Max score", min=0, max=100, value=100)
  #     )
  #   ),
  #   hr(),
  #   DT::dataTableOutput("ziptable")
  # ),

  # conditionalPanel("false", icon("crosshair"))
)
