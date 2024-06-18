library(shiny)
library(shinythemes)
shinyUI(
  fluidPage(theme = shinytheme("united"),
    navbarPage("Navbar",
      tabPanel("Tab 1", 
        h1("Tab 1 Content") 
      ), 
      tabPanel("Tab 2", 
        h1("Tab 2 Content") 
      ),
      navbarMenu("More",
        tabPanel("Tab 3",
          h1("Tab 3 Content")
        ),
        tabPanel("Tab 4",
          h1("Tab 4 Content")
        )
      )
    )
  )
)