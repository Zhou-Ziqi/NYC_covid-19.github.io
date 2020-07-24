library(shiny)
library(flexdashboard)
library(shinyWidgets)

shinyApp(ui = navbarPage(
  title = div(img(src='logo.png',style="margin-top: -14px; padding-right:10px;padding-bottom:10px", height = 60)),
  windowTitle = "NYC covid-19 dashboard",
  id = 'menus',
  tabPanel('Home',
           shinyjs::useShinyjs()),
  tabPanel("Tracker"),
  tabPanel("Distribution"),
  tabPanel("Time Trend"),
  tabPanel("Demographics"),
  tabPanel("Comorbidity"),
  tabPanel("About")
),

server = function(input, output, session) {
  shinyjs::addClass(id = "menus", class = "navbar-right")

}

)