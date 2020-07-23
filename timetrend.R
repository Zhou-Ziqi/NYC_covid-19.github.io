library(shiny)
library(flexdashboard)
library(tidyverse)
library(readxl)
library(viridis)
library(leaflet)
library(plotly)
library(shinyWidgets)
library(lubridate)


finalMaydata <- read_csv("data/finalMaydata.csv") %>% 
  select(zipcode,day,neighborhood_name,borough_group, positive,covid_case_rate, covid_death_count, covid_death_rate,newcases) %>% 
  mutate(day = as.Date(day,format = "%m/%d/%y"))
  
final_Junedata <- read_csv("data/final_Junedata.csv") %>% 
  dplyr::rename(newcases = newcases_june) %>% 
  select(zipcode,day,neighborhood_name,borough_group, positive,covid_case_rate, covid_death_count, covid_death_rate,newcases)

Aprildata_with_nebhod <- read_csv("data/Aprildata_with_nebhod.csv")

data <- rbind(finalMaydata,final_Junedata)
Week <- unique(as.Date(cut(data$day, "week")) + 6)



weeklydf <- data %>% 
  mutate(zipcode = factor(zipcode)) %>% 
  filter(day %in% Week)


data$week <- as.Date(cut(data$day, "week")) + 6
weeklynew <- aggregate(data$newcases, by=list(week=data$week, zipcode = data$zipcode), FUN=sum)
weeklynew <- dplyr::rename(weeklynew, new_cases = x)

zipcode = data %>% distinct(zipcode) %>% pull()


shinyApp(
  ui = fluidPage(
    h1("Time trend"),
    hr(),
    fluidRow(
      column(width = 4, offset = 1, selectInput("character",
                                                "Choose a characteristics",
                                                c("please select..." = "plz",
                                                  "Cumulative Cases Count" = "pocase", 
                                                  "Death Count" = "death", 
                                                  "Positive Cases Rate" = "porate", 
                                                  "Death Rate" = "derate",
                                                  "New cases" = "newcase"
                                                  ),
                                                selected = NULL)),
      column(width = 5, "this part will have some instructions"),
    ),
    hr(),
    
    #### Cumulative Cases Count
    conditionalPanel(
      condition = "input.character == 'pocase'",
      h2("Cumulative Cases Count"),
      fluidRow(
        column(width = 12,
               sidebarPanel(pickerInput("zip1", 
                                        label = "Choose zipcodes", 
                                        choices =zipcode,
                                        multiple = TRUE,
                                        selected = zipcode[1:5],
                                        options = list(`actions-box` = TRUE))),
               mainPanel(plotlyOutput("pocase", width="100%",height="500px"))))
    ),
    
    #### Death Count
    conditionalPanel(
      condition = "input.character == 'death'",
      h2("Death Count"),
      fluidRow(
        column(width = 12,
               sidebarPanel(pickerInput("zip2", 
                                        label = "Choose zipcodes", 
                                        choices =zipcode, 
                                        multiple = TRUE,
                                        selected = zipcode[1:5],
                                        options = list(`actions-box` = TRUE))),
               mainPanel(plotlyOutput("death", width="100%",height="500px"))))
    ),
    
    #### Positive Cases Rate
    conditionalPanel(
      condition = "input.character == 'porate'",
      h2("Positive Cases Rate"),
      fluidRow(
        column(width = 12,
               sidebarPanel(pickerInput("zip3", 
                                        label = "Choose zipcodes", 
                                        choices =zipcode, 
                                        multiple = TRUE,
                                        selected = zipcode[1:5],
                                        options = list(`actions-box` = TRUE))),
               mainPanel(plotlyOutput("porate", width="100%",height="500px"))))
    ),
    
    #### Death Rate
    conditionalPanel(
      condition = "input.character == 'derate'",
      h2("Death Rate"),
      fluidRow(
        column(width = 12,
               sidebarPanel(pickerInput("zip4", 
                                        label = "Choose zipcodes", 
                                        choices =zipcode,
                                        multiple = TRUE,
                                        selected = zipcode[1:5],
                                        options = list(`actions-box` = TRUE))),
               mainPanel(plotlyOutput("derate", width="100%",height="500px"))))
    ),
    
    #### New cases
    conditionalPanel(
      condition = "input.character == 'newcase'",
      h2("New cases"),
      fluidRow(
        column(width = 12,
               sidebarPanel(pickerInput("zip5", 
                                        label = "Choose zipcodes", 
                                        choices =zipcode,
                                        multiple = TRUE,
                                        selected = zipcode[1:5],
                                        options = list(`actions-box` = TRUE))),
               mainPanel(plotlyOutput("newcases", width="100%",height="500px"))))
    )
    
    

    
    
  ),
  
  server = function(input, output){

    output$pocase <- renderPlotly({
      
      weeklydf %>% 
        filter(zipcode %in% input$zip1) %>%
        plot_ly(x = ~day,
                y = ~positive,
                type="scatter",
                mode = 'lines+markers',
                color= ~zipcode) %>% 
        layout(legend=list(title=list(text='<b> Zipcode </b>'), orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
               xaxis = list(
                 type = "date",
                 range=c('2020-05-18', '2020-07-05')))
      

      
    })
    
    output$death <- renderPlotly({
      weeklydf %>% 
        filter(zipcode %in% input$zip2) %>%
        plot_ly(x = ~day,
                y = ~covid_death_count,
                type="scatter",
                mode = 'lines+markers',
                color= ~zipcode) %>% 
        layout(legend=list(title=list(text='<b> Zipcode </b>'), orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
               xaxis = list(
                 type = "date",
                 range=c('2020-05-18', '2020-07-05')))
      
    }) 
    
    output$porate <- renderPlotly({
      weeklydf %>% 
        filter(zipcode %in% input$zip3) %>%
        plot_ly(x = ~day,
                y = ~covid_case_rate,
                type="scatter",
                mode = 'lines+markers',
                color= ~zipcode) %>% 
        layout(legend=list(title=list(text='<b> Zipcode </b>'), orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
               xaxis = list(
                 type = "date",
                 range=c('2020-05-18', '2020-07-05')))
    }) 
    
    output$derate <- renderPlotly({
      weeklydf %>% 
        filter(zipcode %in% input$zip4) %>%
        plot_ly(x = ~day,
                y = ~covid_death_rate,
                type="scatter",
                mode = 'lines+markers',
                color= ~zipcode) %>% 
        layout(legend=list(title=list(text='<b> Zipcode </b>'), orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
               xaxis = list(
                 type = "date",
                 range=c('2020-05-18', '2020-07-05')))
    })
    
    output$newcases <- renderPlotly({
      weeklynew %>% 
        mutate(zipcode = factor(zipcode)) %>% 
        filter(zipcode %in% input$zip5) %>%
        plot_ly(x = ~week,
                y = ~new_cases,
                type="scatter",
                mode = 'lines+markers',
                color= ~zipcode) %>% 
        layout(legend=list(title=list(text='<b> Zipcode </b>'), orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
               xaxis = list(
                 type = "date",
                 range=c('2020-05-18', '2020-07-10')))
    })
    
    
    
  }
)














