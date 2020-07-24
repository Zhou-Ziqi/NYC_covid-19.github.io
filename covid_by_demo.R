# covid_by_demo
library(shiny)
library(flexdashboard)
library(tidyverse)
library(readxl)
library(plotly)
library(shinyWidgets)
library(ggthemes)
library(RColorBrewer)



# input data

byage = read_csv("./distribution_of_covid-19/data/demoage_data.csv")

byrace = read_csv("./distribution_of_covid-19/data/BYRACE_demoage_data.csv")

bysex = read_csv("./distribution_of_covid-19/data/demoage_data_sex.csv")

# Define UI for application that draws plot

outcome_age = byage %>% distinct(outcome) %>% pull()
outcome_race = byrace %>% distinct(outcome) %>% pull()
outcome_sex = bysex %>% distinct(outcome) %>% pull()

group_age = byage %>% distinct(group) %>% pull()

# ui
ui =  fluidPage(
  titlePanel("COVID-19 by Demographics in NYC"),
  
    sidebarPanel(width = 3,
      helpText("Distribution of COVID-19 in NYC."),
      
      dateInput(
        inputId = "date_choice", 
        label = "Select Date",
        format = "yyyy-mm-dd",
        value = "2020-05-01",
        language = "en"
      ),
      
      
      radioButtons(inputId = "character",
                   label =  "Choose a characteristics: ",   
                   c("Race" = "race",
                     "Sex" = "sex",
                     "Age" = "age")
      )),
      
      
      
      mainPanel(
      conditionalPanel(
        condition = "input.character == 'age'",
        
        fluidRow(column(width = 9,selectInput("outcome_age", 
                                              label = "Choose an outcome", 
                                              choices =outcome_age, 
                                              selected = "CASE_COUNT")),
          
          column(width = 9, 
                        h4("some words to describe the charts")),
                 
                 ),
        br(),
 
        fluidRow(
          column(width = 12, plotlyOutput(outputId = "piechart_age_MN")),
          #column(width = 4, plotlyOutput(outputId = "piechart_age_BX")),
          #column(width = 4, plotlyOutput(outputId = "piechart_age_BK")),
          h4(),
          #column(width = 4, plotlyOutput(outputId = "piechart_age_SI")),
          #column(width = 4, plotlyOutput(outputId = "piechart_age_QN")),
          column(width = 12, plotlyOutput(outputId = "Barchart_age")),
          column(width = 9, selectInput("group", 
                                        label = "Choose an age group", 
                                        choices =group_age))),
          column(width = 12, plotlyOutput(outputId = "timetrend_age"))),
          
       
       
      
    
    conditionalPanel(
      condition = "input.character == 'sex'",
      
      fluidRow(column(width = 9,selectInput("outcome_sex", 
                                            label = "Choose an outcome", 
                                            choices =outcome_sex, 
                                            selected = "CASE_COUNT")),
               
               column(width = 9, 
                      h4("some words to describe the charts")),
               
      ),
      br(),

      fluidRow(
        column(width = 3, plotlyOutput(outputId = "piechart_sex_MN")),
        column(width = 3, plotlyOutput(outputId = "piechart_sex_BX")),
        column(width = 3, plotlyOutput(outputId = "piechart_sex_BK")),
        h4(),
        column(width = 3, plotlyOutput(outputId = "piechart_sex_SI")),
        column(width = 3, plotlyOutput(outputId = "piechart_sex_QN")),
        column(width = 12, plotlyOutput(outputId = "Barchart_sex")),
        column(width = 12, plotlyOutput(outputId = "timetrend_sex")))
      
      
      
    ),
  conditionalPanel(
    condition = "input.character == 'race'",
    
    fluidRow(column(width = 9,selectInput("outcome_race", 
                                          label = "Choose an outcome", 
                                          choices =outcome_race, 
                                          selected = "CASE_COUNT")),
             
             column(width = 9, 
                    h4("some words to describe the charts")),
             ),
    br(),

    fluidRow(
      column(width = 3, plotlyOutput(outputId = "piechart_race_MN")),
      column(width = 3, plotlyOutput(outputId = "piechart_race_BX")),
      column(width = 3, plotlyOutput(outputId = "piechart_race_BK")),
      h4(),
      column(width = 3, plotlyOutput(outputId = "piechart_race_SI")),
      column(width = 3, plotlyOutput(outputId = "piechart_race_QN")),
      column(width = 12, plotlyOutput(outputId = "Barchart_race")),
      column(width = 12, plotlyOutput(outputId = "timetrend_race")))
    
    
    
  ))
    
    
    )


# server

server = function(input, output) {

  output$piechart_age_MN = renderPlotly({
    data_to_plot_mn = byage %>% 
      filter(outcome == as.character(input$outcome_age) &
               boro == "MN" &
               group != "Boroughwide" &
               day == input$date_choice)
    data_to_plot_qn = byage %>% 
      filter(outcome == as.character(input$outcome_age) &
               boro == "QN" &
               group != "Boroughwide" &
               day == input$date_choice)
    
    fig = plotly()
    fig <- fig %>% add_pie(data = data_to_plot_mn, labels = paste0("Age Group: ", data_to_plot$group), 
                           values = ~count,
                           name = "Proportion")
    fig <- fig %>% add_pie(data = data_to_plot_qn, labels = paste0("Age Group: ", data_to_plot$group), 
                           values = ~count,
                           name = "Proportion")
    fig <- fig %>% layout(title = "Pie Charts with Subplots", showlegend = F,
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig1 = plot_ly(labels = paste0("Age Group: ", data_to_plot$group),
            values = data_to_plot$count,
            type = "pie",
            marker = list(colors = brewer.pal(7,"Blues")),
            sort = FALSE,
            opacity=0.8) %>% 
      layout(legend=list(title=list(text='<b> Age Group </b>'),orientation = 'h', 
                         xanchor = "center", 
                         x = 0.5, y = -0.5),
             title = "Manhattan")
    
    
    
    fig2 = plot_ly(labels = paste0("Age Group: ", data_to_plot$group),
            values = data_to_plot$count,
            type = "pie",
            marker = list(colors = brewer.pal(7,"Blues")),
            sort = FALSE,
            opacity=0.8) %>% 
      layout(legend=list(title=list(text='<b> Age Group </b>'),orientation = 'h', 
                         xanchor = "center", 
                         x = 0.5, y = -0.5),
             title = "Queens")
    fig
   
    })
  
    output$piechart_age_BK = renderPlotly({
    data_to_plot = byage %>% 
      filter(outcome == as.character(input$outcome_age) &
               boro == "BK" &
               group != "Boroughwide" &
               day == input$date_choice)
    
    plot_ly(labels = paste0("Age Group: ", data_to_plot$group),
            values = data_to_plot$count,
            type = "pie",
            marker = list(colors = brewer.pal(7,"Blues")),
            sort = FALSE,
            opacity=0.8) %>% 
      layout(legend=list(title=list(text='<b> Age Group </b>'),orientation = 'h', 
                         xanchor = "center", 
                         x = 0.5, y = -0.5),
             title = "Brooklyn")
 
  })
    output$piechart_age_BX = renderPlotly({
      data_to_plot = byage %>% 
        filter(outcome == as.character(input$outcome_age) &
                 boro == "BX" &
                 group != "Boroughwide" &
                 day == input$date_choice)
      
      plot_ly(labels = paste0("Age Group: ", data_to_plot$group),
              values = data_to_plot$count,
              type = "pie",
              marker = list(colors = brewer.pal(7,"Blues")),
              sort = FALSE,
              opacity=0.8) %>% 
        layout(legend=list(title=list(text='<b> Age Group </b>'),orientation = 'h', 
                           xanchor = "center", 
                           x = 0.5, y = -0.5),
               title = "Bronx")
      
    })
    
    output$piechart_age_QN = renderPlotly({
      data_to_plot = byage %>% 
        filter(outcome == as.character(input$outcome_age) &
                 boro == "QN" &
                 group != "Boroughwide" &
                 day == input$date_choice)
      
      plot_ly(labels = paste0("Age Group: ", data_to_plot$group),
              values = data_to_plot$count,
              type = "pie",
              marker = list(colors = brewer.pal(7,"Blues")),
              sort = FALSE,
              opacity=0.8) %>% 
        layout(legend=list(title=list(text='<b> Age Group </b>'),orientation = 'h', 
                           xanchor = "center", 
                           x = 0.5, y = -0.5),
               title = "Queens")
      
    })
    
    output$piechart_age_SI = renderPlotly({
      data_to_plot = byage %>% 
        filter(outcome == as.character(input$outcome_age) &
                 boro == "SI" &
                 group != "Boroughwide" &
                 day == input$date_choice)
      
      plot_ly(labels = paste0("Age Group: ", data_to_plot$group),
              values = data_to_plot$count,
              type = "pie",
              marker = list(colors = brewer.pal(7,"Blues")),
              sort = FALSE,
              opacity=0.8) %>% 
        layout(legend=list(title=list(text='<b> Age Group </b>'),orientation = 'h', 
                           xanchor = "center", 
                           x = 0.5, y = -0.5),
               title = "Staten Island")
      
    })
  

    output$Barchart_age = renderPlotly({
      
      a = byage %>% 
      filter(outcome == as.character(input$outcome_age) &
               group != "Boroughwide" &
               day == input$date_choice) %>% 
        ggplot(aes(fill = group, y = count, x = boro)) + 
        geom_bar(position="dodge", stat="identity")
      
      ggplotly(a)
      
    })
    
    
    output$timetrend_age = renderPlotly({
      
      b = byage %>% 
        filter(outcome == input$outcome_age &
                 group != "Boroughwide" 
                 ) %>% 
        filter(group == as.character(input$group)) %>% 
        ggplot(aes(x = day, y = count ,color = boro)) + 
        geom_line()
      
      ggplotly(b)
    })
    
    ######
    output$piechart_sex_MN = renderPlotly({
      data_to_plot = bysex %>% 
        filter(outcome == as.character(input$outcome_sex) &
                 boro == "MN" &
                 group != "Boroughwide" &
                 day == input$date_choice)
      
      plot_ly(labels = paste0("Gender Group: ", data_to_plot$group),
              values = data_to_plot$count,
              type = "pie",
              marker = list(colors = brewer.pal(7,"Blues")),
              sort = FALSE,
              opacity=0.8) %>% 
        layout(legend=list(title=list(text='<b> Gender </b>'),orientation = 'h', 
                           xanchor = "center", 
                           x = 0.5, y = -0.5),
               title = "Manhattan")
    })
    
    output$piechart_sex_BK = renderPlotly({
      data_to_plot = bysex %>% 
        filter(outcome == as.character(input$outcome_sex) &
                 boro == "BK" &
                 group != "Boroughwide" &
                 day == input$date_choice)
      
      plot_ly(labels = paste0("Gender Group: ", data_to_plot$group),
              values = data_to_plot$count,
              type = "pie",
              marker = list(colors = brewer.pal(7,"Blues")),
              sort = FALSE,
              opacity=0.8) %>% 
        layout(legend=list(title=list(text='<b> Gender Group </b>'),orientation = 'h', 
                           xanchor = "center", 
                           x = 0.5, y = -0.5),
               title = "Brooklyn")
      
    })
    output$piechart_sex_BX = renderPlotly({
      data_to_plot = bysex %>% 
        filter(outcome == as.character(input$outcome_sex) &
                 boro == "BX" &
                 group != "Boroughwide" &
                 day == input$date_choice)
      
      plot_ly(labels = paste0("Gender Group: ", data_to_plot$group),
              values = data_to_plot$count,
              type = "pie",
              marker = list(colors = brewer.pal(7,"Blues")),
              sort = FALSE,
              opacity=0.8) %>% 
        layout(legend=list(title=list(text='<b>Gender Group </b>'),orientation = 'h', 
                           xanchor = "center", 
                           x = 0.5, y = -0.5),
               title = "Bronx")
      
    })
    
    output$piechart_sex_QN = renderPlotly({
      data_to_plot = bysex %>% 
        filter(outcome == as.character(input$outcome_sex) &
                 boro == "QN" &  
                 group != "Boroughwide" &
                 day == input$date_choice)
      
      plot_ly(labels = paste0("Gender Group: ", data_to_plot$group),
              values = data_to_plot$count,
              type = "pie",
              marker = list(colors = brewer.pal(7,"Blues")),
              sort = FALSE,
              opacity=0.8) %>% 
        layout(legend=list(title=list(text='<b> Gender Group </b>'),orientation = 'h', 
                           xanchor = "center", 
                           x = 0.5, y = -0.5),
               title = "Queens")
      
    })
    
    output$piechart_sex_SI = renderPlotly({
      data_to_plot = bysex %>% 
        filter(outcome == as.character(input$outcome_sex) &
                 boro == "SI" &
                 group != "Boroughwide" &
                 day == input$date_choice)
      
      plot_ly(labels = paste0("Gender: ", data_to_plot$group),
              values = data_to_plot$count,
              type = "pie",
              marker = list(colors = brewer.pal(7,"Blues")),
              sort = FALSE,
              opacity=0.8) %>% 
        layout(legend=list(title=list(text='<b> Gender </b>'),orientation = 'h', 
                           xanchor = "center", 
                           x = 0.5, y = -0.5),
               title = "Staten Island")
      
    })
    
    
    output$Barchart_sex = renderPlotly({
      
      a = bysex %>% 
        filter(outcome == as.character(input$outcome_sex) &
                 day == input$date_choice &  
                 group != "Boroughwide" ) %>% 
        ggplot(aes(fill = group, y = count, x = boro)) + 
        geom_bar(position="dodge", stat="identity")
      
      ggplotly(a)
      
    })
    
    
    output$timetrend_sex = renderPlotly({
      
      b = bysex %>% 
        filter(outcome == input$outcome_sex &   group != "Boroughwide" ) %>% 
        filter(group == as.character(input$group)) %>% 
        ggplot(aes(x = day, y = count ,color = group)) + 
        geom_line()
      
      ggplotly(b)
    })
    
    
    
    #######
    
    output$piechart_race_MN = renderPlotly({
      data_to_plot = byrace %>% 
        filter(outcome == as.character(input$outcome_race) &
                 boro == "MN" &
                
                 day == input$date_choice)
      
      plot_ly(labels = paste0("Age Group: ", data_to_plot$group),
              values = data_to_plot$count,
              type = "pie",
              marker = list(colors = brewer.pal(7,"Blues")),
              sort = FALSE,
              opacity=0.8) %>% 
        layout(legend=list(title=list(text='<b> Age Group </b>'),orientation = 'h', 
                           xanchor = "center", 
                           x = 0.5, y = -0.5),
               title = "Manhattan")
    })
    
    output$piechart_race_BK = renderPlotly({
      data_to_plot = byrace %>% 
        filter(outcome == as.character(input$outcome_race) &
                 boro == "BK" &
                 day == input$date_choice)
      
      plot_ly(labels = paste0("Age Group: ", data_to_plot$group),
              values = data_to_plot$count,
              type = "pie",
              marker = list(colors = brewer.pal(7,"Blues")),
              sort = FALSE,
              opacity=0.8) %>% 
        layout(legend=list(title=list(text='<b> Age Group </b>'),orientation = 'h', 
                           xanchor = "center", 
                           x = 0.5, y = -0.5),
               title = "Brooklyn")
      
    })
    output$piechart_race_BX = renderPlotly({
      data_to_plot = byrace %>% 
        filter(outcome == as.character(input$outcome_race) &
                 boro == "BX" &
                 day == input$date_choice)
      
      plot_ly(labels = paste0("Age Group: ", data_to_plot$group),
              values = data_to_plot$count,
              type = "pie",
              marker = list(colors = brewer.pal(7,"Blues")),
              sort = FALSE,
              opacity=0.8) %>% 
        layout(legend=list(title=list(text='<b> Age Group </b>'),orientation = 'h', 
                           xanchor = "center", 
                           x = 0.5, y = -0.5),
               title = "Bronx")
      
    })
    
    output$piechart_race_QN = renderPlotly({
      data_to_plot = byrace %>% 
        filter(outcome == as.character(input$outcome_race) &
                 boro == "QN" &
                 day == input$date_choice)
      
      plot_ly(labels = paste0("Age Group: ", data_to_plot$group),
              values = data_to_plot$count,
              type = "pie",
              marker = list(colors = brewer.pal(7,"Blues")),
              sort = FALSE,
              opacity=0.8) %>% 
        layout(legend=list(title=list(text='<b> Age Group </b>'),orientation = 'h', 
                           xanchor = "center", 
                           x = 0.5, y = -0.5),
               title = "Queens")
      
    })
    
    output$piechart_race_SI = renderPlotly({
      data_to_plot = byrace %>% 
        filter(outcome == as.character(input$outcome_race) &
                 boro == "SI" &
                 day == input$date_choice)
      
      plot_ly(labels = paste0("Age Group: ", data_to_plot$group),
              values = data_to_plot$count,
              type = "pie",
              marker = list(colors = brewer.pal(7,"Blues")),
              sort = FALSE,
              opacity=0.8) %>% 
        layout(legend=list(title=list(text='<b> Age Group </b>'),orientation = 'h', 
                           xanchor = "center", 
                           x = 0.5, y = -0.5),
               title = "Staten Island")
      
    })
    
    
    output$Barchart_race = renderPlotly({
      
      a = byrace %>% 
        filter(outcome == as.character(input$outcome_race) &
                 day == input$date_choice) %>% 
        ggplot(aes(fill = group, y = count, x = boro)) + 
        geom_bar(position="dodge", stat="identity")
      
      ggplotly(a)
      
    })
    
    
    output$timetrend_race = renderPlotly({
      
      b = byrace %>% 
        filter(outcome == input$outcome_race) %>% 
        
        ggplot(aes(x = day, y = count ,color = group,group = boro)) + 
        geom_line()
      
      ggplotly(b)
    })
    
    
    
    
    
    
}


shinyApp(ui, server)