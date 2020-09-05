library(tidyverse)
library(plotly)
library(readxl)
library(shiny)
###projection shiny app draft
#read the data


train_data =read_xlsx("./data/WeeklyProjections20200901.xlsx",sheet = 3) %>% 
  janitor::clean_names() %>% 
  separate(col = new_cases, into = c("new_cases_value","new_cases_lower","new_cases_upper")) %>%
  mutate(new_cases_value = as.numeric(new_cases_value),
         new_cases_lower = as.numeric(new_cases_lower),
         new_cases_upper = as.numeric(new_cases_upper))%>% 
  separate(col = new_deaths, into = c("new_deaths_value","new_deaths_lower","new_deaths_upper")) %>%
  mutate(new_deaths_value = as.integer(new_deaths_value),
         new_deaths_lower = as.integer(new_deaths_lower),
         new_deaths_upper = as.integer(new_deaths_upper)
  ) %>% 
  separate(col = new_total_hospitalizations, into = c("new_hosp_value","new_hosp_lower","new_hosp_upper")) %>%
  mutate(new_hosp_value = as.integer(new_hosp_value),
         new_hosp_lower = as.integer(new_hosp_lower),
         new_hosp_upper = as.integer(new_hosp_upper),
         date = as.Date(date,format = "%m/%d/%y")
  ) 

project_data = read_xlsx("./data/WeeklyProjections20200901.xlsx",sheet = 4) %>% 
  janitor::clean_names() %>% 
  separate(col = new_cases, into = c("new_cases_value","new_cases_lower","new_cases_upper")) %>%
  mutate(new_cases_value = as.numeric(new_cases_value),
         new_cases_lower = as.numeric(new_cases_lower),
         new_cases_upper = as.numeric(new_cases_upper)
  ) %>% 
  separate(col = new_deaths, into = c("new_deaths_value","new_deaths_lower","new_deaths_upper")) %>%
  mutate(new_deaths_value = as.integer(new_deaths_value),
         new_deaths_lower = as.integer(new_deaths_lower),
         new_deaths_upper = as.integer(new_deaths_upper)) %>% 
  separate(col = new_total_hospitalizations, into = c("new_hosp_value","new_hosp_lower","new_hosp_upper")) %>%
  mutate(new_hosp_value = as.integer(new_hosp_value),
         new_hosp_lower = as.integer(new_hosp_lower),
         new_hosp_upper = as.integer(new_hosp_upper),
         date = as.Date(date,format = "%m/%d/%y"),
         week = format(date, format="%Y-%U")) 


#select seasonality
season = train_data %>% distinct(seasonality) %>% pull()
#select location
location = train_data %>% distinct(location) %>% pull()
#select intervention
intervention = project_data %>% distinct(intervention) %>% pull()


#ui

ui = fluidPage(
  fluidRow(
    column(width = 10, offset = 1, h2("NYC COVID-19 Projection")),
    column(width = 10, offset = 1, h4("Model by Columbia")),
    
    column(width = 10, offset = 1, span(htmlOutput("Projecttext"), style="font-size: 15px; line-height:150%")),
    column(width = 10, offset = 1, helpText(paste("Train Data last updated: ", max(train_data$date)))),
    column(4, offset = 1,
           selectInput("season", 
                       label = "Choose Seasonality Assumption", 
                       choices = season
           )),
    column(4, offset = 1,
           selectInput("loc_proj", 
                       label = "Choose a location", 
                       choices = location
           )),
    column(4, offset = 1,
           selectInput("interve", 
                       label = "Choose intervention", 
                       choices = intervention
           )),
   
    column(width = 10, offset = 1, plotlyOutput("proj_line_case", width = "100%", height = "100%")),
    column(width = 10, offset = 1, plotlyOutput("proj_line_deat", width = "100%", height = "100%")),
    column(width = 10, offset = 1, plotlyOutput("proj_line_hosp", width = "100%", height = "100%"))
   
    #column(width = 10, offset = 1, helpText("Data Sources: https://github.com/nychealth/coronavirus-data"))
  )
)

server = function(input, output) {
  output$proj_line_case = renderPlotly({
    train_data_sel = train_data %>% 
      filter(seasonality == input$season  & location == input$loc_proj) 
    season_city_newcases = project_data %>% 
      filter(seasonality == input$season  & intervention ==input$interve & location == input$loc_proj)
    b = ggplot() + 
      geom_line(data = train_data_sel ,aes(x = date, y = new_cases_value,group = 1)) +
      geom_point(data = train_data_sel , aes(x = date, y = new_cases_value)) +
      theme_minimal() +
      #theme(legend.position = "none")+
      #theme(panel.spacing.y=unit(2, "lines")) + 
      #ggplot(season_city_newcases) + 
      geom_path(data = season_city_newcases,aes(y=new_cases_value, x=date, colour = "Projected Median Number",group = 1))+
      geom_ribbon(data = season_city_newcases, aes(ymin=new_cases_lower, 
                                                   ymax=new_cases_upper, 
                                                   x=date, fill = "IQR"), alpha = 0.2,group = 1) + 
      scale_colour_manual("",values="red") + 
      scale_fill_manual("",values="grey12") + 
      theme_bw() +
      theme(panel.border = element_blank()) +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
      theme(axis.line = element_line(colour = "black")) +
      theme(strip.background = element_blank()) + 
      theme(axis.text.x = element_text(angle = 65, hjust = 1)) + 
      theme(panel.spacing.y=unit(1, "lines")) + 
      theme(legend.position = "none")+
      xlab("") + 
      ylab("") + 
      ggtitle(paste0("New Total Hospitalizations Under ", input$input$interve," Control Scenarios"))
    
    ggplotly(b)
  })
  
  output$proj_line_deat = renderPlotly({
    train_data_sel = train_data %>% 
      filter(seasonality == input$season  & location == input$loc_proj) 
    season_city_newcases = project_data %>% 
      filter(seasonality == input$season  & intervention ==input$interve & location == input$loc_proj)
    
    b = ggplot() + 
      geom_line(data = train_data_sel ,aes(x = date, y = new_deaths_value,group = 1)) +
      geom_point(data = train_data_sel , aes(x = date, y = new_deaths_value)) +
      theme_minimal() +
      #theme(legend.position = "none")+
      #theme(panel.spacing.y=unit(2, "lines")) + 
      #ggplot(season_city_newcases) + 
      geom_path(data = season_city_newcases,aes(y=new_deaths_value, x=date, colour = "Projected Median Number",group = 1))+
      geom_ribbon(data = season_city_newcases, aes(ymin=new_deaths_lower, 
                                                   ymax=new_deaths_upper, 
                                                   x=date, fill = "IQR"), alpha = 0.2,group = 1) + 
      scale_colour_manual("",values="red") + 
      scale_fill_manual("",values="grey12") + 
      theme_bw() +
      theme(panel.border = element_blank()) +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
      theme(axis.line = element_line(colour = "black")) +
      theme(strip.background = element_blank()) + 
      theme(axis.text.x = element_text(angle = 65, hjust = 1)) + 
      theme(panel.spacing.y=unit(1, "lines")) + 
      theme(legend.position = "none")+
      xlab("") + 
      ylab("") + 
      ggtitle(paste0("New Deaths Under", input$input$interve,"Control Scenarios"))
    
    ggplotly(b)
  })
  
  output$proj_line_hosp = renderPlotly({
    train_data_sel = train_data %>% 
      filter(seasonality == input$season  & location == input$loc_proj) 
    season_city_newcases = project_data %>% 
      filter(seasonality == input$season  & intervention ==input$interve & location == input$loc_proj)
    
    
    b = ggplot() + 
      geom_line(data = train_data_sel ,aes(x = date, y = new_hosp_value,group = 1)) +
      geom_point(data = train_data_sel , aes(x = date, y = new_hosp_value)) +
      theme_minimal() +
      #theme(legend.position = "none")+
      #theme(panel.spacing.y=unit(2, "lines")) + 
      #ggplot(season_city_newcases) + 
      geom_path(data = season_city_newcases,aes(y=new_hosp_value, x=date, colour = "Projected Median Number",group = 1))+
      geom_ribbon(data = season_city_newcases, aes(ymin=new_hosp_lower, 
                                                   ymax=new_hosp_upper, 
                                                   x=date, fill = "IQR"), alpha = 0.2,group = 1) + 
      scale_colour_manual("",values="red") + 
      scale_fill_manual("",values="grey12") + 
      theme_bw() +
      theme(panel.border = element_blank()) +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
      theme(axis.line = element_line(colour = "black")) +
      theme(strip.background = element_blank()) + 
      theme(axis.text.x = element_text(angle = 65, hjust = 1)) + 
      theme(panel.spacing.y=unit(1, "lines")) + 
      theme(legend.position = "none")+
      xlab("") + 
      ylab("") + 
      ggtitle(paste("New Total Hospitalizations Under", input$input$interve,"Control Scenarios"))
    
    ggplotly(b)
  })
  
  
  
}

shinyApp(ui, server)







