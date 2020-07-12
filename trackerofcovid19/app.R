library(tidyverse)
library(shiny)
library(DT)
library(readxl)

# clean the data
data_yester = read.csv("./data_for_table/data-by-modzcta0710.csv") %>% 
    janitor::clean_names() %>% 
    mutate(date = as.Date("2020-07-10"))

data_today = read.csv("./data_for_table/data-by-modzcta0711.csv")%>% 
    janitor::clean_names() %>% 
    mutate(date = as.Date("2020-07-11"))

data = rbind(data_today,data_yester)
new_case = 0
new_death = 0

for (i in 1:177) {
    new_case[i] = data[i,4] - data[i+177,4]
    new_case
    
    new_death[i] = data[i,7] - data[i+177,7]
    new_death
}

for (i in 178:354) {
    
    new_case[i] = NA
    
    new_death[i] = NA
}

data = data %>% 
    mutate(new_case = new_case,
           new_death = new_death) 

for (i in 1:177) {
    data[i,14] = NA
    data[i,15] = NA
    
    if(data[i,12] >= 0 & data[i,13] >= 0){
        
        data[i,14] =  paste0("+",data[i,12],
                             "<br>",
                             data[i,4])
        
        data[i,15] = paste0("+",data[i,13],
                            "<br>",
                            data[i,7])
    }
    
    if(data[i,12] < 0 & data[i,13] >= 0){
        
        data[i,14] =  paste0(data[i,12],
                             "<br>",
                             data[i,4])
        
        data[i,15] = paste0("+",data[i,13],
                            "<br>",
                            data[i,7])
    }
    
    if(data[i,12] >= 0 & data[i,13] < 0){
        
        data[i,14] =  paste0("+",data[i,12],
                             "<br>",
                             data[i,4])
        
        data[i,15] = paste0(data[i,13],
                            "<br>",
                            data[i,7])
    }
    
    if(data[i,12] < 0 & data[i,13] < 0){
        
        data[i,14] =  paste0(data[i,12],
                             "<br>",
                             data[i,4])
        
        data[i,15] = paste0(" ",data[i,13],
                            "<br>",
                            data[i,7])
    }
}


table = data %>% 
    filter(date == max(date)) %>% 
    rename(Positive = V14,
           Death = V15) %>% 
    select(modified_zcta, neighborhood_name,borough_group,Positive,Death) %>%
    arrange(modified_zcta)

kable =knitr::kable(table,align = "ccccc")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Tracker of COVID-19 in NYC"),

    # Sidebar with a slider input for number of bins 
    fluidRow(
        DT::dataTableOutput("table")
    )
      
    )


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$table <- DT::renderDataTable(DT::datatable({
        table
    }, rownames = FALSE))
    
    

}

# Run the application 
shinyApp(ui = ui, server = server)
