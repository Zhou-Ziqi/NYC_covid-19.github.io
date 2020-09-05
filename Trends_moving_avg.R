library(tidyverse)
library(shiny)
library(plotly)
library(RColorBrewer)
# what we already have
borocase_new <- read_csv("data/boro_newcase_trend.csv") %>% select(-1)


boro_incidence_case_daily = borocase_new %>% 
  mutate(boro = factor(boro)) %>% 
  # filter(date_of_interest %in% week) %>%
  rename(hospitalization_count = hospitalized_count) %>% 
  pivot_longer(case_count:death_count,
               names_to = "type",
               values_to = "count") %>% 
  mutate(#type = str_replace_all(type, "cum_", ""),
    type = str_to_title(str_replace_all(type, "_", " ")),
    newtype = str_replace_all(paste(type, "Rate"),"Count ",""),
    pop_num = ifelse(boro == "Bronx", 1434693, 
                     ifelse(boro == "Brooklyn",2582830, 
                            ifelse(boro == "Manhattan",1611943, 
                                   ifelse(boro == "Staten Island", 476179,
                                          ifelse(boro == "Queens", 2288710, 2288710))))),
    rate = round(count/pop_num*1000000,1)) %>% 
  rename(Borough = boro,
         Date = date_of_interest,
         Count = count,
         Rate = rate) %>% 
  mutate(type = str_replace_all(type,"Case Count", "Total Cases"),
         type = str_replace_all(type,"Death Count", "Total Deaths"),
         type = str_replace_all(type,"Hospitalization Count", "Total Hospitalizations"),
         newtype = str_replace_all(newtype,"Case Rate","Case Rate (per 100,000 people)"),
         newtype = str_replace_all(newtype,"Death Rate","Death Rate (per 100,000 people)"),
         newtype = str_replace_all(newtype,"Hospitalization Rate","Hospitalization Rate (per 100,000 people)"))%>% 
  mutate(type = factor(type, levels = c("Total Cases","Total Deaths","Total Hospitalizations")),
         newtype = factor(newtype, levels = c("Case Rate (per 100,000 people)","Death Rate (per 100,000 people)","Hospitalization Rate (per 100,000 people)"))) %>% 
  arrange(type) %>% 
  arrange(newtype)


NBH_trends1 = boro_incidence_case_day %>% distinct(Borough) %>% pull()
choices_trend_mvag = c("Total Cases", "Total Deaths","Total Hospitalizations") 
choices_trend_mvag_rate = c("Case Rate (per 100,000 people)", "Death Rate (per 100,000 people)","Hospitalization Rate (per 100,000 people)")




ui =  fluidPage(
  
  
  fluidRow(column(width = 10,offset = 1 ,selectInput("choices_trend_mvag", 
                                        label = "Choose an outcome", 
                                        choices = choices_trend_mvag
                                        )),
           
           column(width = 10,offset = 1 , 
                 plotlyOutput("trendsmoving_avg",width = "100%", height = "100%")),
           
  ),
  
  fluidRow(column(width = 10,offset = 1 ,selectInput("choices_trend_mvag_rate", 
                                          label = "Choose an outcome", 
                                          choices = choices_trend_mvag_rate
  )),
  
  column(width = 10,offset = 1 , 
         plotlyOutput("trendsmoving_avg_rate",width = "100%", height = "100%"))
  )
  
  
)


server = function(input, output) {
  
  
  
  
  output$trendsmoving_avg = renderPlotly({
    
    borocase_new_day_bx_case = boro_incidence_case_day %>% 
      filter(type == input$choices_trend_mvag ) %>% 
      select(-newtype,-Rate)
    
    N = nrow(borocase_new_day_bx_case )
    cases = pull(borocase_new_day_bx_case ,Count)
    #deaths = pull(df_totalcase_bx ,deaths)
    ave.cases = rep(0, N-6)
    #ave.deaths = rep(0, N-6)
    
    for (i in 4:(N-3)) {
      ave.cases[i] = mean(cases[(i-3):(i+3)])
      #ave.deaths[i] = mean(deaths[(i-3):(i+3)])
    }
    for (i in 1:3) {
      ave.cases[i] = ave.cases[1]
      #ave.deaths[i] = ave.deaths[1]
    }
    for (i in (N-2):N) {
      ave.cases[i] = ave.cases[N-3]
      #ave.deaths[i] = ave.deaths[N-3]
    }
    
    df.ave = borocase_new_day_bx_case  %>% 
      mutate(ave_cases = round(ave.cases))
    
    a = ggplot(df.ave, aes(x = Date, y = Count)) + geom_col(color = "#F6BDBC", fill = "#F6BDBC", alpha = 0.8) + geom_line(aes(x = Date, y = ave.cases), color = "red", size = 1) + 
      ggtitle(paste0(input$choices_trend_mvag, " in different Boroughs")) + theme_classic() + 
      theme_bw() +
      theme(panel.border = element_blank()) +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
      theme(axis.line = element_line(colour = "black")) +
      theme(strip.background = element_blank()) + 
      theme(axis.text.x = element_text(angle = 65, hjust = 1)) + 
      theme(legend.title = element_blank()) +
      theme(panel.spacing.y=unit(1, "lines")) + 
        facet_wrap(.~Borough, scales = "free") + 
      xlab("") + 
      ylab("")
      
      ggplotly(a)
    
  })

  
  output$trendsmoving_avg_rate = renderPlotly({
    
    borocase_new_day_bx_case = boro_incidence_case_day %>% 
      filter(newtype == input$choices_trend_mvag_rate ) %>% 
      select(-newtype,-Rate)
    
    N = nrow(borocase_new_day_bx_case )
    cases = pull(borocase_new_day_bx_case ,Count)
    #deaths = pull(df_totalcase_bx ,deaths)
    ave.cases = rep(0, N-6)
    #ave.deaths = rep(0, N-6)
    
    for (i in 4:(N-3)) {
      ave.cases[i] = mean(cases[(i-3):(i+3)])
      #ave.deaths[i] = mean(deaths[(i-3):(i+3)])
    }
    for (i in 1:3) {
      ave.cases[i] = ave.cases[1]
      #ave.deaths[i] = ave.deaths[1]
    }
    for (i in (N-2):N) {
      ave.cases[i] = ave.cases[N-3]
      #ave.deaths[i] = ave.deaths[N-3]
    }
    
    df.ave = borocase_new_day_bx_case  %>% 
      mutate(ave_cases = round(ave.cases))
    
    a = ggplot(df.ave, aes(x = Date, y = Count)) + geom_col(color = "#F6BDBC", fill = "#F6BDBC", alpha = 0.8) + geom_line(aes(x = Date, y = ave.cases), color = "red", size = 1) + 
      ggtitle(paste0(input$choices_trend_mvag_rate, " in different Boroughs")) + theme_classic() + 
      theme_bw() +
      theme(panel.border = element_blank()) +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
      theme(axis.line = element_line(colour = "black")) +
      theme(strip.background = element_blank()) + 
      theme(axis.text.x = element_text(angle = 65, hjust = 1)) + 
      theme(legend.title = element_blank()) +
      theme(panel.spacing.y=unit(1, "lines")) + 
      facet_wrap(.~Borough, scales = "free") + 
      xlab("") + 
      ylab("")
    
    ggplotly(a)
    
  })
}



shinyApp(ui, server)
  
  
  
  
  