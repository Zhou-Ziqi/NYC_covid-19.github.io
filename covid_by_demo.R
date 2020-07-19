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

