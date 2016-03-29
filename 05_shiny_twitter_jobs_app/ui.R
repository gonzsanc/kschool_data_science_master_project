#rsconnect::deployApp('C:/Users/gonzalo/OneDrive/KSCHOOL/Proyecto/shiny_app')
source("Twitter_007_000_Loader.R")
library(shinythemes)
library(shiny)
library(DT)

shinyUI(navbarPage("Twitter Jobs Seach App",
                   theme=shinytheme("spacelab"),
                  
                   findJobsPanel
                   ,corpusPanel

                  
                   
                   
  ))