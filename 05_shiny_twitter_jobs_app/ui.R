#rsconnect::deployApp('C:/Users/gonzalo/OneDrive/KSCHOOL/Proyecto/twitter_jobs_app')
source("Twitter_007_000_Loader.R")
library(shinythemes)
library(shiny)
library(DT)

shinyUI(navbarPage("Twitter Jobs Seach App Prototype",
                   theme=shinytheme("spacelab"),
                  
                   findJobsPanel
                   ,corpusPanel

                   
  ))

