#rsconnect::deployApp('C:/Users/gonzalo/OneDrive/KSCHOOL/Proyecto/twitter_jobs_app')
source("Twitter_007_000_Loader.R")
library(shiny)
library(shinythemes)
library(DT)
#Main menu.
shinyUI(navbarPage("Twitter Jobs Seach App Prototype",
                   theme=shinytheme("spacelab"),
                  
                   findJobsPanel
                   ,corpusPanel
                   ,classificationPanel

                   
  ))
