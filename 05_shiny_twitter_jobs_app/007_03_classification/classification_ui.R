source("007_03_classification/data_preparation/DataPreparationUI.R")



classificationPanel <- 
  tabPanel("Data Configuration",dataPreparationUI())        
                                     


classificationPanelOld <- navbarMenu("Extract Jobs"
                          ,tabPanel("Data Configuration",dataPreparationUI())        
                          ,tabPanel("Linear Regression") 
                          ,tabPanel("Decision Tree")
                          ,tabPanel("Gradient Boosting")
                          ,tabPanel("Random Forest")
                          ,tabPanel("SVM")
                          ,tabPanel("Neural Network")
                                  
)