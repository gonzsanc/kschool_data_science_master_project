source("007_03_classification/layout//DataPreparationUI.R")



classificationPanel <- 
  tabPanel("Job Extractor",dataPreparationUI())        
                                     

classificationPanelOld <- navbarMenu("Extract Jobs"
                          ,tabPanel("Gradient Boosting")
                          ,tabPanel("Linear Regression") 
                          ,tabPanel("Decision Tree")
                          ,tabPanel("Random Forest")
                          ,tabPanel("SVM")
                          ,tabPanel("Neural Network")
                                  
)