dataPreparationUI <- function(){
  
  mainPanel(
    fluidRow(
           
      h4("Gradient Boosting Trees Predictive Modelling")
  ),
  
  fluidRow(
    column(width=3,
           
           radioButtons("scoring", "Term Document Matrix Scores:",
                        c("Frequency" = "frq",
                          "Tf-idf" = "tfi")
                        ,inline=T
                        )                    
           )
    ,column(width=3,
            selectInput("selectScale", label = "Data Normalization", 
                        choices = list("Centered" = 1, "Logarithmic" = 2), 
                        selected = 1)
            )#col
    ,column(width=3,sliderInput("sldSensibility","Sensibility",min=0,max=1,value=0.5,step=0.1) )
    ,column(width=3,verbatimTextOutput(outputId = "status"))
    ),#row
  
  fluidRow(
    column(width=4,actionButton("predict",label ="Extract Job Offers" ))
    ,column(width=5,checkboxInput("simplify","GBM Model Simplification",value = T))
    ,column(width=3,"")
    #,column(width=3,actionButton("refresh","Update Twitter Data"))
  ),
  fluidRow(
    hr()
  )
  ,fluidRow(
  mainPanel(
      DT::dataTableOutput("jobsTable")
    )
  )#Row
  
  )#panel
    

}# dataPreparationUI