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
                        choices = list("None" = 1, "Logarithmic" = 2), 
                        selected = 1)
            )#col
    ,column(width=3,sliderInput("sldSensibility","Sensibility",min=0,max=1,value=0.5,step=0.1) )
    ,column(width=3,verbatimTextOutput(outputId = "status"))
    ),#row
  
  fluidRow(
    column(width=4,actionButton("predict",label ="Extract Job Offers" ))
    ,column(width=6,"")
    ,column(width=2,actionButton("refresh","Refresh"))
  ),
  fluidRow(
    hr()
  )
  ,fluidRow(
    
    
  )#Row
  
  )#panel
    

}# dataPreparationUI