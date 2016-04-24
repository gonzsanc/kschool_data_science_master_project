#Generic Classification Menu.
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
    ,column(width=3, sliderInput("sldSparsityGBM","Sparsity",min=0.5,max=0.999,value=0.8,step=0.001)
    )#col
    ,column(width=3,sliderInput("sldSensibility","Sensibility",min=0,max=1,value=0.5,step=0.02) )
    ,column(width=3,actionButton("predict",label ="Extract Job Offers" ))

    ),#row
  
  fluidRow(
    hr()
  )
  ,fluidRow(
    column(12,
           mainPanel(
      DT::dataTableOutput("jobsTable")
      )
    )
  )
  
  )#panel
    

}# dataPreparationUI