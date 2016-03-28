library(shiny)

shinyServer(function(input, output) {
 
    #Twitter Search results table
    output$tblTweets <- DT::renderDataTable(
      if (input$txtSearchBox!=""){
      data <- getTweetsDataFrame(input$txtSearchBox,n=input$n)
      GenAllTMObjects (data)
      DT::datatable(genTweetTabTable(tweets.messages.dt), 
                    options = list(pageLength = 10), 
                    escape = c(1,2)
                    )
      }
    )#End of Twitter Search results table

    
  })
  