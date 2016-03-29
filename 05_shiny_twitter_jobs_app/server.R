
#library(rsconnect) 
#deployApp("C:/Users/gonzalo/OneDrive/KSCHOOL/Proyecto/twitter_jobs_app")
cmdSearchTwitter_counter <- 1

shinyServer(function(input, output) {
  
    #Twitter Search results table
    output$tblTweets <- DT::renderDataTable({
      if (input$txtSearchBox!="" & input$cmdSearchTwitter==cmdSearchTwitter_counter){
        cmdSearchTwitter_counter <<- cmdSearchTwitter_counter + 1
        data <- getTweetsDataFrame(input$txtSearchBox,n=input$n)
        GenAllTMObjects (data)
        DT::datatable(genTweetTabTable(tweets.messages.dt), 
                      options = list(pageLength = 10), 
                      escape = c(1,2)
      )
      }
    }
    )#End of Twitter Search results table

    
    output$Dendrogram <- isolate({
      renderPlot({
      drawDendrogram(messages.corpus,
                     input$ddSparsity,
                     input$ddClusters,
                     input$ddFontSize
                     )
      
      })
      
   
    })
    
    #SEGUIR POR 007_5_corpus_main poniendo los controles para el dendograma.
    
    
  })
  