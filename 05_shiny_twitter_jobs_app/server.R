
#library(rsconnect) 
#deployApp("C:/Users/gonzalo/OneDrive/KSCHOOL/Proyecto/twitter_jobs_app")
cmdSearchTwitter_counter <- 1
oldmethod <- "N/A"


shinyServer(function(input, output) {

#SEARCH TAB =========================================================================
    #Twitter Search results table
    output$tblTweets <- DT::renderDataTable({
      input$cmdSearchTwitter
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

    
    output$Dendrogram <- #isolate({
      renderPlot({
      input$cmdSearchTwitter
      drawDendrogram(messages.corpus,
                     input$ddSparsity,
                     input$ddClusters,
                     input$ddFontSize
                     )
      
        })
   
    ##})#EndofDendrogram
    
##END SEARCH TAB =========================================================================
    
##CORPUS TAB =========================================================================
    
  #Term Frequency schedule
    
    output$termFreqPlot <- 
      renderPlot({
        input$cmdSearchTwitter
        drawTermFreqPlot(messages.corpus,
                       input$termFreq
        )
 
    })
    
    
    #Para los tópicos:
    #input$numberOfTopicsSlider

    # Associations
    output$PCACorrelationMatrix <- renderPlot({
      input$termFreq
      genTopicsCorrPCAObjects (messages.corpus,minFreq=input$termFreq,numberOfTopics=10)
      genCorrplot()

    })
    
    output$tblLDA <- DT::renderDataTable({
      input$LDAMethod
      input$numberOfTopicsSlider
      
      
      recalculate <- T
      
      if (oldmethod!=input$LDAMethod){
        oldmethod <<- input$LDAMethod
        recalculate <- F
      }else{
        recalculate <- T
      }
      
      
      
      DT::datatable(getLDADataTable(messages.corpus,
                                    numberOfTopics=input$numberOfTopicsSlider,
                                    method=input$LDAMethod,recalculate), 
                    options = list(pageLength = 25), 
                    escape = c(1,2))
      })
      
    output$downloadLDA <- downloadHandler(
      filename = function() { paste("LDATopics", '.csv', sep='') },
      content = function(file) {
        write.csv(
          
          getLDADataTable(messages.corpus,
                          numberOfTopics=input$numberOfTopicsSlider,
                          method=input$LDAMethod,F), 
          file)
      }
    )
    
  
    })#END OF SERVER
  