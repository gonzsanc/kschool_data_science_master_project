#library(rsconnect) 
#deployApp("C:/Users/gonzalo/OneDrive/KSCHOOL/Proyecto/proyecto-fin-master")
#options(shiny.host = "0.0.0.0")
source("007_03_classification/classification_server_external.R")

shinyServer(function(input, output) {
  
  #SERVER BODY.
  cmdSearchTwitter_counter <- 1
  oldmethod <- "N/A"
#SEARCH TAB =========================================================================
    #Twitter Search results table
  
      output$tblTweets <- DT::renderDataTable({
        input$cmdSearchTwitter
        if (isolate(input$txtSearchBox)!="" & isolate(input$cmdSearchTwitter)==cmdSearchTwitter_counter){
          cmdSearchTwitter_counter <<- cmdSearchTwitter_counter + 1
          data <- getTweetsDataFrame(isolate(input$txtSearchBox),n=isolate(input$n))
          GenAllTMObjects (data)
          DT::datatable(genTweetTabTable(tweets.messages.dt), 
                        options = list(pageLength = 10), 
                        escape = c(1,2))
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
      renderChart2 ({
        input$cmdSearchTwitter
        p <- drawTermFreqPlot (messages.corpus,input$termFreq)
        return (p)
    })
    
    
    #Para los tópicos:
    #input$numberOfTopicsSlider

    # Associations
    
    
    output$PCACorrelationMatrix <- renderPlot({
      getPCACorrMatrix(messages.corpus,minFreq=input$termFreq,numberOfTopics=10)
      
    })
    
    output$PCAVariance <- renderPlot({
      genTopicsCorrPCAObjects (messages.corpus,minFreq=input$termFreq,numberOfTopics=6)
    })
    
    
  #Latent topics     
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
                    escape = c(1,2),filter = "bottom")
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


    output$downloadMessagesLDA <- downloadHandler(
      filename = function() { paste("LDATopics", '.csv', sep='') },
      content = function(file) {
        write.csv(
          
          genMessagesWithTopics(method=input$LDAMethod), 
          file)
      }
    )

    
    ######CLASSIFICATION GBM
      source("007_03_classification/classification_server.R")
      #STATUS TEXTBOX
      #JobsTable
      jobsTable <- reactiveValues()
      jobsTable$value <- DT::datatable(data = data.frame())
      
      
      output$jobsTable <- DT::renderDataTable({
        input$predict
        
        scor <- reactiveValues()
        spar <- reactiveValues()
        sens <- reactiveValues()
        scor$value = isolate(input$scoring)
        spar$value = isolate(input$sldSparsityGBM)
        sens$value = isolate(input$sldSensibility)
        
        
        if(!is.null(messages.corpus)){
          p <- getPrediction(sparsity = spar$value, sensibility = sens$value)
          jobs <- getPredictedJobs(classifiedMessages = p,method = scor$value)
          jobsTable$value <- DT::datatable(jobs)}
        jobsTable$value
      })
      
      
})#END OF SERVER
  
