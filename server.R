# server.R
# Shiny app to run checks of baseline tables for trials
# January 2022

shinyServer(function(input, output) {
  
  # reactive function to get data from Excel file and create t-statistics
  tstats = reactive({
    inFile <- input$excel.file
    if (is.null(inFile)==TRUE){return(NULL)} # stop here if no file
    if (is.null(inFile)==FALSE){
      data = my_read_excel(inFile$datapath)
    }
    
    # get t-statistics for both statistics types
    tstats.c = t.stats.continuous(indata = data$continuous)
    tstats.p = t.stats.percents(indata = data$percents)
    # 
    tstats = bind_rows(tstats.c, tstats.p, .id = 'statistic') %>%
      mutate(study = 1) # dummy study number
    
    # make simulated data
    for (k in 1:20){
      tstats.sim = make_sim(data)
      
      # get t-statistics for both statistics types
      tstats.c = t.stats.continuous(indata = tstats.sim$continuous) 
      tstats.p = t.stats.percents(indata = tstats.sim$percents)
      tstats.sim = bind_rows(tstats.c, tstats.p, .id = 'statistic') %>%
        mutate(study = k+1) # dummy study number
      tstats = bind_rows(tstats, tstats.sim) # add to overall data
    }
    
    # return
    return(tstats)
    
  }) # end of function

  # run the Bayesian test
  output$testRes <- renderText({
    
    inFile <- input$excel.file
    if (is.null(inFile)==TRUE){text = NULL} # if no file
    if (is.null(inFile)==FALSE){
      results = run_bayes_test(in_data = tstats())
      mult = results$mult
      text = paste('The probability that the trial has an issue is ', results$p.flag, '.\n',
                   "The multiplier is ", round(mult$mean,2), ", 95% CI ", round(mult$lower,2), ' to ', round(mult$upper,2), ".", sep='')
      
    }
    
    text 
    
  })
  
  # draw the histogram of t-statistics
  output$distPlot <- renderPlot({
    
    inFile <- input$excel.file
    if (is.null(inFile)==TRUE){histo = NULL} # stop here if no file
    if (is.null(inFile)==FALSE){
    # draw the histogram
    histo = ggplot(data=tstats(), aes(x=t, fill=statistic)) +
      theme_bw()+
      scale_fill_manual(values = c('darkseagreen3','orange1'), labels=c('Continuous','Percentages'))+
      geom_histogram()+
      xlab('t-statistic')+
      ylab('Count')
    }
    histo
  })
}
)