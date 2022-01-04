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
    n.sims = input$n.sims
    for (k in 1:n.sims){
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
    if (is.null(inFile)==TRUE){tplot = NULL} # stop here if no file
    if (is.null(inFile)==FALSE){
    # draw the summary of the t-statistics
    colours = grey(runif(n = input$n.sims + 1, min=0.2, max=0.8)) # grey colours for simulations
    colours[1] = 'indianred1' # 
    tplot = ggplot(data=tstats(), aes(x=t, colour=factor(study))) +
      theme_bw()+
      scale_color_manual(values = colours)+
      stat_ecdf()+
      xlab('t-statistic')+
      ylab('Cumulative density')+
      theme(legend.position = 'none',
            panel.grid.minor = element_blank())
    }
    tplot
  })
}
)