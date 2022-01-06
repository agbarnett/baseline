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
    
    ## exclude perfectly correlated neighbours, e.g. male/female gender
    # does knock out some valid data, e.g, PMC7821012, but works very well on others, e.g, PMC6937882 with multiple examples
    to_remove = filter(tstats, statistic %in% c('percent','numbers')) %>%
      arrange(row) %>%
      mutate(diff = abs(lag(t) - t*(-1)) ) %>% # perfectly negative correlation in neighbouring rows
      filter(diff < 0.001 & t!=0) %>% # small difference
      select(row) 
    n_removed = nrow(to_remove)
    if(n_removed > 0){
      for_model = anti_join(for_model, to_remove, by=c('pmcid','row'))
    }
    
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
    to.return = list()
    to.return$n_removed = n_removed
    to.return$tstats = tstats
    return(to.return)
    
  }) # end of function

  # run the Bayesian test
  output$testRes <- renderText({
    
    inFile <- input$excel.file
    if (is.null(inFile)==TRUE){text = NULL} # if no file
    if (is.null(inFile)==FALSE){
      
      # progress message & run model
      withProgress(message = 'Running Bayesian model',
                   detail = 'This may take a few minutes...', value = 0,{
                     incProgress(0.1)
                     results = run_bayes_test(in_data = tstats()$tstats)
                     incProgress(1)
                   })
      
      # summary stats about the table
      for_stats = filter(tstats()$tstats, study==1) # just trial under consideration and not simulations
      n_rows = nrow(for_stats)
      n_continuous = sum(for_stats$statistic == 1)
      n_percent = sum(for_stats$statistic == 2)
      n_removed = tstats()$n_removed
      
      # extract stats from model
      mult = results$mult
      text = paste(
        'The table had ', n_rows,' rows of summary statistics, ', n_continuous ,' continuous and ', n_percent, ' percentages.\n',
        'There were ', n_removed,' rows removed because they were perfectly correlated with the previous row (e.g., percentage of males and females).\n',
        'The probability that the trial is under- or over-dispersed is ', results$p.flag, '.\n',
        "The precision multiplier is ", round(mult$mean,2), ", 90% CI ", round(mult$lower,2), ' to ', round(mult$upper,2), ". Multipliers under 1 indicated over-dispersion (lower precision) and over 1 under-dispersion (higher precision).", sep='')
      
    }
    
    text 
    
  })
  
  # draw the histogram of t-statistics
  output$distPlot <- renderPlot({
    
    inFile <- input$excel.file
    if (is.null(inFile)==TRUE){tplot = NULL} # stop here if no file
    if (is.null(inFile)==FALSE){
      n.sims = input$n.sims
      ## draw the summary of the t-statistics
      # set up different colour and size for trial; move trial to last
      tstats = mutate(tstats()$tstats, study = ifelse(study==1, 999, study))
      colours = grey(runif(n = n.sims + 1, min=0.5, max=0.9)) # grey colours for simulations
      colours[n.sims + 1] = 'indianred1' # 
      sizes = rep(1, n.sims + 1)
      sizes[n.sims + 1] = 2
      # plot
      tplot = ggplot(data=tstats, aes(x=t, size=factor(study), colour=factor(study))) +
      theme_bw()+
      scale_size_manual(values = sizes)+
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