# server.R
# Shiny app to run checks of baseline tables for trials
# January 2022

shinyServer(function(input, output) {
  
  # reactive function to get data from Excel file and create t-statistics
  tstats = reactive({
    
    reason = NULL # reason it went wrong
    
    # PMCID & excel file
    inPMCID <- input$pmcid
    inFile <- input$excel.file
    
    #req(input$excel.file | input$pmcid | file.exists(input$excel.file$datapath))
    
    if (is.null(inFile)==TRUE & inPMCID==''){return(NULL)} # stop here if no file or PMCID
    if (is.null(inFile)==FALSE & inPMCID==''){
      # progress message & get data
      withProgress(message = 'Processing the data from Excel',
                   value = 0,{
                     incProgress(0.1)
                     data = my_read_excel(inFile$datapath)
                     incProgress(1)
                   })

      data$reason = NULL # to match structure from get_pmcid_table
      data$original_table = NULL # ditto
    }
    if (inPMCID!='' & is.null(inFile)==TRUE){
      
      # progress message & get data
      withProgress(message = 'Getting the baseline table data from PubMed Central',
                   value = 0,{
                     incProgress(0.1)
                     data = get_pmcid_table(inPMCID)
                     incProgress(1)
                   })
      
    }
    
    # get t-statistics for both statistics types (continuous and percent)
    tstats.c = tstats.p = NULL
    if(is.null(data$continuous) == FALSE){
      tstats.c = t.stats.continuous(indata = data$continuous)
    }
    if(is.null(data$percents) == FALSE){
      tstats.p = t.stats.percents(indata = data$percents)
    }
    # 
    tstats = bind_rows(tstats.c, tstats.p, .id = 'statistic') %>%
      mutate(study = 1) # dummy study number
    
    ## exclude perfectly correlated neighbours, e.g. male/female gender
    # does knock out some valid data, e.g, PMC7821012, but works very well on others, e.g, PMC6937882 with multiple examples
    to_remove = filter(tstats, statistic == 2) %>% # percents only
      arrange(row) %>%
      mutate(diff = abs(lag(t) - t*(-1)) ) %>% # perfectly negative correlation in neighbouring rows
      filter(diff < 0.001 & t!=0) %>% # small difference
      select(row) 
    n_removed = nrow(to_remove)
    if(n_removed > 0){
      # remove from stats
      tstats = anti_join(tstats, to_remove, by='row')
      # remove from data
      index = rep(TRUE, nrow(data$percents))
      index[to_remove$row] = FALSE
      data$percents = data$percents[index,]
    }
    
    # make simulated data
    n.sims = input$n.sims
    for (k in 1:n.sims){
      tstats.sim = make_sim(data) # simulate based under H0 using the observed data
      
      # get t-statistics for both statistics types
      tstats.c = tstats.p = NULL
      if(is.null(data$continuous) == FALSE){tstats.c = t.stats.continuous(indata = tstats.sim$continuous)}
      if(is.null(data$percents) == FALSE){tstats.p = t.stats.percents(indata = tstats.sim$percents)}
      tstats.sim = bind_rows(tstats.c, tstats.p, .id = 'statistic') %>%
        mutate(study = k+1) # dummy study number
      tstats = bind_rows(tstats, tstats.sim) # add to overall data
    }
    
    # return
    to.return = list()
    to.return$n_removed = n_removed
    to.return$tstats = tstats
    to.return$reason = data$reason
    to.return$original_table = data$original_table
    return(to.return)
    
  }) # end of function

  # show the t-statistics for each row of the table
  output$tableRes <- renderTable({
    inPMCID <- input$pmcid
    inFile <- input$excel.file
    if (is.null(inFile)==TRUE & inPMCID==''){table = NULL} # if no file or pmcid
    if (is.null(inFile)==FALSE | inPMCID!=''){
      table = filter(tstats()$tstats, study==1) %>% # just trial under consideration and not simulations
        select(statistic, row, size, mdiff, sem, t, p) %>%
        mutate(
          size = as.integer(size), # to remove decimal places in presentation
          statistic = ifelse(statistic==1, 'Continuous', 'Percent'))
    }
    table # return
  })
  
  # run the Bayesian test
  output$testRes <- renderText({
    
    inFile <- input$excel.file
    inPMCID <- input$pmcid
    if (is.null(inFile)==TRUE & inPMCID==''){text = NULL} # if no file
    if (is.null(inFile)==FALSE | inPMCID!=''){
      
      # progress message & run model
      withProgress(message = 'Running the Bayesian model',
                   detail = 'This may take a few minutes...', value = 0,{
                     incProgress(0.25)
                     for_model = filter(tstats()$tstats, study==1) # just trial under consideration and not simulations
                     results = run_bayes_test(in_data = for_model, p_prior = input$prior)
                     incProgress(1)
                   })
      
      # summary stats about the table
      n_rows = nrow(for_model)
      n_continuous = sum(for_model$statistic == 1)
      n_percent = sum(for_model$statistic == 2)
      n_removed = tstats()$n_removed
      
      # extract stats from model
      if(is.null(tstats()$reason)){
      mult = results$mult
      text = paste(
        'The table had ', n_rows,' rows of summary statistics, ', n_continuous ,' continuous and ', n_percent, ' percentage.\n',
        'There were ', n_removed,' rows removed because they were perfectly correlated with the previous row (e.g., percentage of males and females).\n',
        'The probability that the trial is under- or over-dispersed is ', results$p.flag, '.\n',
        "The precision multiplier is ", round(mult$mean,2), ", 90% CI ", round(mult$lower,2), ' to ', round(mult$upper,2), ". Multipliers under 1 indicated over-dispersion (lower precision) and over 1 under-dispersion (higher precision).", sep='')
      }
      # add error message to above as alternative text
      if(!is.null(tstats()$reason)){
        text = tstats()$reason
      }
      } # end of if for inPMCID
    text # return
  })
 
  # download the table data extracted from the XML file from PubMed Central
  output$download <- downloadHandler(
    filename = function(){paste('table_data_', input$pmcid, '.csv', sep='')},  # add PMCID to filename
    content = function(fname){
      inPMCID <- input$pmcid
      if (inPMCID==''){table = NULL} # if no PMCID
      table =  tstats()$original_table
      write.csv(x = table, file = fname, quote = FALSE, row.names = FALSE)
    }
  )
  
   
  # draw the histogram of t-statistics
  output$distPlot <- renderPlot({
    
    inPMCID <- input$pmcid
    inFile <- input$excel.file
    if (is.null(inFile)==TRUE & inPMCID==''){tplot = NULL} # stop here if no file
    if (is.null(inFile)==FALSE | inPMCID!=''){
      n.sims = input$n.sims
      ## draw the summary of the t-statistics ##
      
      ## create the median as a summary ##
      # create all CDFs
      average = filter(tstats()$tstats, study > 1) %>% # just simulations
        group_by(study) %>%
        mutate(cdf = ecdf(t)(t)) %>% # CDF per study
        ungroup() 
      # now calculate median CDF
      cdf_median = group_by(average, study) %>%
        arrange(study, t) %>%
        mutate(r = rank(t, ties.method = 'first')) %>%
        group_by(r) %>%
        summarise(mid = median(t)) %>%
        ungroup() %>%
        mutate(e = r/n(),
               study = 1) # had to provide study number
      # add first point of the CDF
      cdf_first = filter(cdf_median, r==1) %>%
        mutate(e = 0, r=0)
      cdf_median = bind_rows(cdf_first, cdf_median)
      
      # set up different colour and size for trial; move trial to last
      tstats = mutate(tstats()$tstats, study = ifelse(study==1, 999, study))
      colours = grey(runif(n = n.sims + 2, min=0.5, max=0.9)) # grey colours for simulations
      colours[1] = 'dodgerblue' # colour for median
      colours[n.sims + 2] = 'indianred1' # colour for trial
      sizes = rep(1, n.sims + 2)
      sizes[c(1,n.sims + 2)] = 2 # median and trial are larger
      # plot
      tplot = ggplot(data=tstats, aes(x=t, size=factor(study), colour=factor(study))) +
      theme_bw()+
      scale_size_manual(values = sizes)+
      scale_color_manual(values = colours)+
      stat_ecdf()+
      geom_step(data=cdf_median, aes(x=mid, y=e))+ # median CDF
      xlab('t-statistic')+
      ylab('Cumulative density')+
      theme(text = element_text(size=14),
            legend.position = 'none',
            panel.grid.minor = element_blank())
    }
    tplot
  })
}
)