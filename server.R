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
      
      data$original_table = NULL # to match previous structure

    }
    if (inPMCID!='' & is.null(inFile)==TRUE){ # get data from PMCID
      
      # progress message & get data
      withProgress(message = 'Getting the baseline table data from PubMed Central',
                   value = 0,{
                     incProgress(0.1)
                     data = get_pmcid_table(inPMCID)
                     incProgress(1)
                   })
      
    }
    
    # big if, only run stats if there's no reason not to
    if(is.null(data$reason)){
      
      # get t-statistics for both statistics types (continuous and percent)
      tstats = make_stats_for_bayes_model(data$data) %>%
        mutate(study = 1) # dummy study number
      
      ## exclude perfectly correlated neighbours, e.g. male/female gender
      # does knock out some valid data, e.g, PMC7821012, but works very well on others, e.g, PMC6937882 with multiple examples
      to_remove = filter(tstats, statistic == 'percent') %>% # percents only
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
      
      # make simulated data from summary statistics
      n.sims = input$n.sims
      all_stats = tstats # start with observed data
      for (k in 1:n.sims){
        sim.data = make_sim(tstats) # simulate based under H0 using the observed data
        
        # get t-statistics for both statistics types
        tstats.c = tstats.p = NULL
        if(!is.null(sim.data$continuous)){
          tstats.c = t.stats.continuous(indata = sim.data$continuous) %>%
            mutate(statistic = 'continuous')
        }
        if(!is.null(sim.data$percents)){
          tstats.p = t.stats.percents(indata = sim.data$percents) %>%
            mutate(statistic = 'percent')
        }
        tstats.sim = bind_rows(tstats.c, tstats.p) %>%
          mutate(pmcid = tstats$pmcid[1], # copy PMCID number
                 study = k+1) # dummy study number
        all_stats = bind_rows(all_stats, tstats.sim) # add to overall data
      }
      
    } # end of big if
    if(!is.null(data$reason)){ # if there is a reason ...
      n_removed = NULL
      all_stats = NULL
      data$original_table = NULL
    }
    
    # return
    to.return = list()
    to.return$n_removed = n_removed
    to.return$n_groups = max(data$data$column) # number of treatment groups
    to.return$tstats = all_stats
    if(!is.null(data$reason)){to.return$reason = data$reason} # carry-forward
    to.return$original_table = data$original_table
    return(to.return)
    
  }) # end of reaction function
  
  # show the t-statistics for each row of the table
  output$tableRes <- renderTable({
    inPMCID <- input$pmcid
    inFile <- input$excel.file
    if (is.null(inFile)==TRUE & inPMCID=='' | is.null(tstats()$tstats)){table = NULL} # if no file or pmcid
    if (is.null(inFile)==FALSE | inPMCID!='' & !is.null(tstats()$tstats)){ # must be some t-statistics
      table = filter(tstats()$tstats, study==1) %>% # just trial under consideration and not simulations
        select(statistic, row, size, mdiff, sem, t, p) %>%
        mutate(
          size = as.integer(size)) # to remove decimal places in presentation
    }
    table # return
  })
  
  # optional error/warning message (to do, add warning)
  output$error <- renderText({
    inFile <- input$excel.file
    inPMCID <- input$pmcid
    if (is.null(inFile)==TRUE & inPMCID==''){error = NULL} # if no file
    if (is.null(tstats()$reason)){error = NULL} # if no error
    if (!is.null(tstats()$reason)){error = tstats()$reason} # if there is an error
    error
  })
  
  # run the Bayesian test
  output$testRes <- renderText({
    
    inFile <- input$excel.file
    inPMCID <- input$pmcid
    if ((is.null(inFile)==TRUE & inPMCID=='') | !is.null(tstats()$reason)){text = NULL} # if no file
    if (is.null(inFile)==FALSE | inPMCID!='' & is.null(tstats()$reason)){ # must be some t-statistics
      
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
      n_groups = tstats()$n_groups
      n_continuous = sum(for_model$statistic == 'continuous')
      n_percent = sum(for_model$statistic == 'percent')
      n_removed = tstats()$n_removed
      
      # extract stats from model
      if(is.null(tstats()$reason)){
        mult = results$mult
        text = paste(
          'The table had ', n_rows,' rows of summary statistics, ', n_continuous ,' continuous and ', n_percent, ' percentage.\n',
          'There were ', n_groups,' treatment groups.\n',
          'There were ', n_removed,' rows removed because they were perfectly correlated with the previous row (e.g., percentage of males and females).\n',
          'The probability that the trial is under- or over-dispersed is ', results$p.flag, '.\n',
          "The precision multiplier is ", round(mult$mean,2), ", 90% CI ", round(mult$lower,2), ' to ', round(mult$upper,2), ". Multipliers under 1 indicated over-dispersion (lower precision) and over 1 under-dispersion (higher precision).", sep='')
      }
    } # end of if for inPMCID
    text # return
  })
  
  # download the table data extracted from the XML file from PubMed Central
  output$download <- downloadHandler(
    filename = function(){paste('table_data_', input$pmcid, '.xlsx', sep='')},  # add PMCID to Excel filename
    content = function(fname){
      inPMCID <- input$pmcid
      if (inPMCID==''){table = NULL} # if no PMCID
      # long table
      table =  tstats()$original_table
      ## make long table into wide format
      # continuous
      tab_continuous = filter(table, statistic =='continuous') 
      t1 = filter(tab_continuous, column == 1) %>%
        select(row, sample_size, stat1, stat2) %>%
        rename('n1' = 'sample_size',
               'mean1' = 'stat1',
               'sd1' = 'stat2')
      t2 = filter(tab_continuous, column == 2) %>%
        select(row, sample_size, stat1, stat2) %>%
        rename('n2' = 'sample_size',
               'mean2' = 'stat1',
               'sd2' = 'stat2')
      continuous_stats = full_join(t1, t2, by='row') %>%
        rename('Name' = 'row') %>%
        mutate(Name = paste('C', Name, sep=''))
      #
      tab_percent = filter(table, statistic =='percent') 
      t1 = filter(tab_percent, column == 1) %>%
        select(row, stat1, sample_size) %>%
        rename('n1' = 'stat1',
               'N1' = 'sample_size')
      t2 = filter(tab_percent, column == 2) %>%
        select(row, stat1, sample_size) %>%
        rename('n2' = 'stat1',
               'N2' = 'sample_size')
      percent_stats = full_join(t1, t2, by='row') %>%
        rename('Name' = 'row') %>%
        mutate(Name = paste('P', Name, sep=''))
      # output to Excel
      yellow <- createStyle(fontColour = "black", fgFill = "#FFFF00") # colours to match template
      green <- createStyle(fontColour = "black", fgFill = "#C6E0B4")
      wb = createWorkbook(creator='Adrian Barnett')
      addWorksheet(wb, sheetName = "long")
      addWorksheet(wb, sheetName = "wide")
      freezePane(wb, sheet = 1, firstRow = TRUE) ## freeze first column
      writeDataTable(wb, sheet = 1, x = table,
                     colNames = TRUE, rowNames = FALSE)
      text = 'Excel data in wide format, can be used in the "Option 2" upload on the "baseline" shiny page'
      writeData(wb, sheet = 2, startRow = 1, x = text)
      text = 'Only enter values in the yellow cells, green cells are optional. Add more yellow cells if there are more rows in the table. Do not change any other cells. Summary statistics that are medians cannot be used.'
      writeData(wb, sheet = 2, startRow = 2, x = text)
      text = 'Continuous variables, group sample size (N), mean and standard deviation'
      writeData(wb, sheet = 2, startRow = 3, x = text)
      writeData(wb, sheet = 2, startRow = 4, startCol = 1, x = 'Name')
      writeData(wb, sheet = 2, startRow = 4, startCol = 2, x = 'N')
      writeData(wb, sheet = 2, startRow = 4, startCol = 3, x = 'Mean')
      writeData(wb, sheet = 2, startRow = 4, startCol = 4, x = 'SD')
      writeData(wb, sheet = 2, startRow = 4, startCol = 5, x = 'N')
      writeData(wb, sheet = 2, startRow = 4, startCol = 6, x = 'Mean')
      writeData(wb, sheet = 2, startRow = 4, startCol = 7, x = 'SD ')
      if(nrow(continuous_stats) > 0){
        writeData(wb, sheet = 2, startRow = 5, x = continuous_stats,
                  colNames = FALSE, rowNames = FALSE)
        rows = 5:(5+nrow(continuous_stats)-1)
        addStyle(wb, sheet =2, rows = rows, cols=2:7, gridExpand = TRUE, style = yellow)
        addStyle(wb, sheet =2, rows = rows, cols=1, gridExpand = TRUE, style = green)
      }
      text = 'Numbers or percents, numerator (n) and denominator (N).'
      writeData(wb, sheet = 2, startRow = 3 + nrow(continuous_stats) + 2, x = text)
      # add percents
      writeData(wb, sheet = 2, startRow = 3 + nrow(continuous_stats) + 3, startCol = 1, x = 'Name')
      writeData(wb, sheet = 2, startRow = 3 + nrow(continuous_stats) + 3, startCol = 2, x = 'n')
      writeData(wb, sheet = 2, startRow = 3 + nrow(continuous_stats) + 3, startCol = 3, x = 'N')
      writeData(wb, sheet = 2, startRow = 3 + nrow(continuous_stats) + 3, startCol = 4, x = 'n')
      writeData(wb, sheet = 2, startRow = 3 + nrow(continuous_stats) + 3, startCol = 5, x = 'N')
      if(nrow(percent_stats) > 0){
        writeData(wb, sheet = 2, startRow = 3 + nrow(continuous_stats) + 4, x = percent_stats,
                  colNames = FALSE, rowNames = FALSE)
        start = 3 + nrow(continuous_stats) + 4
        rows = start:(start+nrow(percent_stats)-1)
        addStyle(wb, sheet =2, rows = rows, cols=1, gridExpand = TRUE, style = green)
        addStyle(wb, sheet =2, rows = rows, cols=2:5, gridExpand = TRUE, style = yellow)
      }
      #
      saveWorkbook(wb, fname, overwrite = TRUE)
      
    }
  )
  
  
  # draw the histogram of t-statistics
  output$distPlot <- renderPlot({
    
    inPMCID <- input$pmcid
    inFile <- input$excel.file
    if (is.null(inFile)==TRUE & inPMCID=='' | is.null(tstats()$tstats)){tplot = NULL} # stop here if no file
    if (is.null(inFile)==FALSE | inPMCID!='' & !is.null(tstats()$tstats)){ # must be some t-statistics
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
      tplot = ggplot(data=tstats, aes(x=t, linewidth=1, colour=factor(study))) +
        theme_bw()+
#        scale_size_manual(values = sizes)+ # kept all lines the same size
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