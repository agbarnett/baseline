# server.R
# Shiny app to run checks of baseline tables for trials
# January 2022, updated February 2025

shinyServer(function(input, output) {
  
  # reactive function to get data from Excel file and create t-statistics
  tstats = reactive({
    
    reason = NULL # reason it went wrong
    
    # PMCID & excel file
    inPMCID <- input$pmcid
    inFile <- input$excel.file
    
    # is it Excel or PMID
    if (is.null(inFile)==TRUE & inPMCID==''){return(NULL)} # stop here if no file or PMCID
    if (is.null(inFile)==FALSE & inPMCID==''){ # Excel
      # progress message & get data
      withProgress(message = 'Processing the data from Excel',
                   value = 0,{
                     incProgress(0.1)
                     data = my_read_excel(inFile$datapath)
                     incProgress(1)
                   })
      
      data$original_table = NULL # to match structure for PMC version
      
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
        tstats = filter(tstats, !row %in% to_remove$row) %>%
          mutate(row = as.numeric(as.factor(row))) # update row number
        # remove from data
        data$data = filter(data$data, !(statistic=='percent' & row %in% to_remove$row))
        data$data = mutate(data$data, row = as.numeric(as.factor(row))) # re-number rows
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
                 study = k+1) # dummy study number for plotting
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
                     results = run_bayes_test(in_data = for_model, p_prior = input$prior) # from 99_functions.R
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
  
  
  ## plot the cumulative density or histogram of t-statistics ##
  output$distPlot <- renderPlot({
    
    inPMCID <- input$pmcid
    inFile <- input$excel.file
    if (is.null(inFile)==TRUE & inPMCID=='' | is.null(tstats()$tstats)){tplot = NULL} # stop here if no file
    if (is.null(inFile)==FALSE | inPMCID!='' & !is.null(tstats()$tstats)){ # must be some t-statistics
      n.sims = input$n.sims
      style <- input$graph.style
      tstats = tstats()$tstats
      df = tstats$size[1]-2 # degrees of freedom for t-distribution
      
      ## draw the summary of the t-statistics ##
      
      if(style == 'histogram'){
        
        #
        study_only = filter(tstats, study == 1)
        
        # define a minimum range for the x-axis
        xlimits = rep(0,2)
        xlimits[1] = min(c(-2, min(tstats$t)))
        xlimits[2] = max(c(2, max(tstats$t)))
        
        # plot
        tplot = ggplot(data=study_only, aes(x = t)) +
          geom_histogram(aes(y=..density..), fill='grey44')+ # observed data
#          geom_density(col='grey44')+ # alternative
          geom_function(fun = dt, args = list(df=10), colour='blue')+ # smooth density with zero mean
          xlab('t-statistic')+
          ylab('Probability density function')+
          scale_x_continuous(limits = xlimits)+
          theme_bw()+
          theme(text = element_text(size=14),
                panel.grid.minor = element_blank())
      }
      
      if(style == 'cumulative'){  
        
        ## STUCK HERE, WHAT IS GOING ON WITH CDF IN WRONG DIRECTION!
        
        ## create the mean as a summary ##
        # create all CDFs for all simulations
        all_cdfs = filter(tstats, study > 1) %>% # just simulations
          group_by(study) %>%
          mutate(cdf = ecdf(t)(t)) %>% # CDF per study
          ungroup() 
        # smooth cdf based on ideal t-distribution
        ts = filter(all_stats, study > 1) %>% pull(t) # just use simulated t-values 
        cdf_smooth = data.frame(t = ts) %>%
          arrange(t) %>%
          mutate(cdf = pt(t, df = df, lower.tail=TRUE)) 
        
        # now calculate mean CDF of simulations - is not always monotonically increasing
        #cdf_mean = group_by(all_cdfs, cdf) %>%
        #  summarise(t = mean(t)) %>%
        #  ungroup() %>%
        #  mutate(study = 1) # had to provide study number, same as study 1 to get linewidth
        # add first point of the CDF (where it hits the y-axis)
        cdf_first = cdf_smooth[1,] %>% 
          mutate(cdf = 0)
        cdf_last = cdf_smooth[nrow(cdf_smooth),] %>% 
          mutate(cdf = 1)
        cdf_smooth = bind_rows(cdf_first, cdf_smooth, cdf_last)
        
        # set up different colour and size for trial; move trial to last
        tstats = mutate(tstats, study = ifelse(study==1, 999, study))
        colours = grey(runif(n = n.sims + 1, min=0.5, max=0.9)) # grey colours for simulations
        colours[n.sims + 1] = 'indianred1' # colour for trial
        sizes = rep(0.5, n.sims + 1) # thin lines
        sizes[n.sims + 1] = 1 # mean and trial are larger
        # plot
        tplot = ggplot(data=tstats, aes(x=t, linewidth=factor(study, ordered = TRUE), colour=factor(study, ordered = TRUE))) +
          stat_ecdf()+
          scale_linewidth_manual(values = sizes)+
          scale_color_manual(values = colours)+
          geom_step(data = cdf_smooth, aes(x=t, y=cdf), linewidth=1, col='dodgerblue')+ # mean CDF
          xlab('t-statistic')+
          ylab('Cumulative density')+
          theme_bw()+
          theme(text = element_text(size=14),
                legend.position = 'none',
                panel.grid.minor = element_blank(),
                panel.grid.major = element_line(colour="lightcyan")) # to avoid clash with grey of simulations
      } # end of style if
    } 
    
    tplot
  })
  
  ## which text to use for plot
  output$whichPlot <- renderText({
    if(input$graph.style=='cumulative'){text = "The trial CDF is in red and the simulated trial CDFs in grey. The simulated trials are generated following the null hypothesis of no dispersion. An ideal distribution is in blue."}
    if(input$graph.style=='histogram'){text = 'The observed histogram and the expected probability density for a t-distribution.'}
    text
  })
  
  ## which title to use for plot
  output$whichPlotTitle <- renderText({
    if(input$graph.style=='cumulative'){title = "Cumulative density functions (CDFs)."}
    if(input$graph.style=='histogram'){title = 'Histogram and probability density function (PDF).'}
    title
  })
  
}
)