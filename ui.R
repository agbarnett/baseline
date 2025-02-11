# ui.R
# Shiny app to run under- and over-dispersion checks of baseline tables for trials
# May 2023

# 
shinyUI(fluidPage(

    # Application title
    titlePanel("Detecting under- or over-dispersion in a baseline table from a randomised controlled trial"),
    # add text with links to examples
    div(p(HTML(paste0('To run the checks, either 1) enter a PubMed Central ID (PMCID) from a published randomised trial, e.g, PMC5640030, and our automated algorithm will attempt to extract the baseline table, or 2) upload an Excel spreadsheet with the summary statistics for the two randomised groups. An example spreadsheet is ', 
                      a(href="https://raw.githubusercontent.com/agbarnett/baseline/master/example_data/excel_example.xlsx", "here"),
                      ' and a blank template is ', 
                      a(href="https://raw.githubusercontent.com/agbarnett/baseline/master/excel_template.xlsx", "here"),
                      '. A paper with details on the approach is ', 
                      a(href="https://f1000research.com/articles/11-783/v2", "here"),
                      '.')))),
    
    # Sidebar to read in Excel file
    sidebarLayout(
        sidebarPanel(
            
			# Input: number ----
            textInput(inputId ="pmcid", 
                      value = '',
                      label = "Option 1: Enter PMCID",
					  placeholder = 'PMCxxxxxxx'),
			h5('example: PMC5640030',style="display:inline-block"), # add example

            # Input: Select a file ----
            fileInput(inputId ="excel.file", 
                      label = "Option 2: Upload Excel file",
                      multiple = FALSE,
                      accept = c(".xlsx")),
            
            # prior
            numericInput(inputId ="prior", 
                         label = "Prior probability of dispersion (either over- or under-)",
                         value = 0.5,
                         min = 0.01,
                         max = 0.99),
            
            h5('The default probability of 0.5 means it is equally likely that there is or is not under or over-dispersion.'),
            
            # simulations
            numericInput(inputId ="n.sims", 
                      label = "Number of simulations",
                      value = 20,
                      min = 1,
                      max = 200),
      			
			      # choice of output, histogram or cumulative density
      			selectInput(inputId = "graph.style", 
      			            label = "Plot the cumulative density or histogram",
      			            choices = c("Cumulative density" = "cumulative",
			                              "Histogram" = "histogram"),
      			            selected = "cumulative")
            
        ),
            
        # Show a plot of the t-statistics
        mainPanel(
        #    
           h4(textOutput("whichPlotTitle")),
           textOutput("whichPlot"),
           plotOutput("distPlot"),
           #
           h4("Error message (if any):"),
           span(textOutput("error"), style="color:red"),
           #
           h4("Bayesian model results:"),
           textOutput("testRes"),
           #
           h4("T-statistics for the baseline table:"),
           tableOutput("tableRes"),
           #
           h4("Download the baseline table if using Option 1:"),
           downloadButton(outputId = 'download', label = "Download")
        )
    ),
    
    # footnotes
    mainPanel(
        
      # examples from 1_run_example.R
      h3("Some notes on the plots"),
      p("The plots are designed to help interpret the results of the Bayesian model."),
      h4("An example of an under-dispersed trial that has been retracted for data fabrication (PMID: 9366938, first two columns of baseline table):"),
        div(style="display:inline-block;",img(src="https://raw.githubusercontent.com/agbarnett/baseline/master/example_under.jpg", height = 400, width = 500, style="left;")),
      p("Here the trial CDF (red line) is too thin with a sharp change in the cumulative distribution because too many t-statistics are close to zero. The probability that the trial is under- or over-dispersed is 0.99, with a precision multiplier of 8.6 (much larger than 1)."),
     br(),
        
        h4("An example with no issues (PMID: 25851385):"),
     div(style="display:inline-block;",img(src="https://raw.githubusercontent.com/agbarnett/baseline/master/example_fine.jpg", height = 400, width = 500, style="left;")),
     p("Here the trial CDF generally falls in the middle of the simulations and is close to the mean CDF (blue line). The probability that the trial is under- or over-dispersed is 0.08, with a precision multiplier of 0.97 (close to 1)."),
     br(),   
     
        h4("An example of an over-dispersed trial (PMID: 30510801):"),
     div(style="display:inline-block;",img(src="https://raw.githubusercontent.com/agbarnett/baseline/master/example_over.jpg", height = 400, width = 500, style="left;")),
     p("This trial has only five summary statistics and two are large negative t-statistics so the trial CDF is relatively far to the left. The probability that the trial is under- or over-dispersed is 0.94, with a precision multiplier of 0.21 (much smaller than 1)."),
     br(),
        
        
    p(tags$a(href='mailto:a.barnett@qut.edu.au', 'Email'), ' me (Adrian) if you find a bug or have any ideas for improvements.', sep='')
    )
    
))

