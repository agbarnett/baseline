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
                      a(href="https://f1000research.com/articles/11-783/v1", "here"),
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
                         label = "Prior probability of under- or over-dispersion",
                         value = 0.5,
                         min = 0.01,
                         max = 0.99),
            
            h5('The default probability of 0.5 means it is equally likely that there is or is not under or over-dispersion.'),
            
            # simulations
            numericInput(inputId ="n.sims", 
                      label = "Number of simulations",
                      value = 20,
                      min = 1,
                      max = 200)
            
        ),
            
        # Show a plot of the t-statistics
        mainPanel(
        #    
           h4("Cumulative density functions (CDFs)"),
           h5("The trial CDF is in red and the simulated trial CDFs in grey. The simulated trials are generated following the null hypothesis of no dispersion. The median of the simulations is in blue."),
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
        h4("An example of an under-dispersed trial:"),
        div(style="display:inline-block;",img(src="https://raw.githubusercontent.com/agbarnett/baseline/master/example_under.jpg", height = 400, width = 500, style="left;")),
     br(),
        
        h4("An example with no issues:"),
     div(style="display:inline-block;",img(src="https://raw.githubusercontent.com/agbarnett/baseline/master/example_fine.jpg", height = 400, width = 500, style="left;")),
     br(),   
     
        h4("An example of an over-dispersed trial:"),
     div(style="display:inline-block;",img(src="https://raw.githubusercontent.com/agbarnett/baseline/master/example_over.jpg", height = 400, width = 500, style="left;")),
    br(),
        
        
    p(tags$a(href='mailto:a.barnett@qut.edu.au', 'Email'), ' me (Adrian) if you find a bug or have any ideas for improvements.', sep='')
    )
    
))

