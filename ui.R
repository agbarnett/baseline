# ui.R
# Shiny app to run under- and over-dispersion checks of baseline tables for trials
# January 2022

# 
shinyUI(fluidPage(

    # Application title
    titlePanel("Detecting under- or over-dispersion in a baseline table from a randomised controlled trial"),
    # add text with links to examples
    div(p(HTML(paste0('To run the checks upload an Excel spreadsheet with the summary statistics for the two randomised groups. An example spreadsheet is ', 
                      a(href="https://raw.githubusercontent.com/agbarnett/baseline/master/example_data/excel_example.xlsx", "here"),
                      ' and a blank template is ', 
                      a(href="https://raw.githubusercontent.com/agbarnett/baseline/master/excel_template.xlsx", "here"),'.')))),
    
    # Sidebar to read in Excel file
    sidebarLayout(
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput(inputId ="excel.file", 
                      label = "Upload Excel file",
                      multiple = FALSE,
                      accept = c(".xlsx")),
            
            # Input: Select a file ----
            numericInput(inputId ="n.sims", 
                      label = "Number of simulations",
                      value = 20,
                      min = 1,
                      max = 200),
            
            h5('More simulations will mean the model will need longer to run.')
            
        ),
            
        # Show a plot of the t-statistics
        mainPanel(
        #    
           h4("Cumulative density functions (CDFs)"),
           h5("The trial CDF is in red and the simulated trial CDFs in grey. The simulated trials are generated following the null hypothesis of no dispersion. A median of the simulations is in blue."),
           plotOutput("distPlot"),
           #
           h4("Bayesian model results:"),
           textOutput("testRes")
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
        
        
        h4("Under construction."),
    p("Email ", tags$a(href='mailto:a.barnett@qut.edu.au', 'e-mail'), ' me (Adrian) if you find a bug or have any ideas for improvements.', sep='')
    )
    
))

