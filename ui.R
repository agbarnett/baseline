# ui.R
# Shiny app to run checks of baseline tables for trials
# January 2022

# 
shinyUI(fluidPage(

    # Application title
    titlePanel("Detecting potential problems in a baseline table from a randomised controlled trial"),
    # add text with links to examples
    div(p(HTML(paste0('To run the checks upload an Excel spreadsheet with the summary statistics for the two randomised groups. An example spreadsheet is ', 
                      a(href="https://github.com/agbarnett/baseline/blob/master/excel_example.xlsx", "here"),
                      ' and a blank template is ', 
                      a(href="https://github.com/agbarnett/baseline/blob/master/excel_template.xlsx", "here"),'.')))),
    
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
            
            h5('More simulations will take more time for the results to appear.')
            
        ),
            
        # Show a plot of the t-statistics
        mainPanel(
        #    
           h4("Cumulative density plots"),
           h5("The plot shows the trial in red and the simulated trials in grey."),
           plotOutput("distPlot"),
           #
           h4("Model results:"),
           textOutput("testRes")
        )
    ),
    
    # to do, report basic statistics about the table
    # cat("The table had x rows with x continuous statistics and y percentage statistics.')
    
    # footnotes
    mainPanel(
        p(" \n"),
        p("Under construction."),
    p("Email ", tags$a(href='mailto:a.barnett@qut.edu.au', 'e-mail'), ' me (Adrian) if you find a bug or have any ideas for improvements.', sep='')
    )
    
))


# Run the application 
#shinyApp(ui = ui, server = server)
