# ui.R
# Shiny app to run checks of baseline tables for trials
# January 2022


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Detecting potential problems in a baseline table from a randomised controlled trial"),
    # add text with links to examples
    div(p(HTML(paste0('To run the checks upload an Excel spreadsheet with the summary statistics for the two randomised groups. An example spreadsheet is ', a(href="https://www.crossref.org/", "here"),' and a blank template is ', a(href="https://www.crossref.org/", "here"),'.')))),
    
    # Sidebar to read in Excel file
    sidebarLayout(
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput(inputId ="excel.file", 
                      label = "Upload Excel file",
                      multiple = FALSE,
                      accept = c(".xlsx"))
        ),
            
        # Show a plot of the t-statistics
        mainPanel(
           plotOutput("distPlot"),
           textOutput("testRes")
        )
    ),
    
    # footnotes
    mainPanel("Under construction."),
    
))


# Run the application 
#shinyApp(ui = ui, server = server)
