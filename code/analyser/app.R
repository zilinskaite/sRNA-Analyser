#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(ggthemr)

ggthemr('solarized')
################################################
##########MAIN PAGE UI#########################
###############################################
main_page <- tabPanel(
  title = "Analysis",
  titlePanel("Analysis"),
  
  ###SIDE BAR UI####
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      title = "Inputs",
      fileInput("csv_input", "Select CSV File to Import", accept = ".csv"),
      br(),
      actionButton("run_button", "Run Analysis", icon = icon("play"))
    ),
    
    #####MAIN PANEL#####
    mainPanel( 
      width = 9,
      tabsetPanel(
        tabPanel(
          title = "Plot",
          downloadButton('downloadPlot', 'Download Plot'),
          
        )
      )
    )
  )
)


################################################
##########ABOUT PAGE UI#########################
###############################################
about_page <- tabPanel(
  title = "About",
  titlePanel("About"),
  h4("Created with R Shiny"),
  h3("How to use this program?"),
  br(),
  p("On the navigation bar on the page top, you can find 'Analysis' and 'About' tabs. 
  Main application page is under 'Analysis' tab."),
  
  p("In 'Analysis' page you should be able to see button to import file.
  Important! Imported file needs to be .CSV format file."), 
  
  p("When you uploaded .CSV file, you can choose column names from uploaded file."), 
  strong("You can select:"),
  
  p("* Numeric feature variable"),
  
  p("* Length (it will be best if you choose column with your sequences length)."),
  
  p("* Sample Name (all sequences needs to have same Sample name for ID)."),
  
  p("After you chose everything - press 'Run Analysis' button."),
  p("After that, you will be able to download plots from Plot tab on the page right panel."),
  br(),
  h3("What files are acceptable for input?"),
  p("Application will accept only .CSV file as input."), 
  p("Also it will be the best if file contains numeric features variables, sequences length and sample ID."),
  h3("What files will be downloaded to your PC?"),
  p("After analysis is finished, you will be able to download result files."),
  p("Result files for visualization will be saved as .SVG files in .ZIP file."),
  "2021"
)

# Define UI for application that draws a histogram
ui <-navbarPage(
  title = "sRNA Analyser",
  theme = shinytheme('cerulean'),
  main_page,
  about_page
)



# Define server logic required to draw a histogram
server <- function(input, output) {
}

# Run the application 
shinyApp(ui = ui, server = server)
