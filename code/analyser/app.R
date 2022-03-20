#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)
library(svglite)
library(ggthemr)

ggthemr('solarized')

not_sel <- "Not Selected"
seq_length <- "length"
tm_gc <- "TM_GC"
gc <- "GC"
sam_name <- "Sample"
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
      selectInput("num_var_1", "Numerical feature Variable", choices = c(not_sel)),
      selectInput("num_var_2", "Length", choices = c(not_sel)),
      selectInput("sample_name", "Sample name", choices = c(not_sel)),
      # sliderInput("lenrange", "Range:",
      #             min=0, max=1000, value = c(200,500)),
      br(),
      actionButton("run_button", "Run Analysis", icon = icon("play"))
    ),
    
    #####MAIN PANEL#####
    mainPanel( 
      width = 9,
      tabsetPanel(
        tabPanel(
          title = "Plots",
          downloadButton('downloadPlot', 'Download Plot'),
          fluidRow(
            column(12,
                   fluidRow(
                     column(6, plotOutput("plot_1")),
                     column(6, plotOutput("plot_2"))),
                   fluidRow(
                     column(6, plotOutput("plot_3")),
                     column(6, plotOutput("plot_4"))
                   )
            ))),
        tabPanel(
          title = "GC Plots"
        ),
        tabPanel(
          title = "Tm_GC Plots"
        ),
        tabPanel(
          title = "Length Plots"
        ),
        tabPanel(
          title = "Promoters"
        ),
        tabPanel(
          title = "Terminators"
        ),
        tabPanel(
          title = "Statistics",
          fluidRow(
            column(width = 4, strong(textOutput("num_var_1_title"))),
            column(width = 4, strong(textOutput("num_var_2_title")))
          ),
          fluidRow(
            column(width = 4, tableOutput("num_var_1_summary_table")),
            column(width = 4, tableOutput("num_var_2_summary_table"))
          ),
          #downloadButton('downloadData', 'Download')
        )
      )
    )
  )
)


#################DRAW PLOTS (VIOLIN, BOXPLOT, HISTOGRAM)

draw_plot_1 <- function(data_input, num_var_1, num_var_2, sample_name){
  if(num_var_1 != not_sel & sample_name != not_sel){
    ggplot(data = data_input,
           aes_string(x = sample_name, y = num_var_1, fill=sample_name)) +
      geom_violin()
  }
}

draw_plot_2 <- function(data_input, num_var_1, num_var_2, sample_name){
  if(num_var_1 != not_sel & num_var_2 != not_sel & sample_name != not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_1, y = num_var_2, fill=sample_name )) + 
      geom_boxplot()
  }
}

draw_plot_3 <- function(data_input, num_var_1,  sample_name){ 
  if(num_var_1 != not_sel ){
    ggplot(data = data_input,
           aes_string(x = num_var_1, fill=sample_name)) +
      geom_histogram()
  }
}
draw_plot_4 <- function(data_input, num_var_2, sample_name){ 
  if(num_var_2 != not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_2, fill=sample_name)) +
      geom_histogram()
  }
  
}

##############Statiscits tables###########################
create_num_var_table <- function(data_input, num_var){
  if(num_var != not_sel){
    col <- data_input[,get(num_var)]
    if (length(col)>5000) col_norm <- sample(col,5000) else col_norm <- col
    norm_test <- shapiro.test(col_norm)
    statistic <- c("mean", "median")
    value <- c(round(mean(col),2), round(median(col),2))
    data.table(statistic, value)
  }
}

#####UI navigation############

ui <- navbarPage(
  title = "sRNA Analyser",
  theme = shinytheme('cerulean'),
  main_page,
  about_page
)
##############server###########

server <- function(input, output, session){
  
  options(shiny.maxRequestSize=10*1024^2) 
  #####data input, only csv filet########
  data_input <- reactive({
    ext <- tools::file_ext(input$csv_input$datapath)
    req(input$csv_input)
    validate(need(ext == "csv", "Please upload a csv file"))
    fread(input$csv_input$datapath)
  })
  
  ############select and update inputs###########
  
  observeEvent(data_input(),{
    choices <- c(not_sel, names(data_input()))
    updateSelectInput(inputId = "num_var_1", choices = choices)
    updateSelectInput(inputId = "num_var_2", choices = choices)
    updateSelectInput(inputId = "sample_name", choices = choices)
    #updateSliderInput(session, inputId = "lenrange", min=min(data$length), max=max(data$length))
  })
  
  #########slider#####
  
  
  num_var_1 <- eventReactive(input$run_button,input$num_var_1)
  num_var_2 <- eventReactive(input$run_button,input$num_var_2)
  sample_name <-eventReactive(input$run_button,input$sample_name)
  #lenrange <-eventReactive(input$run_button, input$lenrange)
  
  #### plots output#############
  
  output$plot_1 <- renderPlot(.plot_1())
  
  .plot_1 <- reactive(
    draw_plot_1(data_input(), num_var_1(), num_var_2(), sample_name())
  )
  
  output$plot_2 <- renderPlot(.plot_2())
  
  .plot_2 <- reactive(
    draw_plot_2(data_input(), num_var_1(), num_var_2(), sample_name())
  )
  
  output$plot_3 <- renderPlot(.plot_3())
  
  .plot_3 <- reactive(
    draw_plot_3(data_input(), num_var_1(), sample_name())
  )
  
  output$plot_4 <- renderPlot(.plot_4())
  
  .plot_4 <- reactive(
    draw_plot_4(data_input(), num_var_2(), sample_name())
  )
  
  plotInput1 = function(){
    .plot_1()
  }
  plotInput2 = function(){
    .plot_2()
  }
  plotInput3 = function(){
    .plot_3()
  }
  plotInput4 = function(){
    .plot_4()
  }
  
  
  ############download result files#################
  output$downloadPlot <- downloadHandler(
    filename = function() {"Plots.pdf"},
    content = function(file){
      pdf(file)
      print(.plot_1())
      print(.plot_2())
      print(.plot_3())
      print(.plot_4())
      dev.off()
    }
  )
  
  #############statistics#####################################
  
  output$num_var_1_title <- renderText(paste("Num Var 1:",num_var_1()))
  
  num_var_1_summary_table <- eventReactive(input$run_button,{
    create_num_var_table(data_input(), num_var_1())
  })
  
  output$num_var_1_summary_table <- renderTable(num_var_1_summary_table(),colnames = FALSE)
  
  output$num_var_2_title <- renderText(paste("Num Var 2:",num_var_2()))
  
  num_var_2_summary_table <- eventReactive(input$run_button,{
    create_num_var_table(data_input(), num_var_2())
  })
  
  output$num_var_2_summary_table <- renderTable(num_var_2_summary_table(),colnames = FALSE)
  
  
}
create_num_var_table <- function(data_input, num_var, sample_name){
  if(num_var != not_sel){
    col <- data_input[,get(num_var)]
    if(length(col)>5000) col_norm<-sample(col,5000) else col_norm<-col
    norm_test <- shapiro.test(col_norm)
    statistic <- c("mean", "median")
    value <- c(round(mean(col),2), round(median(col),2))
    data.table(statistic, value)
  }
}



# Run the application 
shinyApp(ui = ui, server = server)

