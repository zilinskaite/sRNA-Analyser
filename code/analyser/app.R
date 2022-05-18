#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

install.packages("shiny")
install.packages("shinythemes")
install.packages("data.table")
install.packages("ggplot2")
install.packages("devtools", dependencies = TRUE, INSTALL_opts = '--no-lock')
#install.packages("ggthemr", dependencies = TRUE, INSTALL_opts = '--no-lock')
devtools::install_github('Mikata-Project/ggthemr')
install.packages("DT", dependencies = TRUE, INSTALL_opts = '--no-lock')
library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)
library(ggthemr)
library(DT)

ggthemr("solarized")

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
      fileInput("csv_input", "Select CSV File to Import", accept = ".csv", multiple = TRUE),
      selectInput("sample_name", "Sample name", choices = c(not_sel)),
      selectInput("GC_input", "GC", choices = c(not_sel)),
      selectInput("TmGC_input", "Tm_GC", choices = c(not_sel)),
      selectInput("len_input", "Length2", choices = c(not_sel)),
      # sliderInput("lenrange", "Range:",
      #             min=0, max=1000, value = c(200,500)),
      br(),
      actionButton("run_button", "Run Analysis", icon = icon("play")),
      downloadButton('downloadPlot', 'Download Plot')
    ),
    
    #####MAIN PANEL#####
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          title = "GC Plots",
          fluidRow(
            column(12,
                   fluidRow(
                     column(6, plotOutput("plot_GC1")),
                     column(6, plotOutput("plot_GC2")),
                     column(6, plotOutput("plot_GC3")))
            ))),
        tabPanel(
          title = "Tm_GC Plots",
          fluidRow(
            column(12,
                   fluidRow(
                     column(6, plotOutput("plot_TmGC1")),
                     column(6, plotOutput("plot_TmGC2")),
                     column(6, plotOutput("plot_TmGC3")))
            ))),
        tabPanel(
          title = "Length Plots",
          fluidRow(
            column(12,
                   fluidRow(
                     column(6, plotOutput("plot_len1")),
                     column(6, plotOutput("plot_len2")))
            ))),
        tabPanel(
          title = "Promoters"
        ),
        tabPanel(
          title = "Terminators"
        ),
        # tabPanel(
        # title = "Statistics",
        #fluidRow(
        #  column(width = 4, strong(textOutput("num_var_1_title"))),
        #  column(width = 4, strong(textOutput("num_var_2_title")))
        #),
        #fluidRow(
        #  column(width = 4, tableOutput("num_var_1_summary_table")),
        #  column(width = 4, tableOutput("num_var_2_summary_table"))
        #),
        #  DTOutput("data")
        #downloadButton('downloadData', 'Download')
        #)
      )
    )
  )
)


#################DRAW PLOTS (VIOLIN, BOXPLOT, HISTOGRAM)

###############GC###################
draw_plot_GC1 <- function(data_input, GC_input, len_input, sample_name){
  if(GC_input != not_sel & sample_name != not_sel){
    ggplot(data = data_input,
           aes_string(x = sample_name, y = GC_input, fill=sample_name)) +
      geom_violin()
  }
}

draw_plot_GC2 <- function(data_input, GC_input, len_input, sample_name){
  if(GC_input != not_sel & len_input != not_sel & sample_name != not_sel){
    ggplot(data = data_input,
           aes_string(x = GC_input, y = len_input, fill=sample_name )) +
      geom_boxplot()
  }
}

draw_plot_GC3 <- function(data_input, GC_input,  sample_name){
  if(GC_input != not_sel & sample_name != not_sel){
    ggplot(data = data_input,
           aes_string(x = GC_input, fill=sample_name)) +
      geom_histogram()
  }
}

###############Tm_GC###################
draw_plot_TmGC1 <- function(data_input, TmGC_input, len_input, sample_name){
  if(TmGC_input != not_sel & sample_name != not_sel){
    ggplot(data = data_input,
           aes_string(x = sample_name, y = TmGC_input, fill=sample_name)) +
      geom_violin()
  }
}

draw_plot_TmGC2 <- function(data_input, TmGC_input, len_input, sample_name){
  if(TmGC_input != not_sel & len_input != not_sel & sample_name != not_sel){
    ggplot(data = data_input,
           aes_string(x = TmGC_input, y = len_input, fill=sample_name )) +
      geom_boxplot()
  }
}

draw_plot_TmGC3 <- function(data_input, TmGC_input,  sample_name){
  if(TmGC_input != not_sel & sample_name != not_sel){
    ggplot(data = data_input,
           aes_string(x = TmGC_input, fill=sample_name)) +
      geom_histogram()
  }
}

#######length#####################
draw_plot_len1 <- function(data_input,  len_input, sample_name){
  if(len_input != not_sel & sample_name != not_sel){
    ggplot(data = data_input,
           aes_string(x = sample_name, y = len_input, fill=sample_name)) +
      geom_violin()
  }
}

draw_plot_len2 <- function(data_input, len_input, sample_name){
  if(len_input != not_sel){
    ggplot(data = data_input,
           aes_string(x = len_input, fill=sample_name)) +
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
    #ext <- tools::file_ext(input$csv_input$datapath)
    #req(input$csv_input)
    #validate(need(ext == "csv", "Please upload a csv file"))
    rbindlist(lapply(input$csv_input$datapath, fread),
              use.names = TRUE, fill = TRUE)
  })
  #  output$data <- renderDT(raw_data, options = list(lengthChange = FALSE))
  
  
  ############select and update inputs###########
  
  observeEvent(data_input(),{
    choices <- c(not_sel, names(data_input()))
    updateSelectInput(inputId = "sample_name", choices = choices)
    updateSelectInput(inputId = "GC_input", choices = choices)
    updateSelectInput(inputId = "TmGC_input", choices = choices)
    updateSelectInput(inputId = "len_input", choices = choices)
    #updateSliderInput(session, inputId = "lenrange", min=min(data$length), max=max(data$length))
  })
  
  #########slider#####
  sample_name <-eventReactive(input$run_button,input$sample_name)
  GC_input <- eventReactive(input$run_button,input$GC_input)
  TmGC_input <- eventReactive(input$run_button,input$TmGC_input)
  len_input <-eventReactive(input$run_button,input$len_input)
  
  #lenrange <-eventReactive(input$run_button, input$lenrange)
  
  #### plots output#############
  ##############GC######################
  output$plot_GC1 <- renderPlot(.plot_GC1())
  
  .plot_GC1 <- reactive(
    draw_plot_GC1(data_input(), GC_input(), len_input(), sample_name())
  )
  
  output$plot_GC2 <- renderPlot(.plot_GC2())
  
  .plot_GC2 <- reactive(
    draw_plot_GC2(data_input(), GC_input(), len_input(), sample_name())
  )
  output$plot_GC3 <- renderPlot(.plot_GC3())
  
  .plot_GC3 <- reactive(
    draw_plot_GC3(data_input(), GC_input(), sample_name())
  )
  
  ##############Tm_GC######################
  output$plot_TmGC1 <- renderPlot(.plot_TmGC1())
  
  .plot_TmGC1 <- reactive(
    draw_plot_TmGC1(data_input(), TmGC_input(), len_input(), sample_name())
  )
  
  output$plot_TmGC2 <- renderPlot(.plot_TmGC2())
  
  .plot_TmGC2 <- reactive(
    draw_plot_TmGC2(data_input(), TmGC_input(), len_input(), sample_name())
  )
  
  output$plot_TmGC3 <- renderPlot(.plot_TmGC3())
  
  .plot_TmGC3 <- reactive(
    draw_plot_TmGC3(data_input(), TmGC_input(), sample_name())
  )
  ################len###################
  output$plot_len1 <- renderPlot(.plot_len1())
  
  .plot_len1 <- reactive(
    draw_plot_len1(data_input(), len_input(), sample_name())
  )
  
  output$plot_len2 <- renderPlot(.plot_len2())
  
  .plot_len2 <- reactive(
    draw_plot_len2(data_input(), len_input(), sample_name())
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
  ######################GC#################
  plotInputGC1 = function(){
    .plot_GC1()
  }
  plotInputGC2 = function(){
    .plot_GC2()
  }
  plotInputGC3 =  function(){
    .plot_GC3()
  }
  
  ######################Tm_GC#################
  plotInputTmGC1 = function(){
    .plot_TmGC1()
  }
  plotInputTmGC2 = function(){
    .plot_TmGC2()
  }
  plotInputTmGC3 = function(){
    .plot_TmGC3()
  }
  
  #####################len################
  plotInputlen1 = function(){
    .plot_len1()
  }
  plotInputlen2 = function(){
    .plot_len2()
  }
  
  ############download result files#################
  output$downloadPlot <- downloadHandler(
    filename = function() {"Plots.pdf"},
    content = function(file){
      pdf(file)
      print(.plot_GC1)
      print(.plot_GC2)
      print(.plot_GC3)
      print(.plot_TmGC1)
      print(.plot_TmGC2)
      print(.plot_TmGC3)
      print(.plot_len1)
      print(.plot_len2)
      dev.off()
    }
  )
  
}



# Run the application
shinyApp(ui = ui, server = server)

