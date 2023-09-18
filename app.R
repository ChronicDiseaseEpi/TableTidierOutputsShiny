#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(writexl)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Simple conversion of TableTidier json outputs"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
          h1("Upload file"),
            fileInput(inputId = "myfile", label = "Uploaded json file", accept = ".json"),
          h2("Download all data and terminology to an excel (xlsx) file"),
          shiny::downloadButton(outputId = "download_all", label = "Download all tables"),
          h2("Or select a table and view individually in Data and Terminology tabs"),
          p("Can subsequently filter and download data for these tables in a variety of formats"),
          numericInput("n_slct", "Select Table Number", min = 1L, max = 999, step = 1L, value = 2)
        ),

        # Show a plot of the generated distribution
        mainPanel(tabsetPanel(
          tabPanel("Overview",
                   h2("All tables in collection"),
                   dataTableOutput(outputId = "info"),
                   dataTableOutput(outputId = "notes")),
          tabPanel("Data", dataTableOutput(outputId = "table_data")),
          tabPanel("Terminology", dataTableOutput(outputId = "table_term")),
          tabPanel("Data and terminology", 
                   dataTableOutput(outputId = "table_data_term"),
                   shiny::actionLink("separate_button", "Separate values to columns"))
        )
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  clctn <-    reactive({
    file <- input$myfile
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == ".json", "PLease upload a json file"))
    a <- ReadCollection(file$datapath)
    a 
    })
  
  mytbl <- reactive( {
     clctn()[[input$n_slct]]
  }
    )
  
  output$notes <- renderDataTable(ConvertNotes(clctn()) %>% 
                                mutate(tid = str_sub(tid, 4)))
  output$info <- renderDataTable(ConvertInfo(clctn()))
  
 output$table_data <- renderDataTable(
   ConvertData(mytbl()),
   server = FALSE,
   editable = TRUE,
   extensions = 'Buttons', 
   options = list(dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))

 output$table_term <- renderDataTable(
   ConvertTerminology(mytbl()),
   extensions = 'Buttons', 
   options = list(dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
 
 output$table_data_term <- renderDataTable(
   ConvertDataTerm(mytbl()),
   extensions = 'Buttons', 
   options = list(dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
 
 output$download_all <- downloadHandler(
   filename = function() {
     paste("data_", Sys.Date(), ".xlsx", sep="")
   },
   content = function(file) {
     mydata <- map(clctn(), ConvertDataTerm)
     mydata$terminology <- map(clctn(), ConvertTerminology) %>% 
       bind_rows(.id = "tid") %>% 
       mutate(tid = str_sub(tid, 4))
     mydata$info <- ConvertInfo(clctn())
     mydata$notes <- ConvertNotes(clctn()) %>% 
       mutate(tid = str_sub(tid, 4))
     write_xlsx(mydata[c("info","notes","terminology", 
                         setdiff(names(mydata),
                                 c("info", "notes","terminology")))],
                file)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
