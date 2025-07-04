library(shiny)
library(DT)
lapply(list.files("sources/"), \(x) source(paste0("sources/",x)))

ui <- navbarPage("ICC4IRR", # App title
                 tabpanel_home,
                 tabpanel_estIRR,
                 tabpanel_estQk,
                 tabpanel_about)


server <- function(input, output, session) {
  
  # Dataset 1 reactive
  data1 <- reactive({
    req(input$file1)
    ext <- tools::file_ext(input$file1$name)
    path <- input$file1$datapath
    
    if (ext == "csv") {
      read.csv(path)
    } else if (ext == "txt") {
      read.table(path, header = TRUE, sep = "", quote="\"")
    } else if (ext == "rds") {
      readRDS(path)
    } else {
      showNotification("Unsupported file type. Please upload .csv, .txt, or .rds.", type = "error")
      NULL
    }
  })
  
  # select col for summary (keep it?)
  output$col <- renderUI({
    req(data1())
    selectInput("col", "Select column(s) for summary statistics:", choices = names(data1()), 
                multiple = TRUE)
  })
  # base DT:: output for tables
  output$table1 <- DT::renderDataTable({
    req(data1())
    data1()
  }, options = list(scrollY = 500, paging = FALSE, searching = FALSE))
  
  # calculate summary statistics | var
  output$summary1 <- renderPrint({
    req(data1(), input$col)
    summary(data1()[, input$col, drop = FALSE])
  })
  ## -- ICC Arguments
  # subjects
  output$subject <- renderUI({
    req(data1())
    selectInput("subject", "Column name for subjects:", 
                choices = names(data1()), 
                multiple = FALSE)
  })
  # raters
  output$rater <- renderUI({
    req(data1())
    selectInput("rater", "Column name for raters:", 
                choices = names(data1()), 
                multiple = FALSE)
  })
  ## ratings
  output$Y <- renderUI({
    req(data1())
    selectInput("Y", "Column name for ratings:", 
                choices = names(data1()), 
                multiple = FALSE)
  })
  
  
# -------------------------------------------------------------------------
# Tab 2 -------------------------------------------------------------------
  
  # Dataset 2 reactive
  data2 <- reactive({
    req(input$file2)
    read.csv(input$file2$datapath)
  })
  
  output$cols_ui2 <- renderUI({
    req(data2())
    selectInput("cols2", "Select column(s) for summary:", choices = names(data2()), multiple = TRUE)
  })
  
  output$table2 <- renderTable({
    req(data2())
    head(data2(), 20)
  })
  
  output$summary2 <- renderPrint({
    req(data2(), input$cols2)
    summary(data2()[, input$cols2, drop = FALSE])
  })
  
}

shinyApp(ui, server)
