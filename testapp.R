library(shiny)
library(DT)
library(bslib)
lapply(list.files("sources/"), \(x) source(paste0("sources/",x)))

## MANUAL
source("ICCfunctions_tj.R")
ui <- navbarPage("ICC4IRR", # App title
                 theme = bs_theme(preset = "lux"),
                 #input_dark_mode(id = "mode"),
                 #tabpanel_home,
                 tabpanel_estIRR,
                 tabpanel_flow,
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
    selectInput("subject",
                label = HTML("Select column name for <b style='text-shadow: .5px .5px .5px rgba(0,0,0,0.4);'>subjects</b>:"),
                choices = names(data1()), 
                selected = "subject", ## only for development !! @todo
                multiple = FALSE)
  })
  # raters
  output$rater <- renderUI({
    req(data1())
    selectInput("rater", 
                label = HTML("Select column name for <b style='text-shadow: .5px .5px .5px rgba(0,0,0,0.4);'>raters</b>:"),
                choices = names(data1()), 
                selected = "rater", ## only for development !! @todo
                multiple = FALSE)
  })
  ## ratings
  output$Y <- renderUI({
    req(data1())
    selectInput("Y", 
                label = HTML("Select column name for <b style='text-shadow: .5px .5px .5px rgba(0,0,0,0.4);'>ratings</b>:"),
                choices = names(data1()), 
                selected = "Y", ## only for development !! @todo
                multiple = FALSE)
  })
  ## ##.... LONG - WIDE
  output$format <- renderUI({
    req(data1())
    selectInput("format", "Data Type:", 
                choices = c(Long = "long",
                            Wide = "wide",
                            `Plausible Values (PVs)` = "PVs"),
                multiple = FALSE)
  })
  
  output$estimatebutton <- renderUI({
    req(data1())
    actionButton("estimateICCs", "Estimate ICCs", icon = icon("cogs"))
                 #, 
                 #style = "color: white; background-color: #7c97f2; border-color: #7c97f2; 
                 #     border-radius: 6px; padding: 8px 16px; font-size: 16px; font-weight: bold;")
  })
  # model
  model <- eventReactive(input$estimateICCs, { ## BUTTON!!!
    # Analyses for Plausible values
    if(input$format == "PVs"){
      dataset <- data1()
      ICCsests <- estICC_PVs(dataset, subjects = input$subject, raters = input$rater,
                             k = input$k, khat = input$khat, Q = input$Q)
      
    } else {  req(data1())
      # Analyses for Observed variables
      if(input$format == "wide") {
        dataset <- reshape(data = data1(), # wide-format  data
                           direction = "long", # convert to long format
                           idvar = input$subject, # existing ID variable name
                           # list of sets of wide-format variable names to make long
                           varying =list(colnames(data1())[grepl( input$rater , names( data1() ) )],
                                         colnames(data1())[grepl( input$Y , names( data1() ) )]),
                           timevar = "observation",
                           v.names =c(input$rater, input$Y))
      } else {
        dataset <- data1()
      }
      ICCsests <- estICCs(dataset, Y = input$Y, subjects = input$subject, raters = input$rater,
                          estimator = "MLE", 
                          #response = "continuous", 
                          #k = input$k, khat = input$khat, Q = input$Q
                          )
      # Als MCMC weer erin gaat, dan estimator = input$estimator
      # Als binary response weer erin gaat dan response = input$response, 
      # return all object as a list
    }
    
    list(ICCs = ICCsests$ICCs, variances = ICCsests$sigmas, 
         raterDesign = cbind(ICCsests$Q, ICCsests$khat, ICCsests$k))
  })
  output$ICCs <- renderTable({
    req(model()$ICCs)
    model()$ICCs
  }, rownames = TRUE)
  ## Output per ICC for better print:
  output$icca1 <- renderUI({
    req(model()$ICCs)
    withMathJax(
      paste0("$$ICC(A,1) = ", round(model()$ICCs[1, 1], 2), ", \\; ",
             "95\\%~CI~[", round(model()$ICCs[1, 2], 2), ",\\; ",
             round(model()$ICCs[1, 3], 2), "], \\; ",
             "SE = ", round(model()$ICCs[1, 4], 2), "$$"))
  })
  output$iccak <- renderUI({
    req(model()$ICCs)
    withMathJax(
      paste0("$$ICC(A,k) = ", round(model()$ICCs[2, 1], 2), ", \\; ",
             "95\\%~CI~[", round(model()$ICCs[2, 2], 2), ",\\; ",
             round(model()$ICCs[2, 3], 2), "], \\; ",
             "SE = ", round(model()$ICCs[2, 4], 2), "$$"))
  })
  output$iccakhat <- renderUI({
    req(model()$ICCs)
    withMathJax(
      paste0("$$ICC(A,\\widehat{k}) = ", round(model()$ICCs[3, 1], 2), ", \\; ",
             "95\\%~CI~[", round(model()$ICCs[3, 2], 2), ",\\; ",
             round(model()$ICCs[3, 3], 2), "], \\; ",
             "SE = ", round(model()$ICCs[3, 4], 2), "$$"))
  })
  output$iccc1 <- renderUI({
    req(model()$ICCs)
    withMathJax(
      paste0("$$ICC(C,1) = ", round(model()$ICCs[4, 1], 2), ", \\; ",
             "95\\%~CI~[", round(model()$ICCs[4, 2], 2), ",\\; ",
             round(model()$ICCs[4, 3], 2), "], \\; ",
             "SE = ", round(model()$ICCs[4, 4], 2), "$$"))
  })
  output$iccck <- renderUI({
    req(model()$ICCs)
    withMathJax(
      paste0("$$ICC(C,k) = ", round(model()$ICCs[5, 1], 2), ", \\; ",
             "95\\%~CI~[", round(model()$ICCs[5, 2], 2), ",\\; ",
             round(model()$ICCs[5, 3], 2), "], \\; ",
             "SE = ", round(model()$ICCs[5, 4], 2), "$$"))
  })
  output$iccqkhat <- renderUI({
    req(model()$ICCs)
    withMathJax(
      paste0("$$ICC(Q,\\widehat{k}) = ", round(model()$ICCs[6, 1], 2), ", \\; ",
             "95\\%~CI~[", round(model()$ICCs[6, 2], 2), ",\\; ",
             round(model()$ICCs[6, 3], 2), "], \\; ",
             "SE = ", round(model()$ICCs[6, 4], 2), "$$"))
  })
  
  ## ICC card output from UI:
  # put it here instead of UI bc we use req()
  custom_card_style <- "
    background: linear-gradient(45deg, #f5f5f5, #f6f6f6);
    border: none;
    border-radius: 0px;
    box-shadow: 0 4px 6px rgba(0,0,0,0.1);
  "
  output$icc_cards<- renderUI({
    req(model()$ICCs)
    page_fillable(
      layout_columns(
        card(uiOutput("icca1")   , style = custom_card_style ),
        card(uiOutput("iccak")   , style = custom_card_style ),
        card(uiOutput("iccakhat"), style = custom_card_style),
        card(uiOutput("iccc1")   , style = custom_card_style ),
        card(uiOutput("iccck")   , style = custom_card_style ),
        card(uiOutput("iccqkhat"), style = custom_card_style),
        col_widths = rep(6, 6)
      )
    )
  })
  output$variances <- renderTable({
    req(model()$variances)
    model()$variances
  }, rownames = TRUE)
  
  output$raterDesign <- renderTable({
    req(model()$raterDesign)
    model()$raterDesign
  }, rownames = TRUE)
  
  
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
  

# FlowChart Server --------------------------------------------------------

  # Reactive values to store the current state of selections and the result
  rv <- reactiveValues(
    step1 = "",
    step2 = "",
    step3 = "",
    step4_1 = "",
    step4_2 = "",
    Ans = "Please make your selections to determine the ICC."
  )
  
  # Observer for Reset button
  observeEvent(input$reset_button, {
    # Reset all select inputs to their initial state
    updateSelectInput(session, "step1", selected = "")
    updateSelectInput(session, "step2", selected = "")
    updateSelectInput(session, "step3", selected = "")
    updateSelectInput(session, "step4_1", selected = "")
    updateSelectInput(session, "step4_2", selected = "")
    
    # Reset reactive values
    rv$step1 <- ""
    rv$step2 <- ""
    rv$step3 <- ""
    rv$step4_1 <- ""
    rv$step4_2 <- ""
    rv$Ans <- "Please make your selections to determine the ICC."
  })
  
  # Observers for each step input to update reactive values and clear subsequent steps
  observeEvent(input$step1, {
    rv$step1 <- input$step1
    # Clear subsequent steps if step1 changes
    rv$step2 <- ""
    rv$step3 <- ""
    rv$step4_1 <- ""
    rv$step4_2 <- ""
    updateSelectInput(session, "step2", selected = "")
    updateSelectInput(session, "step3", selected = "")
    updateSelectInput(session, "step4_1", selected = "")
    updateSelectInput(session, "step4_2", selected = "")
  }, ignoreNULL = FALSE) # ignoreNULL = FALSE to react to initial empty state
  
  observeEvent(input$step2, {
    rv$step2 <- input$step2
    # Clear subsequent steps if step2 changes
    rv$step3 <- ""
    rv$step4_1 <- ""
    rv$step4_2 <- ""
    updateSelectInput(session, "step3", selected = "")
    updateSelectInput(session, "step4_1", selected = "")
    updateSelectInput(session, "step4_2", selected = "")
  }, ignoreNULL = FALSE)
  
  observeEvent(input$step3, {
    rv$step3 <- input$step3
    # Clear subsequent steps if step3 changes
    rv$step4_1 <- ""
    rv$step4_2 <- ""
    updateSelectInput(session, "step4_1", selected = "")
    updateSelectInput(session, "step4_2", selected = "")
  }, ignoreNULL = FALSE)
  
  observeEvent(input$step4_1, {
    rv$step4_1 <- input$step4_1
  }, ignoreNULL = FALSE)
  
  observeEvent(input$step4_2, {
    rv$step4_2 <- input$step4_2
  }, ignoreNULL = FALSE)
  
  # Main reactive expression to determine the ICC based on current selections
  observe({
    current_Ans <- "Please make your selections to determine the ICC."
    path_details <- c()
    
    # Replicate the flow logic
    if (rv$step1 == "Nested") {
      path_details <- c(path_details, paste0("Step 1: Nested (", rv$step1, ")"))
      if (rv$step3 == "Single") {
        path_details <- c(path_details, paste0("Step 3: Single (", rv$step3, ")"))
        current_Ans <- ICCs[8] # ICC(1)
      } else if (rv$step3 == "Average") {
        path_details <- c(path_details, paste0("Step 3: Average (", rv$step3, ")"))
        if (rv$step4_2 == "Balanced") {
          path_details <- c(path_details, paste0("Step 4.2: Balanced (", rv$step4_2, ")"))
          current_Ans <- ICCs[9] # ICC(k)
        } else if (rv$step4_2 == "Unbalanced") {
          path_details <- c(path_details, paste0("Step 4.2: Unbalanced (", rv$step4_2, ")"))
          current_Ans <- ICCs[10] # ICC(khat)
        }
      }
    } else if (rv$step1 == "Crossed") {
      path_details <- c(path_details, paste0("Step 1: Crossed (", rv$step1, ")"))
      if (rv$step2 == "Absolute") { # Absolute
        path_details <- c(path_details, paste0("Step 2: Absolute (", rv$step2, ")"))
        if (rv$step3 == "Single") {
          path_details <- c(path_details, paste0("Step 3: Single (", rv$step3, ")"))
          current_Ans <- ICCs[5] # ICC(A,1)
        } else if (rv$step3 == "Average") {
          path_details <- c(path_details, paste0("Step 3: Average (", rv$step3, ")"))
          if (rv$step4_2 == "Balanced") {
            path_details <- c(path_details, paste0("Step 4.2: Balanced (", rv$step4_2, ")"))
            current_Ans <- ICCs[6] # ICC(A,k)
          } else if (rv$step4_2 == "Unbalanced") {
            path_details <- c(path_details, paste0("Step 4.2: Unbalanced (", rv$step4_2, ")"))
            current_Ans <- ICCs[7] # ICC(A,khat)
          }
        }
      } else if (rv$step2 == "Relative") { # Relative
        path_details <- c(path_details, paste0("Step 2: Relative (", rv$step2, ")"))
        if (rv$step3 == "Single") {
          path_details <- c(path_details, paste0("Step 3: Single (", rv$step3, ")"))
          if (rv$step4_1 == "Complete") {
            path_details <- c(path_details, paste0("Step 4.1: Complete (", rv$step4_1, ")"))
            current_Ans <- ICCs[1] # ICC(C,1)
          } else if (rv$step4_1 == "Incomplete") {
            path_details <- c(path_details, paste0("Step 4.1: Incomplete (", rv$step4_1, ")"))
            current_Ans <- ICCs[2] # ICC(Q,1)
          }
        } else if (rv$step3 == "Average") {
          path_details <- c(path_details, paste0("Step 3: Average (", rv$step3, ")"))
          if (rv$step4_1 == "Complete") {
            path_details <- c(path_details, paste0("Step 4.1: Complete (", rv$step4_1, ")"))
            current_Ans <- ICCs[3] # ICC(C,k)
          } else if (rv$step4_1 == "Incomplete") {
            path_details <- c(path_details, paste0("Step 4.1: Incomplete (", rv$step4_1, ")"))
            current_Ans <- ICCs[4] # ICC(Q,k)
          }
        }
      }
    }
    
    # Update the reactive value for Ans
    rv$Ans <- current_Ans
    rv$path_summary <- paste(path_details, collapse = "\n")
  })
  
  # inal ICC result
  # @todo make it nice
  output$icc_result <- renderText({
    rv$Ans
  })
  
  # path summary
  output$path_summary <- renderText({
    rv$path_summary
  })
  

# -------------------------------------------------------------------------

  
}

options(shiny.port = 8001)
shinyApp(ui, server)
