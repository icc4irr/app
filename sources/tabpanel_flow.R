
# tabpanel - Flowchart -------------------------------------------------
ICCs <- c("ICC(C,1)", "ICC(Q,1)", "ICC(C,k)", "ICC(Q,k)", "ICC(A,1)", "ICC(A,k)", "ICC(A,khat)", "ICC(1)", "ICC(k)", "ICC(khat)")
tabpanel_flow <- tabPanel(
  "Flowchart",
  #titlePanel("Estimate Interrater Reliability with ICCs"),
  
  wellPanel(
    h4("Follow the steps to determine the ICC:"),
    # tags$ul(
    #   tags$li("Upload a dataset"),
    #   tags$li("Select the columns of interest & and the desired ICC"),
    #   tags$li("Click Estimate IRR & inspect the results")
    # )
  ),
  ## uplaod file and output panels
  # Sidebar layout for inputs and output
  sidebarLayout(
    sidebarPanel(
      # Step 1: Crossed or Nested
      selectInput("step1", "1. Is the Observational Design Crossed or Nested?",
                  choices = c("Please Select" = "", "Crossed", "Nested")),
      
      # Step 2: Relative or Absolute (conditional on Step 1 == "C")
      conditionalPanel(
        condition = "input.step1 == 'Crossed'",
        selectInput("step2", "2. Are Ratings Used for Absolute or Relative
Inferences?",
                    choices = c("Please Select" = "", "Absolute", "Relative"))
      ),
      
      # Step 3: Single or Average (conditional on Step 1 or Step 2)
      conditionalPanel(
        condition = "input.step1 != '' && (input.step1 == 'Nested' || input.step2 != '')",
        selectInput("step3", "3. Are Single or Average Ratings Used??",
                    choices = c("Please Select" = "", "Single", "Average"))
      ),
      
      # Step 4.1: Complete or Incomplete (conditional on specific path)
      conditionalPanel(
        condition = "input.step1 == 'Crossed' && input.step2 == 'Relative' && input.step3 != ''",
        selectInput("step4_1", "Are Observations Complete or Incomplete?",
                    choices = c("Please Select" = "", "Complete", "Incomplete"))
      ),
      
      # Step 4.2: Balanced or Unbalanced (conditional on specific paths)
      conditionalPanel(
        condition = "(input.step1 == 'Nested' && input.step3 == 'Average') || (input.step1 == 'Crossed' && input.step2 == 'Absolute' && input.step3 != '')",
        selectInput("step4_2", "Is the Number of Raters Per Subject Balanced or Unbalanced?",
                    choices = c("Please Select" = "", "Balanced", "Unbalanced"))
      ),
      
      hr(), # Horizontal rule for separation
      
      # Reset button
      actionButton("reset_button", "Reset All Selections", icon = icon("times"),
                   style = "color: white; background-color: #f5ad5f; border-color: #f5ad5f; 
                      border-radius: 6px; padding: 8px 16px; font-size: 16px; font-weight: bold;")
    ),
    
    # Main panel for displaying output
    mainPanel(
      h3("Your Chosen ICC:"),
      # Display the final ICC result
      textOutput("icc_result"),
      br(),
      h4("Decision Path Summary:"),
      textOutput("path_summary")
    )
  )
)
