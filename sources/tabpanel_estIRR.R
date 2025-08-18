
# tabpanel - Estimate IRR -------------------------------------------------

tabpanel_estIRR <- tabPanel(
                            "Estimate IRR",
                            #titlePanel("Estimate Interrater Reliability with ICCs"),
                            
                            wellPanel(
                              h4("Estimate Interrater Reliability with ICCs"),
                             # tags$ul(
                             #   tags$li("Upload a dataset"),
                             #   tags$li("Select the columns of interest & and the desired ICC"),
                             #   tags$li("Click Estimate IRR & inspect the results")
                             # )
                            ),
        ## STEP 1                    
        ## Upload file and output panels
         sidebarLayout(
           sidebarPanel(
             h2("Step 1"),
             fileInput("file1", "Upload your data", accept = c(".rds", ".Rdat", ".csv", ".txt", ".sav", ".xlsx")),
             uiOutput("col")
             
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("Data Table", 
                        div(
                          style = "height: 200px; overflow-y: auto;",
                          DT::dataTableOutput("table1")
                        )
               ),
               tabPanel("Summary Statistics", verbatimTextOutput("summary1"))
             )
           )
         ),
        ##
        ## STEP 2
        sidebarLayout(
          sidebarPanel(
            h2("Step 2"),
            fluidRow( # fluidrow used to make a 2x2 grid for the options
              column(6, uiOutput("format")),
              column(6, uiOutput("subject")),
              column(6, uiOutput("rater")),
              column(6, uiOutput("Y"))
            )
            #,
            #uiOutput("estimatebutton")
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Variances", 
                       uiOutput("variances_cards")),
              tabPanel("Rater Design", 
                       uiOutput("RaterDesign_cards")),
              tabPanel("Intra-Class Correlations (ICCs)",
                       uiOutput("icc_cards")) # fix the cards style in server
            )
          )
        ),
        ##
        ## STEP 3
        sidebarLayout(
          sidebarPanel(
            h3("Step 3 (Optional)"),
            p("Alternative Design factors"),
            fluidRow( 
              column(4, uiOutput("k_input")),
              column(4, uiOutput("khat_input")),
              column(4, uiOutput("Q_input"))
            ),
            uiOutput("estimatebutton")
          ),
          mainPanel()
        ),
        
        wellPanel()
)
