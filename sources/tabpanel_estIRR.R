
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
        ## uplaod file and output panels
         sidebarLayout(
           sidebarPanel(
             h5("Step 1"),
             fileInput("file1", "Upload your data", accept = c(".rds", ".Rdat", ".csv", ".txt")),
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
        sidebarLayout(
          sidebarPanel(
            h5("Step 2"),
            uiOutput("subject"),
            uiOutput("rater"),
            uiOutput("Y"),
        #tags$hr(),
          ),
        mainPanel()
        )
)
