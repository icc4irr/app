
# tabpanel - Estimate IRR -------------------------------------------------

tabpanel_estIRR <- tabPanel(
                            "Estimate IRR",
                            #titlePanel("Estimate Interrater Reliability with ICCs"),
                            accordion(
                              accordion_panel(
                                title = "Estimate Interrater Reliability with ICCs",
                            wellPanel(
                              #h5("Estimate Interrater Reliability with ICCs"),
                             
                              ## INTRODUCTORY TEXT
                              withMathJax(
                                p("Use this tab to estimate the interrater reliability from (planned-incomplete) observation data. 
                                The program uses maximum likelihood estimation of an hierarchical linear model to estimate 
                                intraclass correlation coeefficients (ICCs) and provides Monte-Carlo confidence intervals for 
                                these ICCs (see ",
                                a("Ten Hove et al., 2025, Multivariate Behavioral Research", href = "https://doi.org/10.1080/00273171.2025.2507745", target = "_blank"), 
                                "). The data should be provided in long-format, meaning that each row should represent a 
                                subject-rater combination and three columns should indicate the subject IDs, rater IDs, 
                                and observation scores.  
                                See the Example application in ", 
                                a("Ten Hove et al. (2025, Multivariate Behavioral Research)", href = "https://doi.org/10.1080/00273171.2025.2507745", target = "_blank"),
                                " for an example data set."),
                              hr(),
                              p(
                                h6("• Output:"),
                                "Estimated variance components (subjects, raters, residual), 
                                Design factors (",
                                HTML("\\(k\\)"), " = total number of raters per subject,", 
                                HTML("\\(\\widehat{k}\\)"), " = harmonic mean number of raters per subject,",
                                HTML("\\(Q\\)")," = proportion of non-overlapping raters across subjects; see ",
                                a("Ten Hove et al., 2024, Psychological Methods", href="https://doi.org/10.1037/met0000516", target = "_blank"),  
                                ") and six types of intraclass correlation coefficients (ICCs)."),
                              
                              hr(),
                              p(
                                h6("• Unsure which ICC to interpret?"),
                                "Use the Tab", em("`Flowchart`"),"."),
                              
                              hr(),
                              p(
                                h6("• Alternative Design Factors?"),
                                "Will your ultimate study use a different observation design than was used in the uploaded data 
                                set (e.g., different numbers of raters per subject, or less/more overlapping raters across subjects)?.",
                                "Use the Tab ",em("`Compute Design Factors`")," (if you already know the observation design of your ultimate study) 
                                or ",em("`Estimate Design Factors`")," (if you still need to decide about the observation design of your primary study) 
                                to find the relevant values for ", HTML("\\(\\widehat{k}\\)")," and ", HTML("\\(Q\\)"),
                                ". Next, use these design factors in the optional Step 3 to estimate the ICC for your ultimate observation study."
                                )
                              
                              ) # mathjax
                            )
                            )),
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
              tabPanel("Design factors", 
                       uiOutput("RaterDesign_cards")),
              tabPanel("Intraclass correlation coefficients (ICCs)",
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
        br(),
        br()
)
