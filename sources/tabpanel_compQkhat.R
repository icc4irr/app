
# tabpanel - Compute Q/khat -------------------------------------------------

tabpanel_compQkhat <- tabPanel(
  "Compute Design Factors",
  accordion(
    accordion_panel(
        title = "Compute Design Factors:",
      wellPanel(
      #h4("Compute Q khat given your data:"),
        p("Use this tab to compute the design factors  based on the planned subject-rater 
          combinations in our ultimate study. The data should be provided in long-format, 
          meaning that each row should represent a subject-rater combination and two columns 
          should indicate the subject IDs and rater IDs. This is similar to the first two columns
          in the ", em("`Example application data`")," in ",
          a("Ten Hove et al. (2025, Multivariate Behavioral Research)", 
            href= "https://doi.org/10.1080/00273171.2025.2507745", target = "_blank"),".")
      ) # wellpanel
    )), #accordion
  ## uplaod file and output panels
  sidebarLayout(
    sidebarPanel(
      h2("Step 1"),
      fileInput("file2", "Upload your data", accept = c(".rds", ".Rdat", ".csv", ".txt", ".sav", ".xlsx"))
      
    ),
    mainPanel(
      tabsetPanel(
        div(style = "height: 180px; overflow-y: auto;",
            DT::dataTableOutput("dat2"))
      )
    )
  ),
  sidebarLayout(
    sidebarPanel(
      h2("Step 2"),
      fluidRow(
        column(6, uiOutput("subject_comp")),
        column(6, uiOutput("rater_comp"))
      ),
      uiOutput("computebutton"),
      br(),br()
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Results",
                 uiOutput("compQkhat_card")
        ),
        tabPanel("Design Table", DT::dataTableOutput(("Qktab_comp"))),
      )
    )
  )
)
