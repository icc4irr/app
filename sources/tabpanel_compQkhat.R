
# tabpanel - Compute Q/khat -------------------------------------------------

tabpanel_compQkhat <- tabPanel(
  "Compute Design Factors",
  wellPanel(
    h4("Compute Q khat given your data:"),
  ),
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
      uiOutput("computebutton")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Results",
                 uiOutput("compQkhat_card")
        ),
        tabPanel("Table", DT::dataTableOutput(("Qktab_comp"))),
      )
    )
  )
)
