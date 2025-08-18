
# Tabpanel - Estimate Q/khat ----------------------------------------------

tabpanel_estQk <- tabPanel(
  "Estimate Design Factors",
  wellPanel(
    h4("Input Design Features")
  ),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(6,
               wellPanel(
                 style = "background-color: #fcfcfc; border: none",
                 numericInput("NSubjects", "Number of subjects:", 240)
               )
        ),
        column(6,
               wellPanel(
                 style = "background-color: #fcfcfc; border: none",
                 numericInput("NRaters", "Number of raters:", 6)
               )
        )
      ),
      fluidRow(
        column(6,
               wellPanel(
                 style = "background-color: #fcfcfc; border: none",
                 numericInput("NRperS", "Number of raters per subject:", 2)
               )
        ),
        column(6,
               wellPanel(
                 style = "background-color: #fcfcfc; border: none",
                 selectInput("Design", "Observational Design:",
                             choices = c(
                               Random = "random",
                               Anker = "anker",
                               Block = "block"
                             ),
                             selected = "random")
               )
        )
      ),
    wellPanel(
      style = "background-color: transparent; border: none; text-align: center;",
      uiOutput("estQkhatbutton")
    )
  ),
    mainPanel(
      tabsetPanel(
        tabPanel("Results",
                 uiOutput("estQkhat_card")
        ),
        tabPanel("Table", DT::dataTableOutput(("Qktab"))),
      )
    )
  )
)

