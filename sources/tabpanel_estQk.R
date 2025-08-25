
# Tabpanel - Estimate Q/khat ----------------------------------------------

tabpanel_estQk <- tabPanel(
  "Estimate Design Factors",
  accordion(
    accordion_panel(
      title = "Estimate Design Factors:",
      wellPanel(
   # h4("Input Design Features")
        p("Use this tab to estimate design factors for different potential observation designs. 
          You can choose between ", em("randomly assigned raters")," for each subject, a ", em("block-design")," in which 
          subsets of raters are—as a group—assigned to subsets of subjects, or an ",
          em("anchor-rater design")," which each subject is observed by the same anchor-rater, and one or more randomly assigned 
          raters from a larger rater pool ",
          a("(see Ten Hove et al., 2025, Multivarate Behavioral Research).", href = "https://doi.org/10.1080/00273171.2025.2507745", 
            target = "_blank")
        )
      ) # wellpanel
    )), #accordion
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
                               Anchor = "anchor",
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
        tabPanel("Design Table", DT::dataTableOutput(("Qktab"))),
      )
    )
  )
)

