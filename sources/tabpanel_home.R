
# tab panel - Home --------------------------------------------------------

tabpanel_home <- tabPanel("Home",
         fluidPage(
           titlePanel("Welcome"),
           wellPanel(
             h4("What this app does:"),
             tags$ul(
               tags$li("Upload a dataset"),
               tags$li("View the data and calculate summary statistics"),
               tags$li("Read about the authors and references")
             )
           )
         )
)
