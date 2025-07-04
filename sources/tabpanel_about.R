
# Tab panel - About -------------------------------------------------------

tabpanel_about <- tabPanel("About",
         fluidPage(
           titlePanel("About this App"),
           wellPanel(
             p("Author: Jane Doe"),
             p("Contact: jane.doe@example.com")
           ),
           h4("References & Links"),
           tags$ul(
             tags$li(a("R Project", href = "https://www.r-project.org/", target = "_blank")),
             tags$li(a("Shiny", href = "https://shiny.posit.co/", target = "_blank"))
           )
         )
)