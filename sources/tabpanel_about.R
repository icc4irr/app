
# Tab panel - About -------------------------------------------------------

tabpanel_about <- tabPanel("About",
         fluidPage(
           titlePanel("About this App"),
           wellPanel(
             p("ICC4IRR is a shiny application to estimate interrater reliability using 
             intraclass correlation coefficients from incomplete data, resulting from 
             planned-missing observation designs in which raters vary across subjects."),
             hr(),
             h6("Authors:"),
             p("Tasos Psychogyiopoulos, Letty Koopman and Debby ten Hove"),
             h6("Contact:"),
             a("d.ten.hove@vu.nl", href = "mailto:d.ten.hove@vu.nl", target = "_blank"),
             hr(),
             h6("Cite as:"),
             p("Psychogyiopoulos, A., Koopman L. & Ten Hove, D. (2025). ",
               em("ICC4IRR: A shiny application to estimate interrater reliability using intraclass correlation coefficients"),".",
                  a("https://tasospsy.shinyapps.io/icc4irr_app/", href = "https://tasospsy.shinyapps.io/icc4irr_app/")),
             br(),
             p("Example citation: We investigated the interrater consistency [or agreement] 
               using intraclass correlation coefficients (ICCs) [that accounted for partially 
               non-overlapping raters across subjects] using the R/shiny application ICC4IRR 
               (Psychogyiopoulos, Koopman & Ten Hove, 2025). "),
            ),
           br(),
             h5("Additional references"),
             tags$ul(
               tags$li("Ten Hove, D., Jorgensen, T. D., & van der Ark, L. A. (2024). Updated guidelines on selecting an intraclass correlation 
                       coefficient for interrater reliability, with applications to incomplete observational 
                       designs. ",em("Psychological Methods, 29"),"(5), 967–979.", 
                       a("https://doi.org/10.1037/met0000516", href = "https://doi.org/10.1037/met0000516", target = "_blank")),
               tags$li("Ten Hove, D., Jorgensen, T. D., & Van der Ark, L. A. (2025). How to estimate intraclass correlation coefficients 
                       for interrater reliability from planned incomplete data. ", em("Multivariate Behavioral Research. ")),
                       a("https://doi.org/10.1080/00273171.2025.2507745", href = "https://doi.org/10.1080/00273171.2025.2507745", target = "_blank"),        
               tags$li(a("R Project", href = "https://github.com/icc4irr", target = "_blank")),
               tags$li(a("Shiny", href = "https://tasospsy.shinyapps.io/icc4irr_app/", target = "_blank"))
             ),
             br(),br()
           )
         )