
# Tabpanel - Estimate Q/khat ----------------------------------------------

tabpanel_estQk <- tabPanel("Dataset 2",
         sidebarLayout(
           sidebarPanel(
             fileInput("file2", "Upload CSV file for Dataset 2", accept = ".csv"),
             uiOutput("cols_ui2")
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("Data Table", tableOutput("table2")),
               tabPanel("Summary Statistics", verbatimTextOutput("summary2"))
             )
           )
         )
)