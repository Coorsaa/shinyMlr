tabpanel.bmrplots = tabPanel("Benchmark Plots", 
  sidebarPanel(
    selectInput("bmrplots.type", label = "Plot Type", selected = "Beanplots", 
      choices = c("Beanplots", "Boxplots", "Ranks"))
  ),
  mainPanel(
    plotOutput("bmrplots")
  )
)


