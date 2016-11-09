tabpanel.bmrplots = list(
  fluidRow(
    box(width = 12, align = "center", collapsible = TRUE,
      selectInput("bmrplots.type", label = "Plot Type", selected = "Beanplots", 
        choices = c("Beanplots", "Boxplots"))
    )
  ),
  fluidRow(
    box(width = 12,
      plotOutput("bmrplots")
    )
  )
)

