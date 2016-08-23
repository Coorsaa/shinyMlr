tabpanel.partialdep = tabPanel("Partial Dep.", 
  sidebarPanel(
    uiOutput("partialdep.learner"),
    uiOutput("partialdep.feature"),
    actionButton("partialdep.run", label = "Partial Dep")
  ),
  mainPanel(
    plotOutput("partialdep.plot")
  )
)


