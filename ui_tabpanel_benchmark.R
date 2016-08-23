tabpanel.benchmark = tabPanel("Benchmark", 
  sidebarPanel(
    uiOutput("benchmark.learners.sel"),
    uiOutput("benchmark.measures.sel"),
    selectInput("benchmark.rdesctype", label = "Resampling", selected = "CV", 
      choices = c("CV", "Subsample", "Bootstrap")),
    numericInput("benchmark.iters", label = "Iterations", value = 10L, min = 1L, max = 100L, step = 1L),
    actionButton("benchmark.run", label = "Benchmark")
  ),
  mainPanel(
    # fluidRow(style="height:200px;", verbatimTextOutput("benchmark.text")),
    # fluidRow(dataTableOutput("benchmark.overview"))
    dataTableOutput("benchmark.overview"),
    verbatimTextOutput("benchmark.text")
  )
)

