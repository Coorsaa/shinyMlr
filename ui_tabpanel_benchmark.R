tabpanel.benchmark = list(
  h2("Benchmark"),
  fluidRow(
  box(
    uiOutput("benchmark.learners.sel"),
    uiOutput("benchmark.measures.sel")
  ),
  box(
    selectInput("benchmark.rdesctype", label = "Resampling", selected = "CV", 
      choices = c("CV", "Subsample", "Bootstrap")),
    numericInput("benchmark.iters", label = "Iterations", value = 10L, min = 1L, max = 100L, step = 1L)
  )
  ),
  fluidRow(
    box(width = 12, align = "center",
      actionButton("benchmark.run", label = "Benchmark")
    )
  ),
  fluidRow(box(width= 12,
    dataTableOutput("benchmark.overview"),
    verbatimTextOutput("benchmark.text")
    )
  )
)
