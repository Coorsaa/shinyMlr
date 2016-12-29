tabpanel.benchmark = list(
  tabBox(width = 12,
    tabPanel("Benchmark",
      fluidRow(
        box(
          uiOutput("benchmark.learners.sel"),
          uiOutput("benchmark.measures.sel")
        ),
        box(
          selectInput("benchmark.rdesctype", label = "Resampling", selected = "CV", 
            choices = c("CV", "LOO", "RepCV", "Bootstrap", "Subsample", "Holdout")),
          numericInput("benchmark.iters", label = "Iterations", value = 10L, min = 1L, max = 100L, step = 1L),
          checkboxInput("benchmark.stratification", label = "Stratification", FALSE)
        ),
        column(12, align = "center",
          fluidRow(
            actionButton("benchmark.run", label = "Benchmark")
          )
        )
      ),
      dataTableOutput("benchmark.overview"),
      verbatimTextOutput("benchmark.text")
    ),
    tabPanel("Visualisations",
      fluidRow(
        box(width = 12, align = "center",
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
  )
)

