tabpanel.benchmark = fluidRow(
  tabBox(width = 12,
    tabPanel("Benchmark",
      # fluidRow(
        box(title = "Learners", width = 12, align = "center",
          uiOutput("benchmark.learners.sel")
      ),
      fluidRow(
        column(width = 3, align = "center",
          makeSidebar(bar.height = 500,
            uiOutput("benchmark.measures.sel"),
            tags$hr(),
            selectInput("benchmark.rdesc.type", label = "Resampling", selected = "CV", 
              choices = c("CV", "LOO", "RepCV", "Bootstrap", "Subsample", "Holdout")),
            uiOutput("benchmark.rdesc.config"),
            # numericInput("benchmark.iters", label = "Iterations", value = 10L, min = 1L, max = 100L, step = 1L),
            checkboxInput("benchmark.stratification", label = "Stratification", FALSE),
            tags$hr(),
            actionButton("benchmark.run", label = "Benchmark")
          )
        ),
        column(width = 9, align = "center",
          dataTableOutput("benchmark.overview"),
          br(),
          verbatimTextOutput("benchmark.text")
        )
      )
      #)
    ),
    tabPanel("Visualisations",
      fluidRow(
        box(width = 12, align = "center",
          selectInput("bmrplots.type", label = "Plot Type", selected = "Beanplots", 
            choices = c("Beanplots", "Boxplots"), width = 200)
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

