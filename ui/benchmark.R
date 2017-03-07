tabpanel.benchmark = fluidRow(
  tabBox(width = 12,
    tabPanel("Benchmark",
      htmlOutput("benchmark.explanatory.text"),
      box(title = "Learners", width = 12, align = "center",
        uiOutput("benchmark.learners.sel")
      ),
      fluidRow(
        column(width = 3, align = "center",
          makeSidebar(bar.height = 570,
            uiOutput("benchmark.measures.sel"),
            tags$hr(),
            selectInput("benchmark.rdesc.type", label = "Resampling", selected = "CV", 
              choices = c("CV", "LOO", "RepCV", "Bootstrap", "Subsample", "Holdout")),
            uiOutput("benchmark.rdesc.config"),
            checkboxInput("benchmark.stratification", label = "Stratification", FALSE),
            tags$hr(),
            uiOutput("benchmark.parallel.ui"),
            tags$hr(),
            bsButton("benchmark.run", label = "benchmark", style = "info",
              icon = icon("hourglass-start")),
            tags$hr()
          )
        ),
        column(width = 9, align = "center",
          verbatimTextOutput("benchmark.text"), 
          br(),
          dataTableOutput("benchmark.overview")
        )
      )
      #)
    ),
    tabPanel("Visualisations",
      htmlOutput("benchmark.plots.text"),
      fluidRow(
        column(6, align = "center",
          selectInput("bmrplots.type", label = "Plot Type", selected = "Beanplots", 
            choices = c("Beanplots", "Boxplots"), width = 200)
        ),
        column(6, align = "center",
          uiOutput("bmrplot.measures.sel")
        )
      ),
      fluidRow(
        box(width = 12,
          plotlyOutput("bmrplots")
        )
      )
    )
  )
)

