tabpanel.benchmark = fluidPage(theme = shinytheme("united"),
  sidebarLayout(
    sidebarPanel(
      sidebarMenu(
        menuItem("Select learners:"),
        uiOutput("benchmark.learners.sel"),
        uiOutput("benchmark.measures.sel"),
        tags$hr(),
        selectInput("benchmark.rdesc.type", label = "Resampling", selected = "CV",
          choices = c("CV", "LOO", "RepCV", "Bootstrap", "Subsample", "Holdout")),
        uiOutput("benchmark.rdesc.config"),
        div(align = "center",
          checkboxInput("benchmark.stratification", label = "Stratification", FALSE),
          tags$hr(),
          uiOutput("benchmark.parallel.ui"),
          tags$hr(),
          bsButton("benchmark.run", label = "benchmark",
            icon = icon("hourglass-start"))
        )
      )
    ),
    mainPanel(
      tabBox(width = 12,
        tabPanel("Benchmark",
          htmlOutput("benchmark.explanatory.text"),
          fluidRow(
            div(align = "center",
              verbatimTextOutput("benchmark.text"),
              br(),
              dataTableOutput("benchmark.overview")
            )
          )
        ),
        tabPanel("Visualisations",
          htmlOutput("benchmark.plots.text"),
          fluidRow(
            column(6, align = "center",
              selectInput("bmrplots.type", label = "Plot Type",
                selected = "Beanplots", choices = c("Beanplots", "Boxplots"),
                width = 200)
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
  )
)








