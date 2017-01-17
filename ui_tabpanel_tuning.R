tabpanel.tuning = fluidRow(
  tabBox(width = 12,
    tabPanel(title = "Tuning selection",
      fluidRow(
        uiOutput("tuning.sel"),
        column(width = 4,
          uiOutput("tuning.iters")
        ),
        column(width = 4,
          numericInput("tuning.cv", "No. of CV folds", min = 1L, max = Inf, value = 3L, step = 1L)
        ),
        uiOutput("tuning.measure.ui"),
        uiOutput("tuning.parallel.ui"),
        column(width = 12, align = "center", 
          dataTableOutput("tuning.table")
        )
      )
    ),
    tabPanel(title = "Param settings",
      fluidRow(
        column(12,
          dataTableOutput("tuning.params.table")
        ),
        column(12,
          uiOutput("tuning.learner.params")
        )
      ),
      fluidRow(      
        column(12, align = "center",
          actionButton("tune.run", "Tune learner")
        )#,
        #column(6, align = "center",
        #  actionButton("tune.set.hp", "Transfer opt. hyperpars to learner")
        #)
      ),
      fluidRow(width = 12,
        column(12, align = "center", 
          verbatimTextOutput("print.tuning.ps"),
          verbatimTextOutput("tuning.text"),
          verbatimTextOutput("print.tuned.lrn")
        )
      )
    )
  )
)

