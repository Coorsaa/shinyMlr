tabpanel.tuning = fluidRow(
  tabBox(width = 12,
    tabPanel(title = "Tuning selection",
      fluidRow(
        uiOutput("tuning.sel"),
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
        column(6, align = "center",
          actionButton("tune.run", "Tune learner")
        ),
        column(6, align = "center",
          actionButton("tune.set.hp", "Transfer opt. hyperpars to learner")
        )
      ),
      fluidRow(width = 12,
        column(12, align = "center", 
          verbatimTextOutput("print.tuning.ps"),
          verbatimTextOutput("tuning.text")
        )
      )
    )
  )
)

