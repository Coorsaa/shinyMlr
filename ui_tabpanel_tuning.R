tabpanel.tuning = fluidRow(
  tabBox(width = 12,
    tabPanel(title = "Tuning selection",
      fluidRow(
        column(width = 4, align = "center",
          makeSidebar(bar.height = 540,
            uiOutput("tuning.sel")
          )
        ),
        column(width = 8, align = "center", 
          dataTableOutput("tuning.table")
        )
      )
    ),
    tabPanel(title = "Param settings",
      fluidRow(
        column(12,
          uiOutput("tuning.learner.params")
        )
      ),
      br(),
      br(),
      fluidRow(      
        column(6, align = "center",
          actionButton("tune.run", "Tune learner")
        ),
        column(6, align = "center",
          actionButton("tune.set.hp", "Transfer opt. hyperpars to learner")
        )
      ),
      br(),
      br(),
      fluidRow(width = 12,
        column(12, align = "center",
          hidden(infoBoxOutput("transfer.info.box", width = 12)),
          verbatimTextOutput("print.tuning.ps"),
          verbatimTextOutput("tuning.text")
        )
      )
    )
  )
)

