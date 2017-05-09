tabpanel.tuning = fluidPage(
  theme = shinytheme("united"),
  sidebarLayout(
    sidebarPanel(width = 3,
      uiOutput("tuning.sel")
    ),
    mainPanel(width = 9,
      tabBox(id = "tuning.tab", width = 12,
        tabPanel(title = "Tuning selection",
          htmlOutput("tuning.explanation.text"),
          fluidRow(
            column(width = 12, align = "center",
              htmlOutput("tuning.validation"),
              dataTableOutput("tuning.table")
            )
          )
        ),
        tabPanel(title = "Param settings",
          fluidRow(htmlOutput("tuning.settings.text")),
          fluidRow(
            uiOutput("tuning.learner.params")
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
          fluidRow(
            column(12, align = "center",
              hidden(infoBoxOutput("transfer.info.box", width = 12)),
              verbatimTextOutput("print.tuning.ps")#,
              # verbatimTextOutput("tuning.text")
            )
          )
        )
      )
    )
  )
)

