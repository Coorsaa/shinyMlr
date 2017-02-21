tabpanel.preprocessing = list(
  fluidRow(htmlOutput("preproc.text")),
  fluidRow(
    column(width = 3, align = "center",
      makeSidebar(
        selectInput("preproc_df", "Choose data",
          choices = c("training set", "test set"), selected = "training set"),
        selectInput("preproc_method", "Choose preprocessing method:",
          choices = list("On data" = c("Drop variable(s)", "Convert variable",
            "Normalize variables", "Remove constant variables", "Cap large values",
            "Subset", "Create dummy features", "Impute"),
          "On task" = c("Feature selection", "Merge small factor levels")),
          selected = "Drop variable(s)"),
        tags$hr(),
        actionButton("preproc_go", "Apply preprocessing"),
        br(),
        tags$hr(),
        bsButton("preproc_undo", "Undo")
      )
    ),
    column(width = 9,
      box(width = 12, title = "Settings",
        uiOutput("preproc_out"),
        plotOutput("plot.feature.selection")
      ),
      box(width = 12,
        dataTableOutput("preproc_data")# ,
        # verbatimTextOutput("preproc_testout")
      )
    )
  )
)
