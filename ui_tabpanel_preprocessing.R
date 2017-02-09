tabpanel.preprocessing = fluidRow(
  box(width = 12, title = "Data Preprocessing",
    fluidRow(
      column(12, align = "right",
        actionButton("preproc_undo", "Undo last change")),
      column(12, align = "left", 
        selectInput("preproc_method", "Choose data preprocessing method:",
          choices = list("On data" = c("Drop variable(s)", "Convert variable",
            "Normalize variables", "Remove constant variables", "Cap large values",
            "Subset", "Create dummy features", "Impute"),
            "On task" = c("Feature selection", "Merge small factor levels")),
          selected = "Drop variable(s)")
      ),
      column(12,
        uiOutput("preproc_impute"),
        hidden(uiOutput("preproc_createdummy")),
        uiOutput("preproc_dropfeature"),
        uiOutput("preproc_caplarge"),
        uiOutput("preproc_normfeat"),
        uiOutput("preproc_remconst"),
        uiOutput("preproc_convar"),
        uiOutput("preproc_subset"),
        hidden(verbatimTextOutput("vi.task.check")),
        uiOutput("preproc_feature_selection"),
        hidden(plotOutput("plot.feature.selection")),
        hidden(uiOutput("preproc_merge_factor_levels"))
      )
    ),
    br(),
    br(),
    column(12,
      fluidRow(
        dataTableOutput("preproc_data")
      )
    )
  )
)
