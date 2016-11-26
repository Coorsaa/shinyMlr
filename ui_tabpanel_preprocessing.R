tabpanel.preprocessing = fluidRow(
  box(width = 12, title = "Data Preprocessing",
    fluidRow(
      column(6, 
        selectInput("preproc_method", "Choose data preprocessing method:",
          choices = c("Impute", "Create dummy features", "Drop variable", "Cap large values",# "mergeSmallFactorLevels",
            "Normalize variables", "Remove constant variables"))
      ),
      column(6,
        uiOutput("preproc_target")
      ),
      column(12,
        uiOutput("preproc_impute"),
        uiOutput("preproc_createdummy"),
        uiOutput("preproc_dropfeature"),
        uiOutput("preproc_remconst"),
        uiOutput("preproc_normfeat"),
        uiOutput("preproc_caplarge")
      )
    ),
    column(12,
      fluidRow(
        dataTableOutput("preproc_data")
      )
    )
  )
)
