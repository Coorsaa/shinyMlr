tabpanel.preprocessing = fluidRow(
  box(width = 12, title = "Data Preprocessing",
    fluidRow(
      column(12, align = "right",
        actionButton("preproc_undo", "Undo last change")),
      column(12, align = "left", 
        selectInput("preproc_method", "Choose data preprocessing method:",
          choices = c("Impute", "Create dummy features", "Drop variable(s)", "Cap large values", # "Merge small factor levels", "Join class levels",
            "Normalize variables", "Remove constant variables", "Convert variable",
            "Subset"))
      ),
      column(12,
        uiOutput("preproc_impute"),
        uiOutput("preproc_createdummy"),
        uiOutput("preproc_dropfeature"),
        uiOutput("preproc_caplarge"),
        uiOutput("preproc_normfeat"),
        uiOutput("preproc_remconst"),
        uiOutput("preproc_convar"),
        uiOutput("preproc_subset")
      )
    ),
    column(12,
      fluidRow(
        dataTableOutput("preproc_data")
      )
    )
  )
)
