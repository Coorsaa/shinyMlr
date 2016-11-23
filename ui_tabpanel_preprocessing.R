tabpanel.preprocessing = fluidRow(
  box(width = 12, title = "Data Preprocessing",
    fluidRow(
      column(6, 
        selectInput("preproc_method", "Choose data preprocessing method:",
          choices = c("Impute", "capLargeValues",  "createDummyFeatures", "dropFeatures", "mergeSmallFactorLevels", "normalizeFeatures", "removeConstantFeatures"))
      ),
      column(6,
        uiOutput("preproc_target")
      ),
      column(12,
        uiOutput("preproc_impute")
      ),
      column(12,
        uiOutput("preproc_createdummy")
      ),
      column(12,
      uiOutput("preproc_dropfeature")
      )
    ),
    column(12,
      fluidRow(
        dataTableOutput("impute_datatable"),
        dataTableOutput("createdummy_datatable"),
        dataTableOutput("dropfeature_datatable")
      )
    )
  )
)
