tabpanel.preprocessing = fluidPage(theme = shinytheme("united"),
  sidebarLayout(
  sidebarPanel(
    div(align = "center",
    sidebarMenu(
      menuItem("Choose data"),
      selectInput("preproc_df", "", choices = c("training set", "test set"),
        selected = "training set"),
      menuItem("Choose method"),
      selectInput("preproc_method", "",
        choices = list("On data" = c("Drop variable(s)", "Convert variable",
          "Normalize variables", "Remove constant variables",
          "Recode factor levels", "Cap large values", "Subset",
          "Create dummy features", "Impute"), "On task" = c("Feature selection",
          "Merge small factor levels")), selected = "Drop variable(s)")
    ),
    tags$hr(),
    br(),
    uiOutput("preproc.go"),
    br(),
    tags$hr(),
    bsButton("preproc_undo", "undo", icon = icon("undo")),
    tags$hr(),
    downloadButton("preproc.data.download", "save processed data")
  )
  ),
  mainPanel(
    fluidRow(htmlOutput("preproc.text")),
    fluidRow(
        box(width = 12, title = "Settings",
          uiOutput("preproc_out"),
          plotlyOutput("plot.feature.selection")
        ),
        tabBox(width = 12,
          tabPanel(title = "Data",
            dataTableOutput("preproc_data")
          ),
          tabPanel(title = "Summary",
            dataTableOutput("summary.datatable2")
          )
        )
    )
  )
)
)
#   ,
#   fluidRow(
#     column(width = 3, align = "center",
#       makeSidebar(
#         selectInput("preproc_df", "Choose data",
#           choices = c("training set", "test set"), selected = "training set"),
#         selectInput("preproc_method", "Choose preprocessing method:",
#           choices = list("On data" = c("Drop variable(s)", "Convert variable",
#             "Normalize variables", "Remove constant variables",
#             "Recode factor levels", "Cap large values", "Subset",
#             "Create dummy features", "Impute"), "On task" = c("Feature selection",
#             "Merge small factor levels")), selected = "Drop variable(s)"),
#         tags$hr(),
#         br(),
#         uiOutput("preproc.go"),
#         br(),
#         tags$hr(),
#         bsButton("preproc_undo", "undo", icon = icon("undo")),
#         tags$hr(),
#         downloadButton("preproc.data.download", "save processed data")
#       )
#     ),
#     column(width = 9,
#       box(width = 12, title = "Settings",
#         uiOutput("preproc_out"),
#         plotlyOutput("plot.feature.selection")
#       ),
#       tabBox(width = 12,
#         tabPanel(title = "Data",
#           dataTableOutput("preproc_data")
#         ),
#         tabPanel(title = "Summary",
#           dataTableOutput("summary.datatable2")
#         )

#       )
#     )
#   )
# )
