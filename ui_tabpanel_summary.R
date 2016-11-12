tabpanel.summary = fluidRow(
  box(width = 12, title = "Data Summary",
    dataTableOutput("summary.datatable")
  ),
  tabBox(width = 12, title = "Data Visualization",
    tabPanel(title = "Histogram",
      fluidRow(
        column(4,
          uiOutput("summary.vis.var")),
        column(8,
          uiOutput("summary.vis.hist.nbins")),
        plotOutput("summary.vis.hist")
      )
    ),
    tabPanel(title = "Boxplot",
      fluidRow(
        column(4,
          uiOutput("summary.vis.bp.var")),
        column(8,
          ""),
        plotOutput("summary.vis.bp")
      )
    )
  )
)