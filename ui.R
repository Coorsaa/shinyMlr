 
source("ui_tabpanel_import.R")
source("ui_tabpanel_summary.R")
source("ui_tabpanel_task.R")
source("ui_tabpanel_benchmark.R")
source("ui_tabpanel_bmrplots.R")
source("ui_tabpanel_predictionplot.R")
source("ui_tabpanel_partialdep.R")
source("ui_tabpanel_train_and_predict.R")

shinyUI(fluidPage(
    shinyjs::useShinyjs(),

    #titlePanel("shinyMlr", img(src="mlrLogo_blue_141x64.png")),
    fluidRow(
      column(2, img(src="mlrLogo_blue_141x64.png")),
      column(10, align = "center", 
        h2("Integration of the",  a("mlr", href = "https://github.com/mlr-org/mlr"), "package into shiny")
        )
      ),

    navbarPage("", 
          tabpanel.import, 
          tabpanel.summary, 
          tabpanel.task, 
          tabpanel.benchmark,
          navbarMenu("Visualisations",
          tabpanel.bmrplots,
          tabpanel.predictionplot,
          tabpanel.partialdep
          ),
          navbarMenu("Train and Predict",
                     tabpanel.train,
                     tabpanel.predict,
                     tabpanel.performance
          )
        )
  )
)


# shinyUI(fluidPage(
#   titlePanel("My Shiny App"),
#   sidebarLayout(
#     sidebarPanel(),
#     mainPanel(
#       p("p creates a paragraph of text."),
#       p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'times'; font-si16pt"),
#       strong("strong() makes bold text."),
#       em("em() creates italicized (i.e, emphasized) text."),
#       br(),
#       code("code displays your text similar to computer code"),
#       div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
#       br(),
#       p("span does the same thing as div, but it works with",
#         span("groups of words", style = "color:blue"),
#         "that appear inside a paragraph.")
#     )
#   )
# ))

