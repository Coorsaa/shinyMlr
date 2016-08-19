 


shinyUI(fluidPage(
  
  #titlePanel("shinyMlr", img(src="mlrLogo_blue_141x64.png")),
fluidRow(
  column(2, img(src="mlrLogo_blue_141x64.png")),
  column(10, h2("Integration of the",  a("mlr", 
                                         href = "https://github.com/mlr-org/mlr"), "package into shiny", align = "center")
  )
),
  
fluidRow(
  column(12,
         tabsetPanel(type = "tabs", 
                     tabPanel("Load", 
                              sidebarPanel(
                                fileInput('file1', 'Choose CSV File',
                                          accept=c('text/csv', 
                                                   'text/comma-separated-values,text/plain', 
                                                   '.csv')),
                                tags$hr(),
                                checkboxInput('header', 'Header', TRUE),
                                radioButtons('sep', 'Separator',
                                             c(Comma=',',
                                               Semicolon=';',
                                               Tab='\t'),
                                             ','),
                                radioButtons('quote', 'Quote',
                                             c(None='',
                                               'Double Quote'='"',
                                               'Single Quote'="'"),
                                             '"')
                              ),
                              mainPanel(tableOutput('contents'))),
                     tabPanel("Preprocessing", 
                              fluidRow(
                                column(2, "Wrapper steps1"),
                                column(3, uiOutput("lrns")),
                                column(3, uiOutput("target"))
                              )),
                     tabPanel("Training", 
                              "Chosen model:", textOutput("lrnText"),
                              column(3,
                                     actionButton("train", label = "Train")),
                              column(3,
                                     br(),
                                     textOutput("trainText")
                              )
                              ),
                     tabPanel("Prediction", "Choose a dataset"),
                     tabPanel("Resample", 
                              fluidRow("Chosen model:", 
                              textOutput("lrnText2"),
                              column(3,
                                     actionButton("resample", label = "Resample")),
                              column(3,
                                     br(),
                                     textOutput("resampleText")
                              )),
                              br(),
                              "Result (mmce):",  textOutput("resamplePrint")
                     )
                
         )
  )
)
))


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

