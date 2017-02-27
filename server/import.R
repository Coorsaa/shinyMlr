# input$file1 will be NULL initially. after user selection a df with cols:
# 'size', 'type', and 'datapath'

##### data import #####

output$import.ui = renderUI({
  type = input$import.type;
  if (is.null(type))
    type = "mlr"
  makeImportSideBar(type)
})

data = reactiveValues(data = NULL, data.test = NULL, data.name = NULL)


observe({
  reqAndAssign(input$import.type, "import.type")
  if (is.null(import.type)) {
    data$data = NULL
  } else if (import.type == "mlr") {
    data$data = getTaskData(get(input$import.mlr))
  } else if (import.type == "OpenML") {
    show("loading.message")
    imp.status = need(!is.null(input$import.OpenML), "")
    if (is.null(imp.status)) {
      data.id = as.integer(input$import.OpenML)
    } else {
      data.id = 61L
    }
    t = getOMLDataSet(data.id = data.id)
    hide("loading.message")
    data$data = t$data
  } else if (import.type == "CSV") {
    f = input$import.csv$datapath
    if (is.null(f)) {
      data$data = NULL
    } else {
      data$data = read.csv(f, header = input$import.header, sep = input$import.sep,
      quote = input$import.quote)
    }
  } else if (import.type == "ARFF") {
    f = input$import.arff$datapath
    if (is.null(f)) {
      data$data = NULL
    } else {
      data$data = foreign::read.arff(f)
      # data$data = readARFF(f)
    }
  }
  preproc.data$data = isolate(data$data)
})

data.name = reactive({
  type = input$import.type
  if (type == "mlr") {
    return(getTaskId(get(input$import.mlr)))
  } else {
    if (type == "OpenML") {
      return(as.character(input$import.OpenML))
    } else {
      if (type == "CSV") {
        return(input$import.csv$name)
      } else {
        return(input$import.arff$name)
      }
    }
  }
})

observe({
  reqAndAssign(input$import.type, "import.type")
  data$data.name = data.name()
})


output$import.preview = DT::renderDataTable({
  reqAndAssign(data$data, "d")
  colnames(d) = make.names(colnames(d))
  d
}, options = list(scrollX = TRUE),
  caption = "You imported the following data set")

output$import.browse.openml = DT::renderDataTable({
  show("loading.message2")
  df = isolate(OMLData())[,c(1:5,10:12)]
  hide("loading.message2")
  df
}, options = list(scrollX = TRUE),
  caption = "Click on OpenML Dataset you want to select.", selection = "single")


#### OpenML

output$tabpanel.browse.openml = renderUI({
  fluidRow(
    box(width = 12, title = "Browse OpenML",
      hidden(
        div(id = "loading.message2", align = "center",
          h4("Loading datasets from OpenML")
        )
      ),
      column(12, DT::dataTableOutput("import.browse.openml"))
    )
  )
})

observeEvent(input$import.type, {
  if (input$import.type == "OpenML")
    shinyjs::show("tabpanel.browse.openml")
  else
    shinyjs::hide("tabpanel.browse.openml")
})

observeEvent(input$import.browse.openml_rows_selected, {
  reqAndAssign(isolate(OMLData()), "opml")
  data.id = opml[input$import.browse.openml_rows_selected, 1]
  d = getOMLDataSet(data.id = data.id)
  updateNumericInput(session, "import.OpenML", value = data.id)
})


OMLData = reactive({
  openml.dfs = tryCatch(listOMLDataSets(), error = function(err) {
    "Failed to load data sets from server."
  })
  openml.dfs
})

