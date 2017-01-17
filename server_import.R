# input$file1 will be NULL initially. after user selection a df with cols:
# 'size', 'type', and 'datapath'

##### data import #####

output$import.ui = renderUI({
  type = input$import.type;
  if (is.null(type))
    type = "mlr"
  makeImportSideBar(type)
})

data = reactiveValues(data = NULL)


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
      data$data = readARFF(f)
    }
  }
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


output$import.preview = DT::renderDataTable({
  reqAndAssign(data$data, "d")
  colnames(d) = make.names(colnames(d))
  d
}, options = list(lengthMenu = c(5, 20, 50), pageLength = 5, scrollX = TRUE))

output$import.browse.openml = DT::renderDataTable({
  show("loading.message2")
  df = listOMLDataSets()[,c(1:5,10:12)]
  hide("loading.message2")
  df
})
