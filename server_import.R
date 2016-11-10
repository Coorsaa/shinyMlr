# input$file1 will be NULL initially. after user selection a df with cols:
# 'size', 'type', and 'datapath'

##### data import #####

output$import.ui = renderUI({
  type = input$import.type;
  if (is.null(type))
    type = "mlr"
  makeImportSideBar(type)
})

data = reactive({
  if (is.null(input$import.type)) {
    return(NULL)
  } else if (input$import.type == "mlr") {
    return(getTaskData(get(input$import.mlr)))
  } else if (input$import.type == "OpenML") {
    t = getOMLDataSet(data.id = as.integer(input$import.OpenML))
    return(t$data)
  } else if (input$import.type == "CSV") {
    f = input$import.csv$datapath
    if (is.null(f)) return(NULL)
    #rn = as.numeric(input$import.rownames)
    read.csv(f, header = input$import.header, sep = input$import.sep,
      quote = input$import.quote) #, row.names = rn)
  } else if (input$import.type == "ARFF") {
    f = input$import.arff$datapath
    if (is.null(f)) return(NULL)
    readARFF(f)
  }
})


output$import.preview = renderDataTable({
  d = data(); if (is.null(d)) return(NULL)
  colnames(d) = make.names(colnames(d)) 
  d
}, options = list(lengthMenu = c(5, 20, 50), pageLength = 5, scrollX = TRUE)
)

output$import.browse.openml = renderDataTable({
  listOMLDataSets()[,c(1:5,10:12)]
})