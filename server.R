library(tools)
library(mlr)
library(readr)

source("server_helpers.R")

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)

shinyServer(function(input, output) {
    
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
    } else if (input$import.type == "CSV") {
      f = input$import.file$datapath
      if (is.null(f)) return(NULL)
      rn = as.numeric(input$import.rownames)
      read.csv(f, header = input$import.header, sep = input$import.sep,
        quote = input$import.quote, row.names = rn)
    }
  })
  
  output$import.preview = renderDataTable({
    d = data(); if (is.null(d)) return(NULL)
    d
  })
  
  ##### data summary #####
  
  output$summary.datatable = renderDataTable({
    d = data(); if (is.null(d)) return(NULL)
    summarizeColumns(d)
  })
  
  ##### task #####
  
  output$task.id = renderUI({
    id = file_path_sans_ext(input$import.file$name)
    # FIXME: quickinit / remove later
    id = "iris"
    textInput("task.id", "Task ID", value = id)
  })
  
  output$task.target = renderUI({
    selectInput("task.target", "Choose a target:", as.list(colnames(data())))
  })
  
  task = reactive({
    d = data() 
    if (is.null(d)) return(NULL)
    sMakeTask(input, d)
  })

  output$task.overview = renderPrint({
    tt = task(); if (is.null(tt)) return(NULL)
    print(tt)
  })
  
  ##### benchmark #####
  
  learners = reactive({
    tt = task(); if (is.null(tt)) return(NULL)
    # FIXME: create function makeLearners("classif", c("rpart", "randomForest"))? 
    if (tt$task.desc$type == "classif")
      learners = c("classif.rpart", "classif.randomForest")
    else
      learners = c("regr.rpart", "regr.randomForest")
    learners = lapply(learners, makeLearner)
    # FIXME: benchmark should accept char vec?
    # FIXME: getLearnerID function seems missing
    lids = extractSubList(learners, "id")
    setNames(learners, lids)
  })
  
  rdesc = reactive({
    makeResampleDesc(input$benchmark.rdesctype, iters = input$benchmark.iters)
  })
  
  measures.avail = reactive({
    tt = task(); if (is.null(tt)) return(NULL)
    ms = listMeasures(tt, create = TRUE)
    return(ms)
  })
  
  output$benchmark.measures.sel = renderUI({
    ms = measures.avail(); if (is.null(ms)) return(NULL)
    ms.ids = extractSubList(ms, "id")
    selectInput("benchmark.measures.sel", "Measures", choices = ms.ids, multiple = TRUE)
  })
  
  bmr = eventReactive(input$benchmark.run, {
    tt = task(); if (is.null(tt)) return(NULL)
    ms = measures.avail()
    ms = ms[input$benchmark.measures.sel]
    lrns = learners()
    rd = rdesc()
  
    withCallingHandlers({
      benchmark(lrns, tt, rd, measures = ms, show.info = TRUE)
    },
    message = function(m) {
      shinyjs::html(id = "benchmark.text", html = m$message, add = FALSE)
    })
  })

  output$benchmark.overview = renderDataTable({
    b = bmr(); if (is.null(b)) return(NULL)
    getBMRAggrPerformances(b, as.df = TRUE)
  })
  
  ##### benchmark plots #####
  
  output$bmrplots = renderPlot({
    b = bmr(); if (is.null(b)) return(NULL)
    plotfun = switch(input$bmrplots.type,
      Beanplots = function(b) plotBMRBoxplots(b, style = "violin"),
      Boxplots = plotBMRBoxplots,
      Ranks = plotBMRSummary
    )
    plotfun(b)
  })
  
  ##### partial dependency #####
  
  output$partialdep.learner = renderUI({
    lrns = learners(); if (is.null(lrns)) return(NULL)
    lids = names(lrns)
    selectInput("partialdep.learner", "Choose a model:", choices = lids)
  })
  
  output$partialdep.feature = renderUI({
    selectInput("partialdep.feature", "Choose a feature:", getTaskFeatureNames(task()))
  })
  
  output$partialdep.plot = renderPlot({
    tt = task(); if (is.null(tt)) return(NULL)
    lrns = learners()
    sPlotPartialDep(input, tt, lrns)
  })
})

