##### task #####

output$task.id = renderUI({
  req(data.name)
  id = data.name()
  textInput("task.id", "Task ID", value = id)
})

output$task.target = renderUI({
  req(data$data)
  col.names = colnames(data$data)
  tsk.weights = input$task.weights
  choices = col.names[col.names != tsk.weights]
  selectInput("task.target", "Choose a target:", choices = choices, selected = getLast(choices))
})

output$task.weights = renderUI({
  col.names = colnames(Filter(is.numeric, data$data))
  tsk.tar = input$task.target
  choices = col.names[col.names != tsk.tar]
  choices = c("-", choices)
  selectInput("task.weights", "Observation weights (optional):", choices = choices, selected = NULL)
})


task.object = reactiveValues(task = NULL)

observeEvent(input$create.task, {
  req(data$data)
  validate(need("Task" %in% class(tsk), tsk))
  d = isolate(data$data)
  colnames(d) = make.names(colnames(d))
  org.col.names = colnames(d)
  task = tryCatch(sMakeTask(input$task.id, input$task.target, d, input$task.weights),
    error = errAsString)
  task.object$task = task
  task.df = getTaskData(task)
  if (input$task.weights != "-") {
    df = getTaskData(task)
    df[, input$task.weights] = mlr:::getTaskWeights(task)
    data$data = df[, org.col.names]
  } else {
    data$data = getTaskData(task)  
  }
})

task = reactive({
  reqAndAssign(task.object$task, "tsk")
  tsk
})

task.type = reactive({
  reqAndAssign(task(), "tsk")
  validate(need("Task" %in% class(tsk), tsk))
  getTaskType(tsk)
})

target.levels = reactive({
  tsk.type = task.type()
  tar.levels = NULL
  if (tsk.type == "classif")
    tar.levels = getTaskClassLevels(task())
  return(tar.levels)
})

task.data = reactive({
  validate(need("Task" %in% class(tsk), tsk))
  getTaskData(task())
})

task.feature.names = reactive({
  getTaskFeatureNames(task())
})

task.numeric.feature.names = reactive({
  reqAndAssign(task(), "tsk")
  d = data$data
  names = colnames(d[vlapply(d, is.numeric)])
  setdiff(names, getTaskTargetNames(tsk))
})

task.factor.feature.names = reactive({
  reqAndAssign(task(), "tsk")
  d = data$data
  names = colnames(d[vlapply(d, is.factor)])
  setdiff(names, getTaskTargetNames(tsk))
})

task.out = reactive({
  validateData(data$data)
  validateTask(input$create.task, task.data(), data$data,
    task.weights = isolate(input$task.weights))
  tsk = task()
  validate(need("Task" %in% class(tsk), tsk))
  tsk
})

output$task.overview = renderPrint({
  printTaskOverviewUI(task.out())
})
