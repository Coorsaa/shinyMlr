##### task #####

output$task.id = renderUI({
  req(data.name)
  id = data.name()
  textInput("task.id", "Task ID", value = id)
})

output$task.target = renderUI({
  choices = as.list(colnames(data$data))
  selectInput("task.target", "Choose a target:", choices = choices, selected = getLast(choices))
})


task.object = reactiveValues(task = NULL, task_old = NULL)

observeEvent(input$create.task, {
  req(data$data)
  d = data$data
  colnames(d) = make.names(colnames(d)) 
  task.object$task = sMakeTask(input$task.id, input$task.target, d)
})

task = reactive({
  reqAndAssign(task.object$task, "tsk")
  tsk
})

task.type = reactive({
  reqAndAssign(task(), "tsk")
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
  validateTask(input$create.task, task.data(), data$data)
  tsk = task()
  tsk
})

output$task.overview = renderPrint({
  # validateTask(input$create.task, task.data(), data$data)
  # tsk = task()
  print(task.out())
})

task.is.consistent = reactive({
  identical(task.data(), data$data)
})
