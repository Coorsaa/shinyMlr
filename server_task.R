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

task = eventReactive(input$create.task, {
  req(data$data)
  d = isolate({na.omit(data$data)})
  # if (is.null(d)) return(NULL)
  colnames(d) = make.names(colnames(d)) 
  sMakeTask(input$task.id, input$task.target, d)
})

task.type = reactive({
  req(task())
  tsk = task()
  getTaskType(tsk)
})

target.levels = reactive({
  # req(task())
  tsk = task()
  tsk.type = task.type()
  tar.levels = NULL
  if (tsk.type == "classif")
    tar.levels = getTaskClassLevels(tsk)
  return(tar.levels)
})

task.data = reactive({
  getTaskData(task())
})

output$task.overview = renderPrint({
  validateTask(input$create.task, task.data(), data$data)
  tsk = task()
  print(tsk)
})

task.is.consistent = reactive({
  identical(task.data(), data$data)
})
