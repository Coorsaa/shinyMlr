##### task #####

output$task.id = renderUI({
  # id = file_path_sans_ext(input$import.file$name)
  # FIXME: quickinit / remove later
  id = "iris"
  textInput("task.id", "Task ID", value = id)
})

output$task.target = renderUI({
  choices = as.list(colnames(data()))
  selectInput("task.target", "Choose a target:", choices = choices, selected = getLast(choices))
})

task = eventReactive(input$create.task, {
  d = isolate({data()})
  if (is.null(d)) return(NULL)
  colnames(d) = make.names(colnames(d)) 
  sMakeTask(input$task.id, input$task.target, d)
})

task.type = reactive({
  req(task())
  tsk = task()
  getTaskType(tsk)
})

target.levels = reactive({
  req(task())
  tsk = task()
  tsk.type = task.type()
  tar.levels = NULL
  if (tsk.type == "classif")
    tar.levels = getTaskClassLevels(tsk)
  return(tar.levels)
})

output$task.overview = renderPrint({
  validate(need(input$create.task != 0L, "you didn't create a task yet"))
  tsk = task()
  print(tsk)
})