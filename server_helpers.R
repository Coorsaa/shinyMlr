# FIXME: mlr: create makeAutoTask or whatever depending on target? 
sMakeTask = function(input, data) {
  id = input$task.id
  tn = input$task.target
  y = data[,tn]
  if (is.numeric(y))
    makeRegrTask(id = id, data = data, target = tn)
  else if (is.factor(y))
    makeClassifTask(id = id, data = data, target = tn)
}


# FIXME: maybe we wnat this as a helper too in mlr directly plot pd plot for one feature??
sPlotPartialDep = function(input, task, learners) {
  lrn = input$partialdep.learner
  lrn = learners[[lrn]]
  mod = train(lrn, task)
  fn = input$partialdep.feature
  pd = generatePartialDependenceData(mod, task, features = fn)
  plotPartialDependence(pd)
}

