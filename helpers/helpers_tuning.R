makeTuningParameterUI = function(par.set, param.ids, param.types) {
  param.lows = Map(function(param) {par.set$pars[[param]]$lower}, param.ids)
  param.ups = Map(function(param) {par.set$pars[[param]]$upper}, param.ids)
  params.vals = Map(function(param) {par.set$pars[[param]]$values}, param.ids)


  param.ui = Map(function(param, param.type, param.low, param.up, param.vals) {
    par = par.set$pars[[param]]

    if (param.type %in% c("numeric", "integer")) {

      if (param.type == "numeric") {
        tune.par.lower = numericInput(inputId = paste0("tune.par.lower.", param),
          label = paste("Lower value for", param),
          value = param.low, min = param.low, max = param.up)
        tune.par.upper = numericInput(inputId = paste0("tune.par.upper.", param),
          label = paste("Upper value for", param),
          value = param.up, min = param.low, max = param.up)
        tune.par.trafo = radioButtons(inputId = paste0("tune.par.trafo.", param),
          label = "Trafo", choices = c("linear", "log2", "log10"), selected = "linear", inline = TRUE)
      } else if (param.type == "integer") {
        tune.par.lower = numericInput(inputId = paste0("tune.par.lower.", param), label = paste("Lower value for", param),
          value = param.low, min = param.low, max = param.up, step = 1L)
        tune.par.upper = numericInput(inputId = paste0("tune.par.upper.", param), label = paste("Upper value for", param),
          value = param.up, min = param.low, max = param.up, step = 1L)
        tune.par.trafo = radioButtons(inputId = paste0("tune.par.trafo.", param),
          label = "Trafo", choices = c("linear", "log2", "log10"), selected = "linear", inline = TRUE)
      }

      par.info.ui = makeLearnerParamInfoUI(par, inline = FALSE)
      pars1 = box(width = 12, height = 175, title = param, solidHeader = TRUE, status = "warning",
        fluidRow(width = 12,
          column(12, div(height = "130px"), par.info.ui),
          column(4, div(height = "130px"), tune.par.lower),
          column(4, div(height = "130px"), tune.par.upper),
          column(4, div(height = "130px"), tune.par.trafo)
        )
      )

      return(pars1)

    } else if (param.type == "discrete") {
      param.vals = lapply(param.vals, as.character)
      discrete.box = checkboxGroupInput(inputId = paste0("tune.par.checkbox", param),
        label = "Values", choices = param.vals, selected = param.vals, inline = TRUE)

      par.info.ui = makeLearnerParamInfoUI(par)
      pars2 = box(width = 12, height = 120, title = param, solidHeader = TRUE, status = "primary",
        fluidRow(width = 12,
          column(7, align = "center", div(height = "130px"), discrete.box),
          column(5, align = "center", div(height = "130px"), par.info.ui)
        )
      )

      return(pars2)

    }

  }, param.ids, param.types, param.lows, param.ups, params.vals)

  column(width = 12,
    param.ui
  )
}

