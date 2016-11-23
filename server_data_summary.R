##### data summary #####

output$summary.datatable = renderDataTable({
  req(data())
  d = data()
  colnames(d) = make.names(colnames(d))
  summarizeColumns(d)
}, options = list(lengthMenu = c(5, 20, 50), pageLength = 5)
)


output$summary.vis.var = renderUI({
  req(data())
  d = data()
  choices = as.list(colnames(data()))
  selectInput("summary.vis.var", "Choose a variable:", choices = choices, selected = getLast(choices), width = "95%")
})

output$summary.vis.hist.nbins = renderUI({
  sliderInput("summary.vis.hist.nbins", "Number of bins", min = 1L, max = 100L, value = 30L, step = 1L, width = "95%")
})


factorvars = reactive({
  req(data())
  d = data()
  colnames(d[sapply(d, is.factor)])
})

observeEvent(input$summary.vis.var, {
  req(factorvars())
  req(input$summary.vis.var)
  if (input$summary.vis.var %in% factorvars()) {
    shinyjs::hide("summary.vis.hist.nbins", animType = "slide")
  } else {
    shinyjs::show("summary.vis.hist.nbins", anim = TRUE)
  }
})


output$summary.vis = renderPlot({
  req(input$summary.vis.var)
  req(data())
  d = na.omit(data())
  factors = sapply(d, is.factor)
  numerics = sapply(d, is.numeric)
  factor_ch = colnames(d[factors])
  num_ch = colnames(d[numerics])
  if (input$summary.vis.var %in% num_ch) {
    ggplot(data = d, aes(x = as.numeric(d[,input$summary.vis.var]))) + 
      geom_histogram(aes(y = ..density..), fill = "white", color = "black", stat = "bin", bins = input$summary.vis.hist.nbins) + 
      geom_density(fill = "blue", alpha = 0.1) + xlab(input$summary.vis.var) +
      geom_vline(aes(xintercept = quantile(as.numeric(d[,input$summary.vis.var]), 0.25)), color = "blue", size = 0.5, linetype = "dashed") +
      geom_vline(aes(xintercept = quantile(as.numeric(d[,input$summary.vis.var]), 0.75)), color = "blue", size = 0.5, linetype = "dashed") +
      geom_vline(aes(xintercept = quantile(as.numeric(d[,input$summary.vis.var]), 0.5)), color = "blue", size = 1, linetype = "dashed")
  } else {
    ggplot(data = d, aes(x = d[,input$summary.vis.var])) + 
      geom_bar(aes(fill = d[,input$summary.vis.var]), stat = "count") + xlab(input$summary.vis.var) +
      guides(fill = FALSE)
  }
})  



##### preprocessing #####

output$preproc_target = renderUI({
  req(data())
  choices = as.list(colnames(data()))
  selectInput("preproc_target", "Choose a target:", choices =  choices, selected = getLast(choices))
})


### Impute

output$preproc_impute = renderUI({
  req(data())
  req(input$preproc_method)
  d = data()
  # box(width = 12,
    fluidRow(
      column(6, 
        conditionalPanel("input.preproc_method == 'Impute'",
          selectInput("impute_methods_num", "Choose imputation method for numeric variables", selected = "imputeMean",
            choices = c("imputeConstant", "imputeMean", "imputeMedian", "imputeMode", "imputeMin", "imputeMax", "imputeNormal", "imputeHist")
          )
        )
      ),
      column(6,
        conditionalPanel("input.impute_methods_num == 'imputeConstant'",
          numericInput("impute_constant_num_input", "Constant value for numerical features:", min = -Inf,  max = Inf, value = 0)
        )
      ),
      column(6,
        conditionalPanel("input.preproc_method == 'Impute'",
          selectInput("impute_methods_fac", "Choose imputation method for factor variables", selected = "imputeMode",
            choices = c("imputeConstant", "imputeMode")#, "imputeMin", "imputeMax")
          )
        )
      ),
      column(6,
        conditionalPanel("input.impute_methods_fac == 'imputeConstant'",
          numericInput("impute_constant_fac_input", "Constant value for factors:", min = -Inf,  max = Inf, value = 0)
        )
      ),
      column(12,
        conditionalPanel("input.preproc_method == 'Impute'",
        actionButton("impute_start", "Start imputation")
        )
      )
    )
  # )
})

impute_data = eventReactive(input$impute_start, {
  req(data())
  d = data()
  req(input$impute_methods_num)
  req(input$impute_methods_fac)
  num = input$impute_methods_num
  fac = input$impute_methods_fac
  
  if (num == "imputeConstant" ) {
    num_impute = imputeConstant(input$impute_constant_num_input)
  } else {
    num_impute = match.fun(input$impute_methods_num)()
  }
    
  if (fac == "imputeConstant" ) {
    fac_impute = imputeConstant(input$impute_constant_fac_input)
  } else {
    fac_impute = match.fun(input$impute_methods_fac)()
  }   
  
  imputed = impute(d, target = input$preproc_target, classes = list(numeric = num_impute, factor = fac_impute))
  return(imputed$data)
})

output$impute_datatable = renderDataTable({
  req(impute_data())
  d = impute_data()
  colnames(d) = make.names(colnames(d))
  d
}, options = list(lengthMenu = c(5, 20, 50), pageLength = 5)
)

preproc_method = reactive(input$preproc_method)
observeEvent(preproc_method(), {
  if (preproc_method() != "Impute") {
    shinyjs::hide("impute_datatable")
  } else {
    shinyjs::show("impute_datatable")
  }
})


### createDummyFeature

output$preproc_createdummy = renderUI({
  req(data())
  choices = as.list(colnames(data()))
  req(input$preproc_method)
  fluidRow(
    conditionalPanel("input.preproc_method == 'createDummyFeatures'",
      column(6,
        selectInput("createdummy_method", "Choose Method", selected = "1-of-n", choices = c("1-of-n", "reference"))
      ),
      column(6, 
        selectizeInput("createdummy_cols", "Choose columns (optional)", choices = choices, multiple = TRUE)
      ),
      column(12,
        actionButton("createdummy_start", "create Dummy Feature")
      )
    )
  )
})

createdummy_data = eventReactive(input$createdummy_start, {
  req(data())
  d = data()
  createDummyFeatures(d, target = input$preproc_target, method = input$createdummy_method, cols = input$createdummy_cols)
})

output$createdummy_datatable = renderDataTable({
  req(createdummy_data())
  d = createdummy_data()
  colnames(d) = make.names(colnames(d))
  d
}, options = list(lengthMenu = c(5, 20, 50), pageLength = 5)
)


observeEvent(preproc_method(), {
  if (preproc_method() != "dropFeatures") {
    shinyjs::hide("dropfeature_datatable")
  } else {
    shinyjs::show("dropfeature_datatable")
  }
})


### dropFeature

output$preproc_dropfeature = renderUI({
  req(data())
  req(input$preproc_method)
  fluidRow(
    conditionalPanel("input.preproc_method == 'dropFeatures'",
      actionButton("dropfeature_start", "Drop Feature"))
  )
})

dropfeature_data = eventReactive(input$dropfeature_start, {
  req(data())
  d = data()
  dropNamed(d, input$preproc_target)
})

output$dropfeature_datatable = renderDataTable({
  req(dropfeature_data())
  d = dropfeature_data()
  colnames(d) = make.names(colnames(d))
  d
}, options = list(lengthMenu = c(5, 20, 50), pageLength = 5)
)


observeEvent(preproc_method(), {
  if (preproc_method() != "dropFeatures") {
    shinyjs::hide("dropfeature_datatable")
  } else {
    shinyjs::show("dropfeature_datatable")
  }
})

