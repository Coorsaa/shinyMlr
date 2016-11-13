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

observeEvent(input$summary.vis.var, {
  req(factorFeatures())
  req(input$summary.vis.var)
  if (input$summary.vis.var %in% factorFeatures()) {
    shinyjs::hide("summary.vis.hist.nbins", animType = "slide")
  } else {
    shinyjs::show("summary.vis.hist.nbins", anim = TRUE)
  }
})


factorvars = reactive({
  req(data())
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
  req(numericFeatures())
  d = na.omit(data())
  if (input$summary.vis.var %in% numericFeatures()) {
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
   conditionalPanel("input.preproc_method != 'removeConstantFeatures'",
     selectInput("preproc_target", "Choose a target:", choices =  choices, selected = getLast(choices))
   )
})


### Impute

output$preproc_impute = renderUI({
  req(data())
  req(input$preproc_method)
  d = data()
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
            choices = c("imputeConstant", "imputeMode")
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


### createDummyFeatures

output$preproc_createdummy = renderUI({
  req(data())
  d = data()
  choices = factorFeatures()
  req(input$preproc_method)
  fluidRow(
    conditionalPanel("input.preproc_method == 'createDummyFeatures'",
      column(6,
        selectInput("createdummy_method", "Choose Method", selected = "1-of-n", choices = c("1-of-n", "reference"))
      ),
      column(6, 
        selectInput("createdummy_cols", "Choose columns (optional)", choices = choices, multiple = TRUE)
      ),
      column(12,
        actionButton("createdummy_start", "Create dummy features")
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
      column(12,
        actionButton("dropfeature_start", "Drop Feature")
      )
    )
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


### removeConstantFeatures


output$preproc_remconst = renderUI({
  req(data())
  d = data()
  choices = as.list(colnames(d))
  req(input$preproc_method)
  fluidRow(
    conditionalPanel("input.preproc_method == 'removeConstantFeatures'",
      column(6,
        sliderInput("remconst_perc", "Choose % of feat. values different from mode", value = 0L, min = 0L, max = 1L, step = 0.01)
      ),
      column(6, 
        selectInput("remconst_cols", "Choose columns which must not deleted", choices = choices, multiple = TRUE)
      ),
      column(6, 
        radioButtons("remconst_na", "Ignore NAs in %-calculation?", choices = c("TRUE", "FALSE"), selected = "FALSE")
      ),
      column(12,
        actionButton("remconst_start", "Remove constant features")
      )
    )
  )
})


remconst_data = eventReactive(input$remconst_start, {
  req(data())
  d = data()
  removeConstantFeatures(d, perc = input$remconst_perc, dont.rm = input$remconst_cols, na.ignore = as.logical(input$remconst_na))
})

output$remconst_datatable = renderDataTable({
  req(remconst_data())
  d = remconst_data()
  colnames(d) = make.names(colnames(d))
  d
}, options = list(lengthMenu = c(5, 20, 50), pageLength = 5)
)


observeEvent(preproc_method(), {
  if (preproc_method() != "removeConstantFeatures") {
    shinyjs::hide("remconst_datatable")
  } else {
    shinyjs::show("remconst_datatable")
  }
})



### normalizeFeatures


output$preproc_normfeat = renderUI({
  req(data())
  d = data()
  choices = numericFeatures()
  req(input$preproc_method)
  fluidRow(
    conditionalPanel("input.preproc_method == 'normalizeFeatures'",
      column(6,
        selectInput("normfeat_method", "Choose method", selected = "standardize", choices = c("center", "scale", "standardize", "range"))
      ),
      column(6, 
        conditionalPanel("input.normfeat_method == 'range'",
          sliderInput("normfeat_range", "Choose range", min = -10L, max = 10L, value = c(0, 1), round = TRUE, step = 1L) #FIXME What would be the best range?
        )  
      ),
      column(6, 
        selectInput("normfeat_cols", "Choose columns (optional)", choices = choices, multiple = TRUE)
      ),
      column(6, 
        conditionalPanel("input.normfeat_method != 'center'",
          selectInput("normfeat_on_constant", "How should constant vectors be treated?", selected = "quiet",
            choices = c("quiet", "warn", "stop"))
        )
      ),
      column(12,
        actionButton("normfeat_start", "Normalize features")
      )
    )
  )
})


normfeat_data = eventReactive(input$normfeat_start, {
  req(data())
  d = data()
  normalizeFeatures(d, target = input$preproc_target, method = input$normfeat_method, cols = input$normfeat_cols,
    range = input$normfeat_range, on.constant = input$normfeat_on_constant)
})

output$normfeat_datatable = renderDataTable({
  req(normfeat_data())
  d = normfeat_data()
  colnames(d) = make.names(colnames(d))
  d
}, options = list(lengthMenu = c(5, 20, 50), pageLength = 5)
)


observeEvent(preproc_method(), {
  if (preproc_method() != "normalizeFeatures") {
    shinyjs::hide("normfeat_datatable")
  } else {
    shinyjs::show("normfeat_datatable")
  }
})


### capLargeValues


output$preproc_caplarge = renderUI({
  req(data())
  d = data()
  max = max(d[vlapply(d, is.numeric)])
  choices = numericFeatures()
  req(input$preproc_method)
  tr = caplarge_threshold()
  fluidRow(
    conditionalPanel("input.preproc_method == 'capLargeValues'",
      column(6,
        numericInput("caplarge_threshold", "Choose threshold", value = max)#FIXME threshold cannot be changed!
      ),
      column(6, 
        selectInput("caplarge_cols", "Choose columns (optional)", choices = choices, multiple = TRUE)
      ),
      column(6, 
        numericInput("caplarge_impute", "Choose impute value (optional)", value = tr)
      ),
      column(6, 
        selectInput("caplarge_what", "What kind of entries are affected?", selected = "abs", choices = c("abs", "pos", "neg"))
      ),
      column(12,
        actionButton("caplarge_start", "Cap large values")
      )
    )
  )
})

caplarge_threshold = reactive({
  req(data())
  d = data()
  max = max(d[vlapply(d, is.numeric)])
  if (!is.null(input$caplarge_threshold)) {
    return(input$caplarge_threshold)
  } else {
    return(max)
  }
})

caplarge_data = eventReactive(input$caplarge_start, {
  req(data())
  d = data()
  capLargeValues(d, target = input$preproc_target, cols = input$caplarge_cols, threshold = input$caplarge_threshold,
    impute = input$caplarge_impute, what = input$caplarge_what)
})

output$caplarge_datatable = renderDataTable({
  req(caplarge_data())
  d = caplarge_data()
  colnames(d) = make.names(colnames(d))
  d
}, options = list(lengthMenu = c(5, 20, 50), pageLength = 5)
)


observeEvent(preproc_method(), {
  if (preproc_method() != "capLargeValues") {
    shinyjs::hide("caplarge_datatable")
  } else {
    shinyjs::show("caplarge_datatable")
  }
})


