##### data summary #####

# numeric variables
numericFeatures = reactive({
  req(data$data)
  d = data$data
  return(colnames(d[vlapply(d, is.numeric)]))
})

# factor variables
factorFeatures = reactive({
  req(data$data)
  d = data$data
  return(colnames(d[vlapply(d, is.factor)]))
})



output$summary.datatable = renderDataTable({
  req(data$data)
  d = data$data
  colnames(d) = make.names(colnames(d))
  summarizeColumns(d)
}, options = list(lengthMenu = c(5, 20, 50), pageLength = 5)
)


output$summary.vis.var = renderUI({
  req(data$data)
  d = data$data
  choices = as.list(colnames(data$data))
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


output$summary.vis = renderPlot({
  req(input$summary.vis.var)
  req(data$data)
  req(numericFeatures())
  d = na.omit(data$data)
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
  req(data$data)
  choices = as.list(c("", colnames(data$data)))
   conditionalPanel("input.preproc_method != 'Remove constant variables'",
     selectInput("preproc_target", "Choose a target:", choices =  choices)
   )
})


### Impute

output$preproc_impute = renderUI({
  req(data$data)
  req(input$preproc_method)
  d = data$data
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
      column(12, align = "center",
        conditionalPanel("input.preproc_method == 'Impute'",
        actionButton("impute_start", "Start imputation")
        )
      )
    )
})

observeEvent(input$impute_start, {
  req(data$data)
  d = data$data
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
  
  imputed = impute(d, target = preproc_target(), classes = list(numeric = num_impute, factor = fac_impute))
  data$data = imputed$data
})

preproc_target = reactive({
  tar = input$preproc_target
  if (is.null(tar) | tar == "") {
    return(character(0L))
  } else {
    tar
  }
})


### createDummyFeatures

output$preproc_createdummy = renderUI({
  req(data$data)
  d = data$data
  choices = factorFeatures()
  req(input$preproc_method)
  fluidRow(
    conditionalPanel("input.preproc_method == 'Create dummy features'",
      column(6,
        selectInput("createdummy_method", "Choose Method", selected = "1-of-n", choices = c("1-of-n", "reference"))
      ),
      column(6, 
        selectInput("createdummy_cols", "Choose columns (optional)", choices = choices, multiple = TRUE)
      ),
      column(12, align = "center",
        actionButton("createdummy_start", "Create dummy features")
      )
    )
  )
})

observeEvent(input$createdummy_start, {
  req(data$data)
  d = data$data
  data$data = createDummyFeatures(d, target = preproc_target(), method = input$createdummy_method, cols = input$createdummy_cols)
})


### dropFeature

output$preproc_dropfeature = renderUI({
  req(data$data)
  req(input$preproc_method)
  fluidRow(
    conditionalPanel("input.preproc_method == 'Drop variable'",
      column(12, align = "center",
        actionButton("dropfeature_start", "Drop variable")
      )
    )
  )
})

observeEvent(input$dropfeature_start, {
  req(data$data)
  d = data$data
  data$data = dropNamed(d, preproc_target())
})


### removeConstantFeatures


output$preproc_remconst = renderUI({
  req(data$data)
  d = data$data
  choices = as.list(colnames(d))
  req(input$preproc_method)
  fluidRow(
    conditionalPanel("input.preproc_method == 'Remove constant variables'",
      column(6,
        sliderInput("remconst_perc", "Choose % of feat. values different from mode", value = 0L, min = 0L, max = 1L, step = 0.01)
      ),
      column(6, 
        selectInput("remconst_cols", "Choose columns which must not deleted", choices = choices, multiple = TRUE)
      ),
      column(6, 
        radioButtons("remconst_na", "Ignore NAs in %-calculation?", choices = c("TRUE", "FALSE"), selected = "FALSE")
      ),
      column(12, align = "center",
        actionButton("remconst_start", "Remove constant variables")
      )
    )
  )
})


observeEvent(input$remconst_start, {
  req(data$data)
  d = data$data
  data$data = removeConstantFeatures(d, perc = input$remconst_perc, dont.rm = input$remconst_cols, na.ignore = as.logical(input$remconst_na))
})


### normalizeFeatures


output$preproc_normfeat = renderUI({
  req(data$data)
  d = data$data
  choices = numericFeatures()
  req(input$preproc_method)
  fluidRow(
    conditionalPanel("input.preproc_method == 'Normalize variables'",
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
      column(12, align = "center",
        actionButton("normfeat_start", "Normalize variables")
      )
    )
  )
})


observeEvent(input$normfeat_start, {
  req(data$data)
  d = data$data
  data$data = normalizeFeatures(d, target = preproc_target(), method = input$normfeat_method, cols = input$normfeat_cols,
    range = input$normfeat_range, on.constant = input$normfeat_on_constant)
})


### capLargeValues


output$preproc_caplarge = renderUI({
  req(input$preproc_method)
  d = data$data
  max = max(d[vlapply(d, is.numeric)])
  choices = numericFeatures()
  tr = input$caplarge_threshold
  
  if (!is.null(tr) && !is.na(tr)) {
    imp = tr
  } else {
    imp = NA
  }
  
  
  fluidRow(
    conditionalPanel("input.preproc_method == 'Cap large values'",
      column(6,
        numericInput("caplarge_threshold", "Choose threshold", value = imp)#FIXME threshold cannot be changed!
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
      column(12, align = "center",
        actionButton("caplarge_start", "Cap large values")
      )
    )
  )
})

# caplarge_threshold = reactive({
#   req(data$data)
#   # d = data$data
#   # max = max(d[vlapply(d, is.numeric)])
#   if (!is.na(input$caplarge_threshold)) #{
#     return(input$caplarge_threshold)
#   # } else {
#     # return(max)
#   }
# })

observeEvent(input$caplarge_start, {
  req(data$data)
  d = data$data
  tr = isolate(input$caplarge_threshold)
  if(is.na(tr))
    tr = Inf
  imp = isolate(input$caplarge_impute)
  if(is.na(imp))
    imp = Inf
  data$data = capLargeValues(d, target = preproc_target(), cols = isolate(input$caplarge_cols), threshold = tr,
    impute = imp, what = isolate(input$caplarge_what))
})


### preproc_data

output$preproc_data = renderDataTable({
  req(data$data)
  d = data$data
  colnames(d) = make.names(colnames(d))
  d
}, options = list(lengthMenu = c(5, 20, 50), pageLength = 5, scrollX = TRUE)
)






