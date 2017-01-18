##### data summary #####

# numeric variables
numericFeatures = reactive({
  # req(data$data)
  d = data$data
  return(colnames(d[vlapply(d, is.numeric)]))
})

# factor variables
factorFeatures = reactive({
  # req(data$data)
  d = data$data
  return(colnames(d[vlapply(d, is.factor)]))
})



output$summary.datatable = DT::renderDataTable({
  reqAndAssign(data$data, "d")
  colnames(d) = make.names(colnames(d))
  summarizeColumns(d)
}, caption = "Click on variable for visualisation!", selection = "single")


summary.vis.var = reactive({
  reqAndAssign(data$data, "d")
  s = summarizeColumns(d)
  s$name[input$summary.datatable_rows_selected]
})

output$summary.vis.hist.nbins = renderUI({
  sliderInput("summary.vis.hist.nbins", "Number of bins", min = 1L, max = 100L, value = 30L, step = 1L, width = "95%")
})

observeEvent(summary.vis.var(), {
  feature = summary.vis.var()
  if (length(feature) > 0L) {
    shinyjs::show("summary.vis.box", anim = TRUE)
    if (feature %in% factorFeatures()) {
      shinyjs::hide("summary.vis.hist.nbins", animType = "fade")
    } else {
      shinyjs::show("summary.vis.hist.nbins", anim = TRUE)
    }
  } else {
    shinyjs::hide("summary.vis.box", anim = TRUE)
  }
})


output$summary.vis = renderPlot({
  reqAndAssign(summary.vis.var(), "feature")
  d = na.omit(data$data)
  if (feature %in% numericFeatures()) {
    ggplot(data = d, aes(x = as.numeric(d[,feature]))) + 
      geom_histogram(aes(y = ..density..), fill = "white", color = "black", stat = "bin", bins = input$summary.vis.hist.nbins) + 
      geom_density(fill = "blue", alpha = 0.1) + xlab(feature) +
      geom_vline(aes(xintercept = quantile(as.numeric(d[,feature]), 0.05)), color = "blue", size = 0.5, linetype = "dashed") +
      geom_vline(aes(xintercept = quantile(as.numeric(d[,feature]), 0.95)), color = "blue", size = 0.5, linetype = "dashed") +
      geom_vline(aes(xintercept = quantile(as.numeric(d[,feature]), 0.5)), color = "blue", size = 1, linetype = "dashed")
  } else {
    ggplot(data = d, aes(x = d[,feature])) + 
      geom_bar(aes(fill = d[,feature]), stat = "count") + xlab(feature) +
      guides(fill = FALSE)
  }
})  



##### preprocessing #####

### Impute

output$preproc_impute = renderUI({
  req(input$preproc_method)
  reqAndAssign(data$data, "d")
  fluidRow(
    conditionalPanel("input.preproc_method == 'Impute'",
      column(6,
        selectInput("impute_exclude", "Exclude column(s) (optional)", choices =  as.list(colnames(d)), multiple = TRUE)
      ),
      column(6,
        selectInput("impute_methods_num", "Choose imputation method for numeric variables", selected = "imputeMean",
          choices = c("imputeConstant", "imputeMean", "imputeMedian", "imputeMode", "imputeMin", "imputeMax", "imputeNormal", "imputeHist")
        )
      ),
      conditionalPanel("input.impute_methods_num == 'imputeConstant'",
        column(6,
          numericInput("impute_constant_num_input", "Constant value for numerical features", min = -Inf,  max = Inf, value = 0)
        )
      ),
      column(6,
        selectInput("impute_methods_fac", "Choose imputation method for factor variables", selected = "imputeMode",
          choices = c("imputeConstant", "imputeMode")
        )
      ),
      conditionalPanel("input.impute_methods_fac == 'imputeConstant'",
        column(6,
          numericInput("impute_constant_fac_input", "Constant value for factors", min = -Inf,  max = Inf, value = 0)
        )
      ),
      column(12, align = "center",
        actionButton("impute_start", "Start imputation")
      )
    )
  )
})

observeEvent(input$impute_start, {
  data$data_old = data$data
  d = data$data
  reqAndAssign(input$impute_methods_num, "num")
  reqAndAssign(input$impute_methods_fac, "fac")
  
  if (num == "imputeConstant" ) {
    num_impute = imputeConstant(input$impute_constant_num_input)
  } else {
    num_impute = match.fun(num)()
  }
    
  if (fac == "imputeConstant" ) {
    fac_impute = imputeConstant(input$impute_constant_fac_input)
  } else {
    fac_impute = match.fun(fac)()
  }   
  
  imputed = impute(d, target = impute_target(), classes = list(numeric = num_impute, factor = fac_impute))
  data$data = imputed$data
})

impute_target = reactive({
  tar = input$impute_exclude
  ifelse (is.null(tar) | tar == "", character(0L), tar)
})


### createDummyFeatures

output$preproc_createdummy = renderUI({
  reqAndAssign(data$data, "d")
  req(input$preproc_method)
  choices = factorFeatures()
  fluidRow(
    conditionalPanel("input.preproc_method == 'Create dummy features'",
      column(6,
        conditionalPanel("input.createdummy_cols == null",
          selectInput("createdummy_exclude", "Exclude column(s) (optional)", choices = choices, multiple = TRUE)
        )
      ),
      column(6,
        selectInput("createdummy_method", "Choose Method", selected = "1-of-n", choices = c("1-of-n", "reference"))
      ),
      column(6,
        conditionalPanel("input.createdummy_exclude == null", 
          selectInput("createdummy_cols", "Choose specific column(s) (optional)", choices = choices, multiple = TRUE)
        )
      ),
      column(12, align = "center",
        actionButton("createdummy_start", "Create dummy features")
      )
    )
  )
})


createdummy_target = reactive({
  tar = input$createdummy_exclude
  ifelse (is.null(tar) | tar == "", character(0L), tar)
})

observeEvent(input$createdummy_start, {
  data$data_old = data$data
  d = data$data
  data$data = createDummyFeatures(d, target = createdummy_target(), method = input$createdummy_method, cols = input$createdummy_cols)
})


### dropFeature

output$preproc_dropfeature = renderUI({
  d = data$data
  req(input$preproc_method)
  fluidRow(
    conditionalPanel("input.preproc_method == 'Drop variable(s)'",
      column(6,
        selectInput("dropfeature_cols", "Choose column(s)", choices =  as.list(colnames(d)), multiple = TRUE)
      ),
      column(12, align = "center",
        actionButton("dropfeature_start", "Drop variable(s)")
      )
    )
  )
})

dropfeature_target = reactive({
  tar = input$dropfeature_cols
  ifelse (is.null(tar) | tar == "", character(0L), tar)
})

observeEvent(input$dropfeature_start, {
  data$data_old = data$data
  d = data$data
  data$data = dropNamed(d, dropfeature_target())
})


### removeConstantFeatures


output$preproc_remconst = renderUI({
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
  data$data_old = data$data
  d = data$data
  data$data = removeConstantFeatures(d, perc = input$remconst_perc, dont.rm = input$remconst_cols, na.ignore = as.logical(input$remconst_na))
})


### normalizeFeatures


output$preproc_normfeat = renderUI({
  d = data$data
  choices = numericFeatures()
  req(input$preproc_method)
  fluidRow(
    conditionalPanel("input.preproc_method == 'Normalize variables'",
      column(6,
        conditionalPanel("input.normfeat_cols == null",
          selectInput("normfeat_exclude", "Exclude column(s) (optional)", choices = choices, multiple = TRUE)
        )
      ),
      column(6,
        conditionalPanel("input.normfeat_exclude == null",
          selectInput("normfeat_cols", "Choose columns (optional)", choices = choices, multiple = TRUE)
        )
      ),
      column(6,
        selectInput("normfeat_method", "Choose method", selected = "standardize", choices = c("center", "scale", "standardize", "range"))
      ),
      column(6, 
        conditionalPanel("input.normfeat_method == 'range'",
          sliderInput("normfeat_range", "Choose range", min = -10L, max = 10L, value = c(0, 1), round = TRUE, step = 1L) #FIXME What would be the best range?
        )  
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

normfeat_target = reactive({
  tar = input$normfeat_exclude
  ifelse (is.null(tar) | tar == "", character(0L), tar)
})

observeEvent(input$normfeat_start, {
  data$data_old = data$data
  d = data$data
  data$data = normalizeFeatures(d, target = normfeat_target(), method = input$normfeat_method, cols = input$normfeat_cols,
    range = input$normfeat_range, on.constant = input$normfeat_on_constant)
})


### capLargeValues


output$preproc_caplarge = renderUI({
  req(input$preproc_method)
  d = data$data
  choices = numericFeatures()
  tr = input$caplarge_threshold
  exc = input$caplarge_exclude
  cols = input$caplarge_cols
  what = input$caplarge_what
  
  if (!is.null(tr) && !is.na(tr)) {
    imp = tr
  } else {
    imp = NA
  }
  
  if (is.null(exc) || is.na(exc))
    exc = NA
  if (is.null(cols) || is.na(cols))
    cols = NA
  if (is.null(what) || is.na(what)) 
    what = "abs"
  
  
  fluidRow(
    conditionalPanel("input.preproc_method == 'Cap large values'",
      column(6,
        conditionalPanel("input.caplarge_cols == null",
          selectInput("caplarge_exclude", "Exclude column(s) (optional)", choices = choices, selected = exc, multiple = TRUE)
        )
      ),
      column(6,
        conditionalPanel("input.caplarge_exclude == null",
          selectInput("caplarge_cols", "Choose columns (optional)", choices = choices, selected = cols, multiple = TRUE)
        )
      ),
      column(6,
        numericInput("caplarge_threshold", "Choose threshold", value = imp)
      ),
      column(6, 
        numericInput("caplarge_impute", "Choose impute value (optional)", value = tr)
      ),
      column(6, 
        selectInput("caplarge_what", "What kind of entries are affected?", selected = what, choices = c("abs", "pos", "neg"))
      ),
      column(12, align = "center",
        actionButton("caplarge_start", "Cap large values")
      )
    )
  )
})

caplarge_target = reactive({
  tar = input$caplarge_exclude
  ifelse (is.null(tar) | tar == "", character(0L), tar)
})

observeEvent(input$caplarge_start, {
  data$data_old = data$data
  d = data$data
  tr = isolate(input$caplarge_threshold)
  if (is.na(tr))
    tr = Inf
  imp = isolate(input$caplarge_impute)
  if (is.na(imp))
    imp = Inf
  data$data = capLargeValues(d, target = caplarge_target(), cols = isolate(input$caplarge_cols), threshold = tr,
    impute = imp, what = isolate(input$caplarge_what))
})


### convert columns


output$preproc_convar = renderUI({
  req(input$preproc_method)
  d = data$data
  fluidRow(
    conditionalPanel("input.preproc_method == 'Convert variable'",
      column(6,
        selectInput("convar_cols", "Choose column", choices = as.list(colnames(d)), multiple = FALSE)
      ),
      column(6,
        selectInput("convar_type", "Convert to", choices = c("numeric", "factor", "integer"))
      ),
      column(12, align = "center",
        actionButton("convar_start", "Convert variable(s)")
      )
    )
  )
})

convar_target = reactive({
  tar = input$convar_cols
  ifelse (is.null(tar) | tar == "", character(0L), tar)
})

observeEvent(input$convar_start, {
  data$data_old = data$data
  type = input$convar_type
  
  if (type == "numeric")
    data$data[,convar_target()] = as.numeric(data$data[,convar_target()])
  
  if (type == "factor")
    data$data[,convar_target()] = as.factor(data$data[,convar_target()])
  
  if (type == "integer")
    data$data[,convar_target()] = as.integer(data$data[,convar_target()])
})


### preproc_data

output$preproc_data = DT::renderDataTable({
  d = data$data
  colnames(d) = make.names(colnames(d))
  d
}, options = list(lengthMenu = c(5, 20, 50), pageLength = 5, scrollX = TRUE)
)

### undo

observeEvent(input$preproc_undo, {
  req(data$data_old)
  data$data = data$data_old
})






