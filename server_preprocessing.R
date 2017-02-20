preproc.data = reactiveValues(data = NULL, data.collection = NULL)

observe({
  req(counter$count < 2L)
  req(input$preproc_df)
  if (input$preproc_df == "training set" | is.null(input$preproc_df)) {
    preproc.data$data = data$data
    preproc.data$data.collection = list(data$data)
  } else {
    # validate(need(data$data.test, "You didn't upload a test set yet. Click on the 'train and predict' panel to do so."))
    preproc.data$data = data$data.test
    preproc.data$data.collection = list(data$data.test)
  }
})

### Impute

preproc_impute = reactive({
  req(input$preproc_method)
  reqAndAssign(preproc.data$data, "d")
  makePreprocUI(
    list(
      selectInput("impute_exclude", "Exclude column(s) (optional)",
        choices =  as.list(colnames(d)), multiple = TRUE),
      selectInput("impute_methods_num", "Choose imputation method for numeric variables",
        selected = "imputeMean",
        choices = c("imputeConstant", "imputeMean", "imputeMedian",
          "imputeMode", "imputeMin", "imputeMax", "imputeNormal", "imputeHist")
      ),
      selectInput("impute_methods_fac", "Choose imputation method for factor variables", selected = "imputeMode",
        choices = c("imputeConstant", "imputeMode"))
    ),
    list(
      NULL,
      conditionalPanel("input.impute_methods_num == 'imputeConstant'",
        numericInput("impute_constant_num_input", "Constant value for numerical features",
        min = -Inf,  max = Inf, value = 0)
      ),
      conditionalPanel("input.impute_methods_fac == 'imputeConstant'",
        numericInput("impute_constant_fac_input", "Constant value for factors", min = -Inf,  max = Inf, value = 0)
      )
    )
  )
})

observeEvent(input$preproc_go, {
  # reqAndAssign(preproc.data(), "d")
  req(input$preproc_method == "Impute")
  d = isolate(preproc.data$data)
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
  preproc.data$data = imputed$data
})

impute_target = reactive({
  tar = input$impute_exclude
  ifelse (is.null(tar) | tar == "", character(0L), tar)
})


## createDummyFeatures

preproc_createdummy = reactive({
  reqAndAssign(preproc.data$data, "d")
  req(input$preproc_method)
  choices = factorFeatures()
  validate(need(length(choices) > 0L, "No factor features available!"))
  makePreprocUI(
    selectInput("createdummy_method", "Choose Method", selected = "1-of-n",
      choices = c("1-of-n", "reference")),
    conditionalPanel("input.createdummy_cols == null",
      selectInput("createdummy_exclude", "Exclude column(s) (optional)",
        choices = choices, multiple = TRUE)
    ),
    conditionalPanel("input.createdummy_exclude == null",
      selectInput("createdummy_cols", "Choose specific column(s) (optional)",
        choices = choices, multiple = TRUE)
    )
  )
})

createdummy_target = reactive({
  tar = input$createdummy_exclude
  ifelse (is.null(tar) | tar == "", character(0L), tar)
})

observeEvent(input$preproc_go, {
  req(input$preproc_method == "Create dummy features")
  d = isolate(preproc.data$data)
  preproc.data$data = createDummyFeatures(d, target = createdummy_target(),
    method = input$createdummy_method, cols = input$createdummy_cols)
})


### dropFeature

preproc_dropfeature = reactive({
  d = preproc.data$data
  req(input$preproc_method)
  makePreprocUI(
    selectInput("dropfeature_cols", "Choose column(s)",
      choices =  as.list(colnames(d)), multiple = TRUE)
  )
})

dropfeature_target = reactive({
  tar = input$dropfeature_cols
  ifelse (is.null(tar) | tar == "", character(0L), tar)
})

observeEvent(input$preproc_go, {
  req(input$preproc_method == "Drop variable(s)")
  d = isolate(preproc.data$data)
  preproc.data$data = dropNamed(d, dropfeature_target())
})


### removeConstantFeatures


preproc_remconst = reactive({
  d = isolate(preproc.data$data)
  choices = as.list(colnames(d))
  req(input$preproc_method)
  makePreprocUI(
    selectInput("remconst_cols", "Exclude columns (optional)",
      choices = choices, multiple = TRUE),
    sliderInput("remconst_perc", "Choose % of feat. values different from mode",
      value = 0L, min = 0L, max = 1L, step = 0.01),
    radioButtons("remconst_na", "Ignore NAs in %-calculation?",
      choices = c("yes", "no"), selected = "FALSE", inline = TRUE)

  )
})


observeEvent(input$preproc_go, {
  req(input$preproc_method == "Remove constant variables")
  # preproc.data$data_old = preproc.data$data
  d = isolate(preproc.data$data)
  na.ign = input$remconst_na == "yes"
  preproc.data$data = removeConstantFeatures(d, perc = input$remconst_perc,
    dont.rm = input$remconst_cols, na.ignore = na.ign)
})


### normalizeFeatures

preproc_normfeat = reactive({
  d = preproc.data$data
  choices = numericFeatures()
  req(input$preproc_method)
  makePreprocUI(
    list(
      conditionalPanel("input.normfeat_cols == null",
        selectInput("normfeat_exclude", "Exclude column(s) (optional)", choices = choices, multiple = TRUE)
      ),
      conditionalPanel("input.normfeat_exclude == null",
        selectInput("normfeat_cols", "Choose columns (optional)", choices = choices, multiple = TRUE)
      )
    ),
    list(
      selectInput("normfeat_method", "Choose method", selected = "standardize",
        choices = c("center", "scale", "standardize", "range")),
      # FIXME What would be the best range?
      conditionalPanel("input.normfeat_method == 'range'",
        sliderInput("normfeat_range", "Choose range", min = -10L, max = 10L,
          value = c(0, 1), round = TRUE, step = 1L)
      ),
      conditionalPanel("input.normfeat_method != 'center'",
        selectInput("normfeat_on_constant", "How should constant vectors be treated?", selected = "quiet",
          choices = c("quiet", "warn", "stop"))
      )
    )
  )
})

normfeat_target = reactive({
  tar = input$normfeat_exclude
  ifelse (is.null(tar) | tar == "", character(0L), tar)
})

observeEvent(input$preproc_go, {
  req(input$preproc_method == "Normalize variables")
  d = isolate(preproc.data$data)
  preproc.data$data = normalizeFeatures(d, target = normfeat_target(), method = input$normfeat_method, cols = input$normfeat_cols,
    range = input$normfeat_range, on.constant = input$normfeat_on_constant)
})

### capLargeValues

preproc_caplarge = reactive({
  req(input$preproc_method)
  d = preproc.data$data
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
  
  
  makePreprocUI(
    list(
      conditionalPanel("input.caplarge_cols == null",
        selectInput("caplarge_exclude", "Exclude column(s) (optional)",
          choices = choices, selected = exc, multiple = TRUE)
      ),
      conditionalPanel("input.caplarge_exclude == null",
        selectInput("caplarge_cols", "Choose columns (optional)",
          choices = choices, selected = cols, multiple = TRUE)
      )
    ),
    list(
      numericInput("caplarge_threshold", "Choose threshold", value = imp),
      numericInput("caplarge_impute", "Choose impute value (optional)", value = tr),
      selectInput("caplarge_what", "What kind of entries are affected?",
        selected = what, choices = c("abs", "pos", "neg"))
    )
  )
})

caplarge_target = reactive({
  tar = input$caplarge_exclude
  ifelse (is.null(tar) | tar == "", character(0L), tar)
})

observeEvent(input$preproc_go, {
  req(input$preproc_method == "Cap large values")
  d = isolate(preproc.data$data)
  tr = isolate(input$caplarge_threshold)
  if (is.na(tr))
    tr = Inf
  imp = isolate(input$caplarge_impute)
  if (is.na(imp))
    imp = Inf
  preproc.data$data = capLargeValues(d, target = caplarge_target(), cols = isolate(input$caplarge_cols), threshold = tr,
    impute = imp, what = isolate(input$caplarge_what))
})

### convert columns


preproc_convar = reactive({
  req(input$preproc_method)
  d = isolate(preproc.data$data)
  makePreprocUI(
    selectInput("convar_cols", "Choose column",
      choices = as.list(colnames(d)), multiple = FALSE),
    selectInput("convar_type", "Convert to",
      choices = c("numeric", "factor", "integer"))
  )
})

convar_target = reactive({
  tar = input$convar_cols
  ifelse (is.null(tar) | tar == "", character(0L), tar)
})

observeEvent(input$preproc_go, {
  req(input$preproc_method == "Convert variable")
  type = input$convar_type
  
  if (type == "numeric")
    preproc.data$data[,convar_target()] = as.numeric(preproc.data$data[,convar_target()])
  
  if (type == "factor")
    preproc.data$data[,convar_target()] = as.factor(preproc.data$data[,convar_target()])
  
  if (type == "integer")
    preproc.data$data[,convar_target()] = as.integer(preproc.data$data[,convar_target()])
})

### subset

preproc_subset = reactive({
  req(input$preproc_method)
  d = isolate(preproc.data$data)
  # method = subset.method()
  makePreprocUI(
    radioButtons("preproc_subset_method", "Type of subset",
      choices = c("Random", "Fix"), selected = "Random", inline = TRUE),
    conditionalPanel("input.preproc_subset_method == 'Random'",
      numericInput("preproc.subset.nsamples", "No. of random samples", min = 1L,
        max = nrow(d), value = 2*ceiling(nrow(d)/3), step = 1L)
    ),
    conditionalPanel("input.preproc_subset_method == 'Fix'",
      sliderInput("preproc.subset", "Choose subset rows", min = 1L, max = nrow(d),
        value = c(1, 2*ceiling(nrow(d)/3)), step = 1L)
    )
  )
})

# subset.method = reactive({
#   method = input$preproc_subset_method
#   if (is.null(method))
#     return("Random")
#   else
#     method
# })

observeEvent(input$preproc_go, {
  req(input$preproc_method == "Subset")
  d = isolate(preproc.data$data)
  reqAndAssign(input$preproc_subset_method, "method")
  if (method == "Fix") {
    ss = input$preproc.subset
    preproc.data$data = d[seq(ss[1], ss[2]), ]
  } else {
    reqAndAssign(input$preproc.subset.nsamples, "n")
    preproc.data$data = d[sample(nrow(d), n), ]
  }
})


### Feature Selection (Filter methods)

filter.methods = reactive({
  listFilterMethods(tasks = TRUE)
})


preproc_feature_selection = reactive({
  req(input$preproc_method)
  reqAndAssign(task(), "tsk")
  tsk.type = tsk$type
  type = pasteDot("task", tsk.type)
  reqAndAssign(isolate(filter.methods()), "fm")
  fm.ids = as.character(fm[which(fm[, type]), "id"])
  d = preproc.data$data
  # filter = vi.filter()
  makePreprocUI(
    radioButtons("vi_abs_or_perc", "Absolute or percentage?", 
      choices = c("Absolute", "Percentage"), selected = "Absolute", inline = TRUE),
    conditionalPanel("input.vi_abs_or_perc == 'Absolute'",
      sliderInput("vi.abs", "Keep no. of most important features", min = 0L,
        max = getTaskNFeats(tsk), value = getTaskNFeats(tsk), step = 1L)
    ),
    conditionalPanel("input.vi_abs_or_perc == 'Percentage'",
      sliderInput("vi.perc", "Keep % of most important features", min = 0L,
        max = 100L, value = 100L, step = 1L)
    ),
    selectInput("vi.method", "Choose a filter method:",
      choices = fm.ids, selected = "randomForestSRC.rfsrc")
  )
})

# vi.filter = reactive({
#   method = input$vi.method
#   if (is.null(method))
#     return("randomForestSRC.rfsrc")
#   else
#     method
# })


vi.abs.or.perc = reactive(input$vi_abs_or_perc)

output$plot.feature.selection = renderPlot({
  reqAndAssign(task(), "tsk")
  tsk.type = tsk$type
  reqAndAssign(input$vi.method, "vi.method")
  vi.data = generateFilterValuesData(tsk, method = vi.method)
  plotFilterValues(vi.data)
})


preproc.method = reactive(input$preproc_method)


output$vi.task.check = renderPrint({
  validateTask(input$create.task, task.data(), data$data)
})

observeEvent(preproc.method(), {
  method = preproc.method()
  if (method %in% c("Feature selection", "Merge small factor levels")) {
    req(is.null(task.object$task))
    shinyjs::show("vi.task.check")
  } else {
    shinyjs::hide("vi.task.check")
  }  
})
  
observeEvent(task(),{
  shinyjs::hide("vi.task.check")
})


observeEvent(preproc.method(), {
  method = preproc.method()
  if (method == "Feature selection") {
    req(task())
    shinyjs::show("plot.feature.selection")
  } else {
    shinyjs::hide("plot.feature.selection")
  }
})

observeEvent(input$create.task, {
  method = preproc.method()
  if (method == "Feature selection")
    shinyjs::show("plot.feature.selection")
})


observeEvent(input$preproc_go, {
  req(input$preproc_method == "Feature selection")
  reqAndAssign(task(), "tsk")
  # task.object$task_old = tsk
  # data$data_old = data$data
  tsk.type = tsk$type
  reqAndAssign(input$vi.method, "method")
  reqAndAssign(vi.abs.or.perc(), "choice")
  
  if (choice == "Absolute") {
    abs = input$vi.abs
    filtered.task = filterFeatures(tsk, method = method, abs = abs)
  } else if (choice == "Percentage") {
    perc = input$vi.perc/100
    filtered.task = filterFeatures(tsk, method = method, perc = perc)
  }
  
  # task.object$task_old = task.object$task
  # task.object$task = filtered.task
  preproc.data$data = getTaskData(filtered.task)
})


### Merge small factor levels


preproc_merge_factor_levels = reactive({
  req(input$preproc_method)
  fnames = task.factor.feature.names()
  validate(need(length(fnames) > 0L, "No factor features available!"))
  makePreprocUI(
    selectInput("merge_factors_cols", "Choose column", choices = fnames,
      selected = getFirst(fnames), multiple = TRUE),
    sliderInput("merge_factors_min_perc", "% of combined proportion should be exceeded",
      min = 0L, max = 100L, value = 1L, step = 1L),
    textInput("merge_factors_new_lvl", "New name of merged level", value = ".merged"),
    go = actionButton("merge_factors_start", "Convert variable(s)")
  )
})

observeEvent(input$preproc_go, {
  req(input$preproc_method == "Merge small factor levels")
  reqAndAssign(task(), "tsk")
  reqAndAssign(input$merge_factors_cols, "cols")
  reqAndAssign(input$merge_factors_min_perc, "min.perc")
  reqAndAssign(input$merge_factors_new_lvl, "new.lvl")
  # data$data_old = data$data
  # task.object$task_old = task.object$task
  merged.task = mergeSmallFactorLevels(tsk, cols = cols, min.perc = min.perc/100, new.level = new.lvl)
  preproc.data$data = getTaskData(merged.task)
  # task.object$task = merged.task
})


# preproc.data = reactiveValues(data = NULL, data.collection = NULL)

counter = reactiveValues(count = 1L)

observeEvent(input$preproc_go, {
  df.type = isolate(input$preproc_df)
  preproc.df = isolate(preproc.data$data)

  preproc.data$data.collection = c(preproc.data$data.collection, list(preproc.df))
  counter$count = counter$count + 1L

  if (input$preproc_method %in% c("Merge small factor levels", "Feature selection")) {
    task.object$task = changeData(task.object$task, preproc.data$data)
  }

  if (df.type == "training set") {
    data$data = preproc.data$data
  } else {
    data$data.test = preproc.data$data
  }
})

observeEvent(input$preproc_undo, {
  req(counter$count > 1L)
  preproc.data$data = preproc.data$data.collection[[counter$count - 1L]]
  preproc.data$data.collection = preproc.data$data.collection[seq_len(counter$count - 1L)]
  if (input$preproc_df == "training set") {
    data$data = preproc.data$data
  } else {
    data$data.test = preproc.data$data
  }
  counter$count = counter$count - 1L
})


#### Preproc out

output$preproc_out = renderUI({
  switch(input$preproc_method,
    "Drop variable(s)" = preproc_dropfeature(),
    "Convert variable" = preproc_convar(),
    "Normalize variables" = preproc_normfeat(),
    "Remove constant variables" = preproc_remconst(),
    "Cap large values" = preproc_caplarge(),
    "Subset" = preproc_subset(),
    "Create dummy features" = preproc_createdummy(),
    "Impute" = preproc_impute(),
    "Feature selection" = preproc_feature_selection(),
    "Merge small factor levels" = preproc_merge_factor_levels())
})


output$preproc_data = DT::renderDataTable({
  validatePreprocData(preproc.data$data, input$preproc_df)
  d = preproc.data$data
  colnames(d) = make.names(colnames(d))
  d
}, options = list(lengthMenu = c(5, 20, 50), pageLength = 5, scrollX = TRUE)
)


output$preproc_testout = renderPrint({
  # counter$count
  # preproc.data$data.collection[counter$count]
  # str(preproc.data$data.collection)
  paste(counter$count, str(preproc.data$data.collection), sep =" \n")
})

