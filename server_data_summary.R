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
      geom_vline(aes(xintercept = quantile(as.numeric(d[,input$summary.vis.var]), 0.05)), color = "blue", size = 0.5, linetype = "dashed") +
      geom_vline(aes(xintercept = quantile(as.numeric(d[,input$summary.vis.var]), 0.95)), color = "blue", size = 0.5, linetype = "dashed") +
      geom_vline(aes(xintercept = quantile(as.numeric(d[,input$summary.vis.var]), 0.5)), color = "blue", size = 1, linetype = "dashed")
  } else {
    ggplot(data = d, aes(x = d[,input$summary.vis.var])) + 
      geom_bar(aes(fill = d[,input$summary.vis.var]), stat = "count") + xlab(input$summary.vis.var) +
      guides(fill=FALSE)
  }
})  



##### preprocessing #####

output$preproc.var = renderUI({
  selectInput("preproc.method", "Choose data preprocessing method:", choices = c("Impute", "capLargeValues",  "createDummyFeatures", "dropFeatures", "mergeSmallFactorLevels", "normalizeFeatures", "removeConstantFeatures"))
})

output$preproc = renderUI({
  req(data())
  req(input$preproc.method)
  d = data()
  preproc.select(d, input$preproc.method)
})

preproc.select = function (d, method) {
  if (method == "Impute") {
    fluidRow(
      column(6,
        selectInput("impute.methods.num", "Choose imputation method for numeric variables", selected = "imputeMean",
          choices = c("imputeConstant", "imputeMean", "imputeMedian", "imputeMode", "imputeMin", "imputeMax", "imputeNormal", "imputeHist"))),
      column(6,  
        selectInput("impute.methods.fac", "Choose imputation method for factor variables", selected = "imputeMode",
          choices = c("imputeConstant", "imputeMode", "imputeMin", "imputeMax"))),
      column(1),
      column(5,
        numericInput("impute.constant.num.input", "Constant value:", min = -Inf,  max = Inf, value = 0)
      ),
      column(5,
        numericInput("impute.constant.fac.input", "Constant value:", min = -Inf,  max = Inf, value = 0)
      ),
      column(1),
    actionButton("impute.start", "Start imputation"))
  } 
}

impute.methods.num = reactive(
  input$impute.methods.num
)

impute_data = eventReactive(input$impute.start, {
  d = data()
 if (input$impute.methods.num == "imputeConstant") {
   imputed = impute(d, classes = list(numeric = imputeConstant(input$impute.constant.input), factor = match.fun(input$impute.methods.fac)()))
 } else {
    imputed = impute(d, classes = list(numeric = match.fun(input$impute.methods.num)(), factor = match.fun(input$impute.methods.fac)()))
 }
  imputed$data
})

output$impute.datatable = renderDataTable({
  req(impute_data())
  d = impute_data()
  colnames(d) = make.names(colnames(d))
  d
}, options = list(lengthMenu = c(5, 20, 50), pageLength = 5)
)

preproc.method = reactive(input$preproc.method)
observeEvent(preproc.method(), {
  if (preproc.method() != "Impute") {
    shinyjs::hide("impute.datatable")
  } else {
    shinyjs::show("impute.datatable")
  }
})

