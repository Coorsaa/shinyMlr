##### data summary #####

# numeric variables
numericFeatures = reactive({
  # req(data$data)
  d = data$data
  return(colnames(Filter(is.numeric, d)))
})

# factor variables
factorFeatures = reactive({
  # req(data$data)
  d = data$data
  return(colnames(Filter(is.factor, d)))
})

output$summary.datatable = DT::renderDataTable({
  validateData(data$data)
  d = data$data
  colnames(d) = make.names(colnames(d))
  summarizeColumns(d)
}, options = list(scrollX = TRUE),
  caption = "Click on variable for visualisation!", selection = "single")

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

summary.vis.out = reactive({
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

output$summary.vis = renderPlot({
  summary.vis.out()
})

summary.vis.collection = reactiveValues(var.names = NULL, var.plots = NULL)

observeEvent(summary.vis.out(), {
  q = summary.vis.out()
  feat = isolate(summary.vis.var())
  # summary.vis.collection$var.names = c(summary.vis.collection$var.names,feat)
  summary.vis.collection$var.plots[[feat]] = q
})
