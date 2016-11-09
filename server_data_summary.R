##### data summary #####

output$summary.datatable = renderDataTable({
  req(data())
  d = data()
  colnames(d) = make.names(colnames(d)) 
  summarizeColumns(d)
}, options = list(lengthMenu = c(5, 20, 50), pageLength = 5)
)


output$summary.vis.hist.var = renderUI({
  d = data()
  # nums = sapply(d, is.numeric)
  # choices = colnames(d[, nums])
  choices = as.list(colnames(data()))
  selectInput("summary.vis.var", "Choose a variable:", choices = choices, selected = getLast(choices), width = "95%")
})

output$summary.vis.hist.nbins = renderUI({
  sliderInput("summary.vis.hist.nbins", "Number of bins", min = 1L, max = 100L, value = 30L, step = 1L, width = "95%")
})


output$summary.vis = renderPlot({
  req(data())
  d = data()
  factors = sapply(d, is.factor)
  numerics = sapply(d, is.numeric)
  factor_ch = colnames(d[,factors])
  num_ch = colnames(d[,numerics])
  ggplot(data = d, aes(x = as.numeric(d[,input$summary.vis.var]))) + 
    geom_histogram(aes(y = ..density..), stat = "bin", bins = input$summary.vis.hist.nbins) + 
    geom_density() + xlab(input$summary.vis.hist.var)
})  

output$summary.vis.hist = renderPlot({
  req(data())
  d = data()
  ggplot(data = d, aes(x = as.numeric(d[,input$summary.vis.var]))) + 
    geom_histogram(aes(y = ..density..), stat = "bin", bins = input$summary.vis.hist.nbins) + 
    geom_density() + xlab(input$summary.vis.hist.var)
})    

output$summary.vis.bp.var = renderUI({
  req(data())
  d = data()
  nums <- sapply(d, is.numeric)
  choices = colnames(d[, nums])
  # choices = as.list(colnames(data()))
  selectInput("summary.vis.bp.var", "Choose a variable:", choices = choices, selected = getLast(choices), width = "95%")
})

output$summary.vis.bp = renderPlot({
  req(data())
  d = data()
  ggplot(data = d, aes(y = as.numeric(d[,input$summary.vis.bp.var]), x = input$summary.vis.bp.var)) + 
    geom_boxplot() + ylab(input$summary.vis.bp.var) + xlab("")
}) 