##### data summary #####

output$summary.datatable = renderDataTable({
  d = data(); if (is.null(d)) return(NULL)
  colnames(d) = make.names(colnames(d)) 
  summarizeColumns(d)
}, options = list(lengthMenu = c(5, 20, 50), pageLength = 5)
)


output$summary.vis.hist.var = renderUI({
  d = data()
  nums <- sapply(d, is.numeric)
  choices = colnames(d[, nums])
  # choices = as.list(colnames(data()))
  selectInput("summary.vis.hist.var", "Choose a variable:", choices = choices, selected = getLast(choices), width = "95%")
})

output$summary.vis.hist.nbins = renderUI({
  sliderInput("summary.vis.hist.nbins", "Number of bins", min = 1L, max = 100L, value = 30L, step = 1L, width = "95%")
})

output$summary.vis.hist = renderPlot({
  d = data() 
  if (is.null(d)) return(NULL)
  ggplot(data = d, aes(x = as.numeric(d[,input$summary.vis.hist.var]))) + 
    geom_histogram(aes(y = ..density..), stat = "bin", bins = input$summary.vis.hist.nbins) + 
    geom_density() + xlab(input$summary.vis.hist.var)
})    

output$summary.vis.bp.var = renderUI({
  d = data()
  nums <- sapply(d, is.numeric)
  choices = colnames(d[, nums])
  # choices = as.list(colnames(data()))
  selectInput("summary.vis.bp.var", "Choose a variable:", choices = choices, selected = getLast(choices), width = "95%")
})

output$summary.vis.bp = renderPlot({
  d = data() 
  if (is.null(d)) return(NULL)
  ggplot(data = d, aes(y = as.numeric(d[,input$summary.vis.bp.var]), x = input$summary.vis.bp.var)) + 
    geom_boxplot() + ylab(input$summary.vis.bp.var) + xlab("")
}) 