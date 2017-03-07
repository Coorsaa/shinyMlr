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
  pos.x = colnames(Filter(function(x) "POSIXt" %in% class(x) , d))
  d = dropNamed(d, drop = pos.x)    
  summarizeColumns(d)
}, options = list(scrollX = TRUE),
  caption = paste("Your dataset contains", nrow(d), "observations. Click on one or more variables for visualisation!"))

summary.vis.var = reactive({
  reqAndAssign(data$data, "d")
  pos.x = colnames(Filter(function(x) "POSIXt" %in% class(x) , d))
  d = dropNamed(d, drop = pos.x)
  s = summarizeColumns(d)
  s$name[input$summary.datatable_rows_selected]
})

output$summary.vis.hist = renderUI({
  list(
    column(9,
      sliderInput("summary.vis.hist.nbins", "Number of bins", min = 1L, max = 100L,
        value = 30L, step = 1L, width = "95%")
    ),
    column(3,
      radioButtons("summary.vis.dens", "Show density?", choices = c("Yes", "No"),
        selected = "Yes", inline = TRUE)
    )
  )
})

observeEvent(summary.vis.var(), {
  feature = summary.vis.var()
  if (length(feature) > 0L) {
    shinyjs::show("summary.vis.box", anim = TRUE)
    if (length(feature) == 1L) {
      if (feature %in% factorFeatures()) {
        shinyjs::hide("summary.vis.hist", animType = "fade")
      } else {
        shinyjs::show("summary.vis.hist", anim = TRUE)
      }
    } else
      shinyjs::hide("summary.vis.hist", animType = "fade")
  } else {
    shinyjs::hide("summary.vis.box", anim = TRUE)
  }
})

summary.vis.out = reactive({
  reqAndAssign(summary.vis.var(), "feature")
  reqAndAssign(input$summary.vis.dens, "density")
  d = na.omit(data$data)
  barfill = "#3c8dbc"
  barlines = "#1d5a92"
  if (length(feature) == 1L) {
    if (feature %in% numericFeatures()) {
      summary.plot = ggplot(data = d, aes(x = as.numeric(d[,feature]))) + 
        geom_histogram(aes(y = ..density..), colour = barlines, fill = barfill, stat = "bin", bins = input$summary.vis.hist.nbins) + xlab(feature) +
        geom_vline(aes(xintercept = quantile(as.numeric(d[,feature]), 0.05)), color = "blue", size = 0.5, linetype = "dashed") +
        geom_vline(aes(xintercept = quantile(as.numeric(d[,feature]), 0.95)), color = "blue", size = 0.5, linetype = "dashed") +
        geom_vline(aes(xintercept = quantile(as.numeric(d[,feature]), 0.5)), color = "blue", size = 1, linetype = "dashed")
      summary.plot = addPlotTheme(summary.plot)
      summary.plot
      if (density == "Yes")
        summary.plot = summary.plot + geom_density(fill = "blue", alpha = 0.1)
      summary.plot
    } else {
      summary.plot = ggplot(data = d, aes(x = d[,feature])) + 
        geom_bar(aes(fill = d[,feature]), stat = "count") + xlab(feature) +
        guides(fill = FALSE)
      summary.plot = addPlotTheme(summary.plot)
      summary.plot
    }
  } else if (length(feature) > 1L) {
    summary.plot = ggpairs(data = d, columns = input$summary.datatable_rows_selected,
        upper = list(continuous = wrap("cor", size = 10)), 
        lower = list(continuous = "smooth"))
    summary.plot
  }
})

output$summary.vis = renderPlotly({
  ggplotly(summary.vis.out())
})

summary.vis.collection = reactiveValues(var.plots = NULL)#var.names = NULL, var.plots = NULL)

observeEvent(summary.vis.out(), {
  q = summary.vis.out()
  feat = isolate(summary.vis.var())
  feat = paste(feat, collapse = ".x.")

  # summary.vis.collection$var.names = c(summary.vis.collection$var.names, feat)
  summary.vis.collection$var.plots[[feat]] = q

})
