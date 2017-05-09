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

output$data.summary.box = renderUI({
  if (input$preproc_df == "training set")
    title = "Data Summary of Training Set"
  else
    title = "Data Summary of Test Set"
  ui = box(width = 12, title = title,
    htmlOutput("summary.text"),
    br(),
    htmlOutput("data.summary.caption"),
    DT::dataTableOutput("summary.datatable")
  )
  ui
})

data.summary = reactive({
  if (input$preproc_df == "training set")
    d = data$data
  else
    d = data$data.test
  validateData(d)
  colnames(d) = make.names(colnames(d))
  pos.x = colnames(Filter(function(x) "POSIXt" %in% class(x) , d))
  d = dropNamed(d, drop = pos.x)    
  summarizeColumns(d)
})

output$data.summary.caption = renderUI({
  capt = sprintf("Your dataset contains %i observations. Click on one or more variables for visualisation!", nrow(data$data))
  helpText(capt)
})

output$summary.datatable = DT::renderDataTable({
  data.summary()
}, options = list(scrollX = TRUE))# , caption = capt)

# used in preproc 
output$summary.datatable2 = DT::renderDataTable({
  data.summary()
}, options = list(scrollX = TRUE))

summary.vis.var = reactive({
  reqAndAssign(data$data, "d")
  pos.x = colnames(Filter(function(x) "POSIXt" %in% class(x) , d))
  d = dropNamed(d, drop = pos.x)
  s = summarizeColumns(d)
  s$name[input$summary.datatable_rows_selected]
})

output$summary.vis.hist = renderUI({
  list(
    column(3,
      radioButtons("summary.vis.dens", "Plot type", choices = c("Histogram", "Density"),
        selected = "Histogram", inline = TRUE)
    ),
    column(9,
      sliderInput("summary.vis.hist.nbins", "Number of bins", min = 1L, max = 100L,
        value = 30L, step = 1L, width = "95%")
    )
  )
})

observeEvent(input$summary.vis.dens, {
  if (input$summary.vis.dens == "Density")
    shinyjs::hide("summary.vis.hist.nbins", animType = "fade")
  else
    shinyjs::show("summary.vis.hist.nbins", animType = "fade")
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
  d = na.omit(data$data)
  reqNFeat(feature, d)
  barfill = "#3c8dbc"
  barlines = "#1d5a92"
  if (length(feature) == 1L) {
    if (feature %in% numericFeatures()) {
      reqAndAssign(input$summary.vis.dens, "density")
      x = as.numeric(d[,feature])
      summary.plot = ggplot(data = d, aes(x = x))
      
      if (density == "Density")
        summary.plot = summary.plot + geom_density(fill = "blue", alpha = 0.1)
      else
        summary.plot = summary.plot + geom_histogram(colour = barlines, fill = barfill, stat = "bin", bins = input$summary.vis.hist.nbins)
      
      summary.plot = summary.plot + xlab(feature) +
        geom_vline(aes(xintercept = quantile(x, 0.05)), color = "blue", size = 0.5, linetype = "dashed") +
        geom_vline(aes(xintercept = quantile(x, 0.95)), color = "blue", size = 0.5, linetype = "dashed") +
        geom_vline(aes(xintercept = quantile(x, 0.5)), color = "blue", size = 1, linetype = "dashed")
      summary.plot = addPlotTheme(summary.plot)
      summary.plot
    } else {
      class = d[,feature]
      summary.plot = ggplot(data = d, aes(x = class)) + 
        geom_bar(aes(fill = class), stat = "count") + xlab(feature) +
        guides(fill = FALSE)
      summary.plot = addPlotTheme(summary.plot)
      summary.plot
    }
  } else if (length(feature) > 1L) {
    summary.plot = ggpairs(data = d, columns = feature,
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
