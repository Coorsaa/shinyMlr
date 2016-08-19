library(mlr)

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)

shinyServer(function(input, output) {
  data = reactive(
    if(is.null(input$file1))
      return(NULL)
    else
      read.csv(input$file1$datapath, header=input$header, sep=input$sep,
                           quote=input$quote)
    )

  output$contents = renderTable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.

    inFile = data()
    if (is.null(inFile))
      return(NULL)
    else
      head(inFile)
  })
  
  output$lrns = renderUI({
    selectInput("lrn", "Choose a learner:", as.list(substr(listLearners("classif", properties = "multiclass")$class, 9, 100))) 
  })

  output$lrnText = renderText({input$lrn})
  output$lrnText2 = renderText({input$lrn})
  
  output$target = renderUI({
    selectInput("target", "Choose a target:", as.list(colnames(data())))
  })
  
  output$trainText = eventReactive(input$train, {
    "Model was trained!"
  })
  
  
  #  model = eventReactive(input$train, {
  #     lrn = makeLearner(paste0("classif.", input$lrn))
  #     task = makeClassifTask(data = data(), target = input$target)
  #      train(lrn, task)
  # })
  
  output$resampleText = eventReactive(input$resample, {
    "Resampling was executed!"
  })
  
  output$resamplePrint = eventReactive(input$resample, {
    lrn = makeLearner(paste0("classif.", input$lrn))
    task = makeClassifTask(data = data(), target = input$target)
    rdesc = makeResampleDesc("CV", iters = 2)
    as.numeric(resample(lrn, task, resampling = rdesc)$aggr)
  })
  
  })

