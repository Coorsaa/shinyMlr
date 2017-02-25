report.config = reactive({
  section.names = c("data", "task", "learners", "tuning",
    "modelling", "benchmark")

  sec.titles = pasteDot("sec.title", section.names)
  sec.titles = lapply(sec.titles, function(x) {
    x = input[[x]]
    if (is.null(x))
      inc = FALSE
    return(x)
  })
  names(sec.titles) = section.names

  include = pasteDot("include", section.names)
  include = lapply(include, function(inc) {
    inc = input[[inc]]
    if (is.null(inc))
      inc = FALSE
    return(inc)
  })
  names(include) = section.names

  hidden = pasteDot("hidden", section.names)
  hidden = lapply(hidden, function(hd) {
    hd = input[[hd]]
    if (is.null(hd))
      hd = FALSE
    return(hd)
  })
  names(hidden) = section.names

  passage = pasteDot("passage", section.names)
  passage = lapply(passage, function(pas) {
    pas = input[[pas]]
    if (is.null(pas))
      hd = ""
    return(pas)
  })
  names(passage) = section.names
  
  list(titles = sec.titles, include = include, hidden = hidden,
    passage = passage)
})

# output$testout = renderPrint({
#   reqAndAssign(summary.vis.var(), "feat")
#   str(summary.vis.collection[[feat]])
# })


output$report = downloadHandler(
  filename = function() {
    paste('report', sep = '.', switch(
      input$report.format, PDF = 'pdf', HTML = 'html'))
  },
  content = function(file) {
    src = normalizePath('report.Rmd')
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd = setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'report.Rmd', overwrite = TRUE)

    params = report.config()
    out = rmarkdown::render('report.Rmd',
      switch(
        input$report.format,
        PDF = pdf_document(), HTML = html_document()
      ),
      params = params
    )
    file.rename(out, file)
  }
)

