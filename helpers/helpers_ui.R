makeSidebar = function(..., bar.height = 500) {
  args = list(...)
  div(class = "sidebarbox",
    box(width = NULL, height = bar.height, args)
  )
}

makeReportConfigUI = function(title, id, passage = "", ...) {
  more.inps = list(...)
  if (length(more.inps) == 0L)
    more.inps = NULL
  section.title.inp = textInput(paste("sec.title", id, sep = "."),
    label = "Section title", value = title)
  include.inp = checkboxInput(paste("include", id, sep = "."),
    "Add to report", TRUE)
  hidden.inp = checkboxInput(paste("hidden", id, sep = "."),
    "Hide source code", FALSE)
  info.text = textInput(paste("passage", id, sep = "."), "Additional text:",
    value = passage)
  box(title = title, solidHeader = TRUE, width = 12, status = "warning", collapsible = TRUE, collapsed = TRUE,
    section.title.inp,
    include.inp,
    hidden.inp,
    info.text,
    more.inps
  )
}
