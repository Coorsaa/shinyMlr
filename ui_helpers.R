makeSidebar = function(..., bar.height = 500) {
  args = list(...)
  box(background = "light-blue", width = NULL, height = bar.height,
    args
  )
}
