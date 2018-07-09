getTool <- function(inputId) {
  tagList(
    tags$head(tags$script(src = "js/navbar.js")),
    tags$html(includeHTML('navbar.html'))
  )
}