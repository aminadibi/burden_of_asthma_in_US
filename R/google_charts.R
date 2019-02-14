#' @import htmlwidgets
#' @export
google_charts = function(){
  htmlwidgets::createWidget("sigma", x, width = width, height = height)
}

#' @export
sigmaOutput <- function(outputId, width = "100%", height = "400px") {
  htmlwidgets::shinyWidgetOutput(outputId, "sigma", width, height, package = "sigma")
}
#' @export
renderSigma <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, sigmaOutput, env, quoted = TRUE)
}