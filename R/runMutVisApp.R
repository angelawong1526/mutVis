#' Launch the shiny app for mutVis Package
#'
#' A function that launches the shiny app for muVis package.
#' The code can be found in \code{./inst/shiny-scripts}.
#'
#' @return Does not return anything. It opens a shiny page
#'
#' @examples
#' \dontrun{
#' runMutVisApp()
#' }
#'
#' @export
#' @importFrom shiny runApp
#'
#'
runMutVisApp <- function() {
  appDir <- system.file("shiny-scripts",
                        package = "mutVis")
  shiny::runApp(appDir, display.mode = "normal")
  return()
}

# [END]
