#' Run the colorexplorer shiny app
#' 
#' @param plotobj You can pass a \code{ggplot} object to be used in the shiny
#' app. It should have at least either a \code{color} or \code{fill} aesthetic
#' for this to make sense (otherwise, you won't be able to change any of the
#' colors interactively).
#' #' @return Called for its side effects.
#' @section Note:
#' The app requires the following additional packages:
#' \itemize{
#'   \item `shiny`
#'   \item `colourpicker`
#'   \item `colorblindr` (available on GitHub at \url{https://github.com/clauswilke/colorblindr})
#' }
#' Make sure they are installed or the app won't work.
#' @examples
#' \donttest{
#' library(ggplot2)
#' 
#' g <- ggplot(mpg, aes(class, fill=class)) + geom_bar()
#' run_colorexplorer(g)
#' }
#' @export
run_colorexplorer <- function(plotobj=NULL) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Please install `shiny` before running the app (install.packages(\"shiny\")).", call.=F)
  }
  if (!requireNamespace("colourpicker", quietly = TRUE)) {
    stop("Please install `colourpicker` before running the app (install.packages(\"colourpicker\").", call.=F)
  }
  if (!requireNamespace("colorblindr", quietly = TRUE)) {
    stop("Please install `colorblindr` before running the app (remotes::install_github(\"clauswilke/colorblindr\")).", call.=F)
  }

  if (!is.null(plotobj) && !inherits(plotobj, "ggplot")) {
    stop("plotobj must be a ggplot object")
  }

  appDir <- system.file("colorexplorer-app", package="ggcertara")
  if (appDir == "") {
    stop("Could not find shiny app directory. Try re-installing `ggcertara`.", call.=F)
  }
  
  if (!is.null(plotobj)) {
    .GlobalEnv$.colorexplorer_plotobj <- plotobj
    on.exit(rm(.colorexplorer_plotobj, envir=.GlobalEnv))
  }
  shiny::runApp(appDir)
}

if (getRversion() >= "2.15.1") utils::globalVariables(c(".colorexplorer_plotobj"))


# vim: ts=2 sw=2 et
