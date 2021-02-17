#' Run the shiny user interface for an nlmixr object
#'
#' @param fit A nlmixr object
#' @param ... Passed to create_ui and create_server
#' @family nlmixr shiny app
#' @export
#' @examples
#' \dontrun{
#' # load your nlmixr fit object as 'fit'
#' run_app(fit)
#' }
#' @importFrom shiny shinyApp
run_app <- function(fit, ...) {
  shiny::shinyApp(
    ui=create_ui(fit=fit, ...),
    server=create_server(fit=fit, ...)
  )
}
