# Server and UI code for the Shiny app

#' app_ui
#' UI for CHI-power Dashboard
#'
#' @name app
#'
#' @return UI for CHI-power Dashboard
#' @export
#' @importFrom bslib page_sidebar sidebar
app_ui <- function()
{
  ui <- page_sidebar(
    title = 'CHI-power Dashboard',
    sidebar = sidebar(
      title = 'SomaLogic', somalogic_sidebarUI('somalogic')
    ),

    somalogic_cardsUI('somalogic')
  )
}


#' app_server
#' Server for CHI-power Dashboard
#'
#' @rdname app
#'
#' @return Server function for CHI-power Dashboard
#' @export
#' @importFrom shiny observeEvent reactiveValuesToList
app_server <- function()
{
  function(input, output, session) {
    somalogic_server("somalogic")
  }
}