#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_arti_fr_server("arti_fr_1")
  mod_arti_fr_server("arti_fr_2")
}
