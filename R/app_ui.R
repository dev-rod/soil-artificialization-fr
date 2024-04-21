#golem::document_and_reload()

#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny shinythemes shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    #shinythemes::themeSelector(),
    # Your application UI logic
    # List the first level UI elements here
    # https://fontawesome.com/icons
    dashboardPage(
      dashboardHeader(title = "Suivi de l'artificialisation des sols en France", titleWidth=420),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Sur Orée d'Anjou", tabName = "local", icon = icon("map")),
          menuItem("En France", tabName = "country", icon = icon("globe")),
          #icon("dashboard"), icon("earth-europe")
          menuItem("Données", tabName = "data", icon = icon("table")),
          menuItem("Experiment", tabName = "xp", icon = icon("flask"), badgeLabel = "new", badgeColor = "green"),
          menuItem("Aires urbaines", tabName="urbanArea", icon = icon("city"))
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName = "local",
                  mod_arti_fr_ui("arti_fr_1")
          ),
          tabItem(tabName = "country",
                  mod_arti_fr_ui("arti_fr_2")
          ),
          tabItem(tabName = "data",
                  mod_arti_fr_ui("arti_fr_3")
          ),
          tabItem(tabName = "xp",
                  mod_arti_fr_ui("arti_fr_4")
          ),
          tabItem(tabName = "urbanArea",
                  mod_arti_fr_ui("arti_fr_5")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "soilArtificialization"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
