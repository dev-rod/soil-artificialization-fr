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
          menuItem("Indicateurs", tabName = "kpi", icon = icon("dashboard")),
          menuItem("Carte", tabName = "map", icon = icon("map")),
          menuItem("Données", tabName = "data", icon = icon("table")),
          menuItem("Experiment", tabName = "xp", icon = icon("flask"),
                   badgeLabel = "new", badgeColor = "green")
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName = "kpi",
                  mod_arti_fr_ui("arti_fr_1")
          ),
          tabItem(tabName = "map",
                  mod_arti_fr_ui("arti_fr_2")
          )
          # tabItem(tabName = "data",
          #         mod_arti_fr_ui("arti_fr_1")
          # )#,
          #tabItem(tabName = "xp",
          #        mod_arti_44_ui("name_of_module2_ui_1")
          #)
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
