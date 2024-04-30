#' arti_fr UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import leaflet echarts4r DT
#' @importFrom shiny NS tagList
mod_arti_fr_ui <- function(id) {
  ns <- shiny::NS(id)

  # LOCAL
  if (grepl("arti_fr_1", id)) {
    tagList(
      # https://shiny.posit.co/r/getstarted/shiny-basics/lesson4/
      #shiny::sliderInput(ns("n"),"n", 1, 150, 75),
      #shiny::plotOutput(ns("histo_conso_local")),
      echarts4r::echarts4rOutput(ns("histo_conso_local"), height = "40vh"),
      leaflet::leafletOutput(ns("carto_conso_local"))
    )
  # GLOBAL
  } else if (grepl("arti_fr_2", id)) {
    tagList(
      #shiny::plotOutput(ns("histo_conso_global")),
      echarts4r::echarts4rOutput(ns("histo_conso_global"), height = "40vh"),
      shiny::fluidRow(
        #leaflet::leafletOutput(ns("carto_conso_global_reg"))#,
        leaflet::leafletOutput(ns("carto_conso_global_dept"))
      ),
      shiny::fluidRow(
        shinydashboard::valueBoxOutput(ns("vbox"))
      )
    )
  # DATATABLE
  } else if (grepl("arti_fr_3", id)) {
    tagList(
      DT::DTOutput(ns("datatable"))
    )
  # XP
  } else if (grepl("arti_fr_4", id)) {
    tagList(
      shiny::plotOutput(ns("histo_conso_global")),
      # leaflet::leafletOutput(ns("carto_conso_global")),
      shiny::textOutput(ns("text")),
      shiny::fluidRow(
        box(width = 2, actionButton("count", "Count")),
        shinydashboard::infoBoxOutput(ns("ibox"))
      ),
      shiny::uiOutput(ns("print")),
      shiny::uiOutput(ns("menu"))
    )
  # IFRAME URBAN AREA
  } else if (grepl("arti_fr_5", id)){
    tagList(
      shiny::htmlOutput("urbanAreaPng")
    )
  }
}

#' arti_fr Server Functions
#'
#' @import DT echarts4r tmap shiny shinydashboard
#'
#' @noRd
mod_arti_fr_server <- function(id){

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    observe({
      # Menu Local
      if (grepl("arti_fr_1", id)) {

        # Histogramme évolution annuelle consommation espace sur Orée d'Anjou
        # avec répartition par type de consommation dans chacunes des colonnes
        output$histo_conso_local <- echarts4r::renderEcharts4r({
          histo_conso_espace_oree_anjou()
        })

        # Carte de chaleur avec carroyage sur 1km du territoire local observé (Orée d'Anjou)
        # de consommation en m² avec 2 décimales par type d'activité sélectionnée
        # dans un sélecteur
        # Pas d'informations concernant le carroyage sur 200m
        output$carto_conso_local <- tmap::renderTmap({
          carto_conso_espace_oree_anjou()
        })

        # histogramme évolution annuelle consommation espace sur La Varenne
        # avec répartition par type de consommation dans chacunes des colonnes
        # output$histo_conso_varenne <- echarts4r::renderEcharts4r({
        #   histo_conso_espace_varenne()
        # })

      # Menu Global
      } else if (grepl("arti_fr_2", id)) {

        # En France, comparaison entre régions ou départements entre 2009 et 2024
        # de Consommation d'espaces total ou selon sélection du type d'activité dans un sélecteur
        # en hectares avec 2 décimales
        # soit n courbes pour chacunes des régions ou départements sélectionnés par l'activité sélectionnée
        # output$histo_conso_global <- shiny::renderPlot({
        output$histo_conso_global <- echarts4r::renderEcharts4r({
          histo_conso_espace_france()
        })

        # Carte de chaleur du territoire observé (régions en france)
        # de consommation en hectares avec 2 décimales par type d'activité sélectionnée
        # dans un sélecteur
        output$carto_conso_global_reg <- tmap::renderTmap({
          carto_conso_espace_reg_fr()
        })

        # Carte de chaleur du territoire observé (départements en France)
        # de consommation en hectares avec 2 décimales par type d'activité sélectionnée
        # dans un sélecteur
        output$carto_conso_global_dept <- tmap::renderTmap({
          carto_conso_espace_dept_fr()
        })

        output$vbox <- shinydashboard::renderValueBox({
          valueBox(
            "Surface totale artificialisée",
            totalSurfaceArti(),
            icon = icon("map")
          )
        })

      # Datatable
      } else if (grepl("arti_fr_3", id)) {

        output$datatable <- DT::renderDT({
          data_conso_espace_france()
        })

      # Draft
      } else if (grepl("arti_fr_4", id)) {

        # output$map <- leaflet::renderLeaflet({
        #   leaflet() |>
        #     leaflet::addTiles() |>
        #     leaflet::setView(lng = 2.35, lat = 48.85, zoom = 10)  # Coordonnées et zoom initiaux
        # })

        # output$plot_conso <- shiny::renderPlot({
        #   plot(head(iris, input$n))
        # })

        output$text <- shiny::renderText({
          'test de texte'
        })

        output$ibox <- shinydashboard::renderInfoBox({
          infoBox(
            "Title",
            input$count,
            icon = icon("credit-card")
          )
        })

        output$print <- shiny::renderPrint({
          'test de print'
        })

        output$menu <- shinydashboard::renderMenu({
          'test de menu'
        })

        ## output$dropdownmenu <- shinydashboard::renderDropdownMenu({
        ##   'test de DropdownMenu'
        ## })

      } else if (grepl("arti_fr_5", id)){

        #src = "https://www.insee.fr/fr/statistiques/fichier/4803954/poster_zaav.png"
        src = "/data/contours_administratifs_france/src/poster_zaav.png"
        output$urbanAreaPng <- shiny::renderText({
          c('<img src="', src, '">')
        })
      }
    })

  })
}

## To be copied in the UI
# mod_arti_fr_ui("arti_fr_1")

## To be copied in the server
# mod_arti_fr_server("arti_fr_1")
