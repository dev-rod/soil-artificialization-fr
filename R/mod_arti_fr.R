#' arti_fr UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import leaflet
#' @importFrom shiny NS tagList
mod_arti_fr_ui <- function(id) {
  ns <- NS(id)
  if (grepl("arti_fr_1", id)) {
    tagList(
      # https://shiny.posit.co/r/getstarted/shiny-basics/lesson4/
      #sliderInput(ns("n"),"n", 1, 150, 75),
      plotOutput(ns("plot")),
      DT::DTOutput(ns("datatable"))
    )
  } else if (grepl("arti_fr_2", id)) {
    tagList(
      leafletOutput(ns("map")),
      textOutput(ns("text")),
      fluidRow(
        box(width = 2, actionButton("count", "Count")),
        infoBoxOutput(ns("ibox")),
        valueBoxOutput(ns("vbox")),
      ),
      uiOutput(ns("print")),
      uiOutput(ns("menu"))
    )
  }
}

#' arti_fr Server Functions
#'
#' @import lubridate ggplot2 DT dplyr
#' @importFrom forcats fct_inorder
#'
#' @noRd
mod_arti_fr_server <- function(id){
  #library(dplyr)

  # Chargement des indicateurs de consommation d'espace en 2019
  observatoire <-
    read.csv(
      'data/mesures_artificialisation_commune/pnb_action7_indicateurs_ff_consommation_espace_2019.csv',
      sep = ";",
      fileEncoding = "UTF-8"
    ) %>%
    mutate(
      idreg = as.factor(idreg),
      epci = as.factor(epci),
      au10 = as.factor(au10),
      typau = as.factor(typau),
      typpopau10 = as.factor(typpopau10),
      uu = as.factor(uu)
    ) %>%
    ungroup()

  # Total de la surface artificialisée en France entre 2009 et 2017.
  total_arti_fr_m2 <- summarise(observatoire, nafart0917 = sum(nafart0917, na.rm = T))[1,1]
  total_arti_fr_km2 <- prettyNum(total_arti_fr_m2 / 1e6, big.mark = " ", decimal.mark = ",")
  total_arti_fr_km2_f <- sprintf("%s km²", total_arti_fr_km2)
  #cat("Surface :", total_arti_fr_km2_f, "km2")

  # Pour chaque région :
  #   - le total de la surface artificialisée
  #   - la part de la surface totale qui a été artificialisée entre 2009 et 2017
  #   - la part de la surface artificialisée lié à l’activité et à l’habitat
  regions_fr <- st_read(dsn = 'data/contours_administratifs_france/ign_admin_express/REGION_CARTO.shp')

  artif_par_reg <- observatoire %>%
    group_by(idregtxt, idreg) %>%
    summarise(
      # surface_totale_artificialisee_2009_2017
      nafart0917 = sum(nafart0917, na.rm = T),
      # surface_totale_commune
      surfcom17 = sum(surfcom17, na.rm = T),
      # surface_totale_artificialisee_2009_2017_activite
      artact0917 = sum(artact0917, na.rm = T),
      # surface_totale_artificialisee_2009_2017_habitat
      arthab0917 = sum(arthab0917, na.rm = T)
    ) %>%
    mutate(
      # part_surface_totale_artificialisee_2009_2017 = ((surface_totale_artificialisee_2009_2017 / surface_totale_commune)*100),
      part_nafart0917 = nafart0917 * 100 / surfcom17,
      # part_surface_totale_artificalisee_2009_2017_act = (((surface_totale_artificialisee_2009_2017_activite)/surface_totale_artificialisee_2009_2017)*100)
      part_artact0917 = artact0917 * 100 / nafart0917,
      # part_surface_totale_artificalisee_2009_2017_hab_act = (((surface_totale_artificialisee_2009_2017_activite+surface_totale_artificialisee_2009_2017_habitat)/surface_totale_artificialisee_2009_2017)*100),
      part_arthab0917 = arthab0917 * 100 / nafart0917
    ) %>%
    ungroup() %>%
    left_join(select(regions_fr, geometry, INSEE_REG), by = c("idreg" = "INSEE_REG"))



  # Quelle région a le plus perdu de m² ?
  filter(artif_par_reg, nafart0917 == max(nafart0917)) %>% pull(idregtxt)

  # Quelle région a perdu la plus grande part de son territoire ?
  filter(artif_par_reg, part_nafart0917 == max(part_nafart0917)) %>% pull(idregtxt)

  # Quelle région artificialise le plus pour l’activité ?
  filter(artif_par_reg, part_artact0917 == max(part_artact0917)) %>% pull(idregtxt)

  # Porter le libellé de la région en une variable factorielle triée par la part de sa surface perdue.
  artif_par_reg <- artif_par_reg %>%
    arrange(-nafart0917) %>%
    mutate(idregtxt = fct_inorder(idregtxt))
  # levels(artif_par_reg$idregtxt)
  #
  # ggplot(data= artif_par_reg)+
  #   geom_col(aes(x=idregtxt,y=nafart0917))

  # Faire la même chose au niveau départemental (à moindre coût).
  # Porter le libellé du département en une variable factorielle triée par région.

  artif_par_dep <- observatoire %>%
    group_by(idregtxt, iddeptxt) %>%
    summarise(
      nafart0917 = sum(nafart0917, na.rm = T),
      surfcom17 = sum(surfcom17, na.rm = T),
      artact0917 = sum(artact0917, na.rm = T),
      arthab0917 = sum(arthab0917, na.rm = T)
    ) %>%
    mutate(
      part_nafart0917 = nafart0917 * 100 / surfcom17,
      part_artact0917 = artact0917 * 100 / nafart0917,
      part_arthab0917 = arthab0917 * 100 / nafart0917
    )

  filter(artif_par_dep, nafart0917 == max(nafart0917)) %>% pull(iddeptxt)
  filter(artif_par_dep, part_nafart0917 == max(part_nafart0917)) %>% pull(iddeptxt)
  filter(artif_par_dep, part_artact0917 == max(part_artact0917)) %>% pull(iddeptxt)


  artif_par_dep <- artif_par_dep %>%
    arrange(idregtxt) %>%
    mutate(iddeptxt = fct_inorder(iddeptxt))

  # Constituer un fichier qui permet de réaliser des séries temporelles au niveau des régions :
  #   faire un fichier avec une ligne pour chaque année n (au format date) et pour chaque région, et une colonne pour :
  #   - le libellé de la région
  #   - le nombre de m² perdu entre l’année n-1 et l’année n
  #   - le nombre de m² perdu cumulé entre 2009 et l’année n.
  artif_par_reg_0910 <- observatoire %>%
    group_by(idregtxt) %>%
    summarise(naf09art10 = sum(naf09art10, na.rm = T)) %>%
    mutate(annee = ymd('20100101')) %>%
    rename(nafart = naf09art10)

  artif_par_reg_1011 <- observatoire %>%
    group_by(idregtxt) %>%
    summarise(naf10art11 = sum(naf10art11, na.rm = T)) %>%
    mutate(annee = ymd('20110101')) %>%
    rename(nafart = naf10art11)

  artif_par_reg_1112 <- observatoire %>%
    group_by(idregtxt) %>%
    summarise(naf11art12 = sum(naf11art12, na.rm = T)) %>%
    mutate(annee = ymd('20120101')) %>%
    rename(nafart = naf11art12)

  artif_par_reg_1213 <- observatoire %>%
    group_by(idregtxt) %>%
    summarise(naf12art13 = sum(naf12art13, na.rm = T)) %>%
    mutate(annee = ymd('20130101')) %>%
    rename(nafart = naf12art13)

  artif_par_reg_1314 <- observatoire %>%
    group_by(idregtxt) %>%
    summarise(naf13art14 = sum(naf13art14, na.rm = T)) %>%
    mutate(annee = ymd('20140101')) %>%
    rename(nafart = naf13art14)

  artif_par_reg_1415 <- observatoire %>%
    group_by(idregtxt) %>%
    summarise(naf14art15 = sum(naf14art15, na.rm = T)) %>%
    mutate(annee = ymd('20150101')) %>%
    rename(nafart = naf14art15)

  artif_par_reg_1516 <- observatoire %>%
    group_by(idregtxt) %>%
    summarise(naf15art16 = sum(naf15art16, na.rm = T)) %>%
    mutate(annee = ymd('20160101')) %>%
    rename(nafart = naf15art16)

  artif_par_reg_1617 <- observatoire %>%
    group_by(idregtxt) %>%
    summarise(naf16art17 = sum(naf16art17, na.rm = T)) %>%
    mutate(annee = ymd('20170101')) %>%
    rename(nafart = naf16art17)

  artif_par_reg_0917 <- rbind(
    artif_par_reg_0910,
    artif_par_reg_1011,
    artif_par_reg_1112,
    artif_par_reg_1213,
    artif_par_reg_1314,
    artif_par_reg_1415,
    artif_par_reg_1516,
    artif_par_reg_1617
  ) %>%
    arrange(idregtxt, annee) %>%
    group_by(idregtxt) %>%
    mutate(nafart_cum = cumsum(as.numeric(nafart)))

  # Faire un graphique ligne avec en abscisse l’année, en ordonnée le nombre de m² perdu cumulé entre 2009 et l’année n,
  # et une ligne pour chaque région.
  # Dans quelle région cela semble s’améliorer ? Dans quelle région cela semble s’aggraver ?

  artif_par_dep <- observatoire %>%
    group_by(idregtxt, iddeptxt) %>%
    summarise_at(vars(nafart0917, surfcom17, artact0917, arthab0917),
                 list(sum = ~ sum(., na.rm = T),
                      min = ~ min(., na.rm = T)))

  artif_par_dep <- observatoire %>%
    group_by(idregtxt, iddeptxt) %>%
    summarise_if(is.numeric,
                 list(sum = ~ sum(., na.rm = T),
                      min = ~ min(., na.rm = T)))

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      if (grepl("arti_fr_1", id)) {

        output$datatable <- renderDT({
          datatable(read.csv(
            'data/mesures_artificialisation_commune/pnb_action7_indicateurs_ff_consommation_espace_2019.csv',
            sep = ";",
            fileEncoding = "UTF-8"
          ),
          extensions = 'Scroller',
          options = list(
            deferRender = F,
            dom = 't',
            columnDefs = list(list(className = 'dt-center', targets = 10)),
            scrollY = 400,
            scroller = TRUE,
            scrollX = T,
            pageLength = 50),
          filter = 'top'
        )
        })

        output$plot <- renderPlot({
          #plot(head(iris, input$n))
          ggplot(data = artif_par_reg_0917) +
            geom_line(aes(x = annee, y = nafart_cum, color = idregtxt))
        })

      } else if (grepl("arti_fr_2", id)) {

        # output$map <- renderLeaflet({
        #   leaflet() %>%
        #     addTiles() %>%
        #     setView(lng = 2.35, lat = 48.85, zoom = 10)  # Coordonnées et zoom initiaux
        #   # Ajoutez ici d'autres couches ou modifications à la carte selon vos besoins
        # })

        output$map <- renderTmap(plot_heatmap(artif_par_reg))

        output$text <- renderText({
          'test de texte'
        })

        output$ibox <- renderInfoBox({
          infoBox(
            "Title",
            input$count,
            icon = icon("credit-card")
          )
        })
        output$vbox <- renderValueBox({
          valueBox(
            "Surface totale artificialisée",
            total_arti_fr_km2_f,
            icon = icon("map")
          )
        })

        output$print <- renderPrint({
          'test de print'
        })

        output$menu <- renderMenu({
          'test de menu'
        })

        # output$dropdownmenu <- renderDropdownMenu({
        #   'test de DropdownMenu'
        # })

      }
    })

  })
}

## To be copied in the UI
# mod_arti_fr_ui("arti_fr_1")

## To be copied in the server
# mod_arti_fr_server("arti_fr_1")
