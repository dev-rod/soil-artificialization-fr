# Sujet
# convention citoyenne demandant un moratoire sur la construction des zones commerciales,
# souhaite savoir combien les activités de commerce artificialisent les sols sur commune de St Herblain

# Observatoire visant à permettre d'évaluer l'objectif pris par la France d'arriver
# à ne plus artificialiser les sols d'ici au plus tard 2050, dans le cadre de la loi ZAN
# Mesure de l'artificialisation des sols par les activités commerciales sur le territoire

# Données

# fichier sirene des établissements
# fichiers fonciers avec la surface totale et la surface artificialisée de chaque parcelle
#  https://doc-datafoncier.cerema.fr/doc/ff/ (doc-datafoncier.cerema.fr/doc_ffta/table/TUP/ obsolète invalide)

# Méthodologie

# source 1 on dispose de la liste des surfaces commerciales du département
# 44 avec leur code sirène, APE, libellé APE et leur polygone géométrique
# source 2 on dispose de la liste des zones artificialisées avec leur
# surface articialisée en m² et la liste de leurs polygones géométriques
# dcntarti Surface de suf de type artificialisé (en m²) (dcnt07 + dcnt09 +
# dcnt10 + dcnt11 + dcnt12 + dcnt13) geomtup Géométrie de l'entité
# (parcelle, uf ou pdlmp) à clarifier car c'est une liste de liste idcom
# code commune saint herblain 44162

## Livrable

# Carte en aplats de couleur de la commune de Saint Herblain pour
# représenter - Nombre de surfaces commerciales occupant chaque zone -
# Part de surface artificialisée occupant chaque zone

# Module UI

#' @title   mod_arti_44_ui and mod_arti_44_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_arti_44
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList includeMarkdown
mod_arti_44_ui <- function(id){
  ns <- NS(id)
  tagList(
    #includeMarkdown(
    #  system.file("app/www/home.md", package = "tidytuesday201942")
    #)
  )
}


# Module Server

#' @rdname mod_arti_44
#' @export
#' @import dplyr
#if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")};
####if("sf" %in% rownames(installed.packages()) == FALSE) {install.packages("sf")}; installed with tmap
#if("tmap" %in% rownames(installed.packages()) == FALSE) {install.packages("tmap")};
#library(dplyr)
#library(sf)
#library(tmap)
#if("svglite" %in% rownames(installed.packages()) == FALSE) {install.packages("svglite")};
#if("jquerylib" %in% rownames(installed.packages()) == FALSE) {install.packages("jquerylib")};
#if("munsell" %in% rownames(installed.packages()) == FALSE) {install.packages("munsell")};
#if("mapview" %in% rownames(installed.packages()) == FALSE) {install.packages("mapview")};
#library(mapview)

#library(tidyverse)
# setwd("path/to/folder/project")
#' @keywords internal

mod_arti_44_server <- function(input, output, session){
  ns <- session$ns

  load("data/sirene44_2019_03/sirene_zones_commerciales_44.RData")
  load("data/foncier_cerema/ff44162.RData")

  companies44 <- sirene44 %>%
    select(SIREN, APET700, LIBAPET, NOMEN_LONG, geometry)
  # plot(companies44)

  artificialZones44162 <- d44_tup_2017_44162_sf_select %>%
    select(idtup, dcntarti, geomtup)
  # plot(artificialZones44162)

  # syntax to remove "old-style crs object detected; please recreate object with a recent sf::st_crs()"
  st_crs(companies44) <- sf::st_crs(companies44)
  # PROJCRS["RGF93 / Lambert-93"
  # syntax to remove "old-style crs object detected; please recreate object with a recent sf::st_crs()"
  st_crs(artificialZones44162) <- sf::st_crs(artificialZones44162)
  # PROJCRS["RGF93 / Lambert-93"

  # Jointure spatiale des données de ventes foncières avec les surfaces artificialisées de Saint Herblain
  # et ajout du champ année des volume de ventes
  companiesInArtificialZones44162 <- st_join(artificialZones44162, companies44) %>%
    filter(!is.na(SIREN))

  mapview(companiesInArtificialZones44162)

  companyDensityInArtificialZones44162 <- companiesInArtificialZones44162 %>%
    group_by(idtup, dcntarti) %>%
    tally()

  companyDensityInArtificialZones44162 %>%
    tm_shape()+
    tm_fill("n", title="Nombre de commerces") +
    tm_polygons("n", textNA="Valeur manquante", style = "jenks")+
    # tm_polygons() +
    # tm_facets("APET700") +
    tm_borders("white", lwd = .5)

  # il eut été interessant de filtrer par activité via le code APE, mais sur quels critères
  company4649ZDensityInArtificialZones44162 <- companiesInArtificialZones44162 %>%
    filter(APET700 == "4649Z") %>%
    group_by(idtup, dcntarti, APET700, LIBAPET) %>%
    tally()

  company4649ZDensityInArtificialZones44162 %>%
    tm_shape()+
    tm_fill("n", title="Nombre de commerces") +
    tm_polygons("n", textNA="Valeur manquante", style = "jenks")+
    # tm_polygons() +
    # tm_facets("APET700") +
    tm_borders("white", lwd = .5)

  output$datatable <- renderDT({
    datatable(artificialZones44162)
    #if (is.character(data)) {
    #datatable(read.csv(data))
    #} else {
    #  datatable(data)
    #}
  })
}
