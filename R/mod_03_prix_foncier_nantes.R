# Exploiter les données DVF sur les transactions immobilières dans l’ancien et la carte des quartiers de la commune sélectionnée pour obtenir un prix moyen des transactions par quartier.
#
# - Les données dvf se retrouvent dans le package variousdata : github.com/maeltheuliere/variousdata
# TODO importer directement les données au global via le csv
# - Contour des quartiers de Nantes :
# https://data.nantesmetropole.fr/explore/dataset/244400404_quartiers-communes-nantes-metropole/export/?disjunctive.libcom
#
# On veut produire les infos suivantes par quartier et année sur la commune observée
#
# Volume de ventes par quartier
# Pourcentage de maisons dans les ventes par quartier
# Prix moyen au m2 par type de bien par quartier

# Module UI

#' @title   mod_foncier_ui and mod_foncier_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_foncier
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList includeMarkdown
mod_foncier_ui <- function(id){
  ns <- NS(id)
  tagList(
    #includeMarkdown(
    #  system.file("app/www/home.md", package = "tidytuesday201942")
    #)
  )
}

# Module Server

#' @rdname mod_foncier
#' @export
#' @import dplyr
# Taper variousdata:: dans la console pour voir les noms des datas disponibles
#if("variousdata" %in% rownames(installed.packages()) == FALSE) {remotes::install_github("MaelTheuliere/variousdata")};library(variousdata)
#if("sf" %in% rownames(installed.packages()) == FALSE) {install.packages("sf")};library(sf)
#if("DT" %in% rownames(installed.packages()) == FALSE) {install.packages("DT")};library(DT)
# @import tidyverse
#if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")};library(tidyverse)
#if("tmap" %in% rownames(installed.packages()) == FALSE) {install.packages("tmap")};library(tmap)
#if("mapview" %in% rownames(installed.packages()) == FALSE) {install.packages("mapview")};library(mapview)
#if("leaflet" %in% rownames(installed.packages()) == FALSE) {install.packages("leaflet")};library(leaflet)
#if("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")};library(lubridate)
## @import forcats
## @import ggplot2
#library(dplyr)
#library(forcats)
#library(ggplot2)
#if("timechange" %in% rownames(installed.packages()) == FALSE) {install.packages('timechange', source='https://cran.rstudio.com/')};
#if("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages('lubridate', source='https://cran.rstudio.com/')};library(lubridate)
####library(tidyr)
## @importFrom dplyr pull
#' @keywords internal

  mod_foncier_server <- function(input, output, session){
    ns <- session$ns

    # Récupération des quartiers de Nantes
    gjsQuartierNantes <- "data/quartier_nantes/244400404_quartiers-nantes.geojson"
    quartierNantes <- st_read(gjsQuartierNantes)

    # Error in clean_columns(as.data.frame(obj), factorsAsCharacter) : list columns are only allowed with raw vector contents
    #mapview(quartierNantes)

    summary(quartierNantes)

    # Récupération des données de ventes foncières depuis le package variousdata
    # Nettoyage des valeurs nulles et conversion en spatial dataframe
    unique(dvf_r52$longitude)
    sdDvf <- dvf_r52 %>%
      filter(!is.na(longitude), !is.na(latitude), !is.na(valeur_fonciere), !is.na(surface_reelle_bati)) %>%
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

    # Jointure spatiale des données de ventes foncières avec les quartiers de Nantes
    # et ajout du champ année des volume de ventes
    dvfNantes <- st_join(sdDvf, quartierNantes)
    #summary(dvfNantes) %>%
    dvfNantes_with_year <- dvfNantes %>%
      filter(!is.na(nom)) %>%
      mutate(
        annee_mutation = year(date_mutation)
      )

  ############################ Ventes par quartier et année sur Nantes #####################################

    # Agrégat ventes par quartier et année
    ventesNantes <- dvfNantes_with_year %>%
      filter(
        nature_mutation == "Vente"
      ) %>%
      group_by(nom, annee_mutation) %>%
      #group_by(nom) %>%
      tally()

    # On réaffecte les données spatiales attendues pour les polygones
    ventesNantes <- st_drop_geometry(ventesNantes)
    ventesNantesWithoutGeom <- ventesNantes
    ventesNantes <- left_join(ventesNantesWithoutGeom, quartierNantes) %>%
      st_as_sf()

    # Carte des ventes par quartier et année sur Nantes
    ventesNantes %>%
      tm_shape()+
      tm_fill("n", title="Nombre de ventes") +
      tm_polygons("n", textNA="Valeur manquante", style = "jenks")+
      tm_polygons() +
      tm_facets("annee_mutation") +
      tm_borders("white", lwd = .5)

  ############################ Pourcentage ventes maison par quartier et année sur Nantes #####################################

  # Agrégat ventes maison par quartier et année sur Nantes
    ventesMaisonsNantes <- dvfNantes_with_year %>%
      filter(
        nature_mutation == "Vente",
        type_local %in% c("Maison")
      ) %>%
      group_by(nom, annee_mutation) %>%
      tally()

    ventesMaisonsNantes <- st_drop_geometry(ventesMaisonsNantes)
    ventesMaisonsNantes <- ventesMaisonsNantes %>%
      rename(nb_maisons = n) %>%
      left_join(ventesNantesWithoutGeom) %>%
      mutate(pourcentage_maison = nb_maisons*100/(n))
    ventesMaisonsNantes <- left_join(ventesMaisonsNantes, quartierNantes) %>%
      st_as_sf()

    # Carte des pourcentages de maisons dans les ventes par quartier et année sur Nantes
    ventesMaisonsNantes %>%
      tm_shape()+
      tm_fill("pourcentage_maison", title="% de maisons\ndans les ventes") +
      tm_polygons("pourcentage_maison", textNA="Valeur manquante", style = "jenks") +
      tm_polygons() +
      tm_facets("annee_mutation") +
      tm_borders("white", lwd = .5)

  ############################ Prix moyen au m2 par type de bien par quartier et année sur Nantes #####################################

    # Agrégat ventes par type de bien, quartier et année
    ventesNantes <- dvfNantes_with_year %>%
      filter(
        nature_mutation == "Vente"
      ) %>%
      group_by(type_local, nom, annee_mutation) %>%
      summarise_at(vars(valeur_fonciere, surface_reelle_bati), sum) %>%
      mutate(prix_m2=valeur_fonciere/surface_reelle_bati)

    ventesNantes <- st_drop_geometry(ventesNantes) %>%
      left_join(quartierNantes) %>%
      st_as_sf()

    # Carte du prix moyen au m2 par type de bien par quartier et année sur Nantes
    ventesNantes %>%
      tm_shape()+
      tm_fill("prix_m2",title="Prix au m² moyen") +
      tm_polygons("prix_m2", textNA="Valeur manquante", style = "jenks") +
      tm_polygons() +
      tm_facets("annee_mutation") +
      tm_facets("type_local") +
      tm_borders("white", lwd = .5)

    # Carte du prix moyen au m2 des maisons par quartier et année sur Nantes
    ventesNantes %>%
      filter(
        type_local %in% c("Maison")
      ) %>%
      tm_shape()+
      tm_fill("prix_m2",title="Prix au m² moyen\nd'une maison") +
      tm_polygons("prix_m2", textNA="Valeur manquante", style = "jenks") +
      tm_polygons() +
      tm_facets("annee_mutation") +
      tm_borders("white", lwd = .5)

  output$datatable <- renderDT({
    datatable(dvfNantes_with_year)
  })
}
