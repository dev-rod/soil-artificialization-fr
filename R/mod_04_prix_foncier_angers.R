# Exploiter les données DVF sur les transactions immobilières
# et obtenir un prix moyen des transactions par quartier dans l’ancien sur Angers
#
# - Données DVF : http://api.cquest.org/dvf (obsolète)
# - Contour des quartiers d'Angers :
# https://www.data.gouv.fr/fr/datasets/limite-des-quartiers-de-la-ville-dangers-1/
# origine https://data.angers.fr/explore/dataset/ccq_007/export/
#
# Résultat par quartier et annee :
# - Volume de ventes
# - Pourcentage de maison/appartement dans les ventes
# - Prix moyen et median au m2 par type de bien

# Module UI

#' @title   mod_foncier_angers_ui and mod_foncier_angers_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_foncier_angers
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList includeMarkdown
mod_foncier_angers_ui <- function(id){
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
#library(httr)
#library(jsonlite)
###library(sf)
###library(tidyverse)
###library(lubridate)
## @importFrom dplyr pull
#' @keywords internal

mod_foncier_server <- function(input, output, session){
  ns <- session$ns

  # Chargement des données DVF
  # On utilise l'API pour récupérer les données de Angers (code commune 49007)
  # On ne garde que les données avec une géolocalisation valide, un prix et une surface renseignés.
  # https://www.data.gouv.fr/fr/reuses/micro-api-dvf-demande-de-valeurs-foncieres/
  # l'api de christian quest n'est plus disponible 502 bad gateway
  # https://github.com/cquest/dvf_as_api

  #get_dvf <- GET("https://api.cquest.org/dvf?code_commune=49007")
  #dvf_content <- content(get_dvf, "text")

  #dvf_json <- fromJSON(dvf_content)$resultats %>%
  #  filter(!is.na(lon), !is.na(lat), !is.na(valeur_fonciere), !is.na(surface_relle_bati))
  # dvf <- st_as_sf(dvf_json,
  #                 coords = c("lon", "lat"),
  #                 crs = 4326
  # )

  # https://app.dvf.etalab.gouv.fr/
  # https://files.data.gouv.fr/geo-dvf/latest/csv/2019/communes/49/
  dvf_csv <- read.csv(
    'data/dvf/2019/49007.csv',
    sep = ",",
    fileEncoding = "UTF-8"
  ) %>%
    filter(!is.na(longitude), !is.na(latitude), !is.na(valeur_fonciere), !is.na(surface_reelle_bati))

  dvf <- st_as_sf(dvf_csv,
    coords = c("longitude", "latitude"),
    crs = 4326
  )

  # Carte des quartiers d'Angers
  quartier_angers <- st_read(dsn = "data/quartier_angers/ccq_007.geojson") %>%
        select(nom_ccq) %>%
    rename(quartier = nom_ccq)
  #mapview(quartier_angers)

  # Data préparation
  # Jointure spatiale pour récupérer les ventes par quartiers

  dvf_avec_quartier <- st_join(
    dvf,
    quartier_angers)
  #mapview(dvf_avec_quartier)

  # Calculs

  # Volume de ventes
  stat <- dvf_avec_quartier %>%
    st_drop_geometry() %>%
      mutate(
      date_mutation = ymd(date_mutation),
      annee_mutation = year(date_mutation)
    )

  ventes <- stat %>%
      filter(
      nature_mutation == "Vente",
      type_local %in% c("Appartement", "Maison")
    ) %>%
      group_by(quartier,type_local,  annee_mutation) %>%
    tally()

  ventes_total <- stat %>%
      filter(
      nature_mutation == "Vente",
      type_local %in% c("Appartement", "Maison")
    ) %>%
      group_by(quartier,  annee_mutation) %>%
    tally()
  Ventes_maisons <- stat %>%
      filter(
      nature_mutation == "Vente",
      type_local %in% c("Maison")
    ) %>%
      group_by(quartier,  annee_mutation) %>%
    tally()

  ## Pourcentage de maisons dans les ventes
  # Version avec tidyr
  # library(tidyr)
  pourcentage_maison <- ventes %>%
    pivot_wider(names_from = type_local,values_from = n) %>%
    mutate(pourcentage_maison = Maison*100/(Maison+Appartement))

  # Version avec jointure
  Ventes_maisons <- Ventes_maisons %>%
    rename(nb_maisons = n) %>%
    left_join(ventes_total) %>%
    mutate(pourcentage_maison = nb_maisons*100/(n))

  ## Prix moyen et median au m2 par type de bien
  prix_m2_moyen <-  stat %>%
    filter(
      nature_mutation == "Vente",
      type_local %in% c("Appartement", "Maison")
    ) %>%
    group_by(quartier, type_local, annee_mutation) %>%
    summarise_at(vars(valeur_fonciere, surface_reelle_bati), sum) %>%
    mutate(prix_m2=valeur_fonciere/surface_reelle_bati)

  prix_m2_median <-  stat %>%
    filter(
      nature_mutation == "Vente",
      type_local %in% c("Appartement", "Maison")
    ) %>%
    mutate(prix_m2=valeur_fonciere/surface_reelle_bati) %>%
    group_by(quartier, type_local, annee_mutation) %>%
    summarise_at(vars(prix_m2), median)

  # Datavisualisaton
  ventes_geo <- quartier_angers %>%
    left_join(ventes)
  pourcentage_maison_geo <- quartier_angers %>%
    left_join(pourcentage_maison)
  prix_m2_moyen_geo <- quartier_angers %>%
    left_join(prix_m2_moyen)
  prix_m2_median_geo <- quartier_angers %>%
    left_join(prix_m2_median)

  # Premières valorisations avec ggplot
  ggplot() +
    geom_sf(
      data = ventes_geo %>%
        filter(annee_mutation == 2019,
               type_local=="Maison"),
      aes(fill = n)
    )
  ggplot() +
    geom_sf(
      data = ventes_geo %>%
        filter(annee_mutation == 2019,
               type_local=="Appartement"),
      aes(fill = n)
    )
  ggplot() +
    geom_sf(
      data = pourcentage_maison_geo %>%
        filter(annee_mutation == 2019),
      aes(fill = pourcentage_maison)
    )
  ggplot() +
    geom_sf(
      data = prix_m2_median_geo %>%
        filter(annee_mutation == 2019,
               type_local=="Appartement"),
      aes(fill = prix_m2)
    )
  ggplot() +
    geom_sf(
      data = prix_m2_median_geo %>%
        filter(annee_mutation == 2019,
               type_local=="Maison"),
      aes(fill = prix_m2)
    )

  output$datatable <- renderDT({
    datatable(dvf_avec_quartier)
  })
}

