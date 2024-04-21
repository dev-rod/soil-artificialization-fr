#' sirene UI Function
#'
#' @description Helper to test sirene - DRAFT
#' Données sirene 44
#' Système national d'identification et du répertoire des entreprises et de leurs établissements
#' https://www.data.gouv.fr/fr/datasets/base-sirene-des-entreprises-et-de-leurs-etablissements-siren-siret/
#'
#' @noRd
#'
#' @import dplyr sf mapview
#'
sirene <- function(){
  # with_package(
  #   "ggplot2",
  #   r <- ls("package:ggplot2", pattern = "^theme_")
  # )
  load("data/sirene44_2019_03/sirene_zones_commerciales_44.RData")
  sirene44_sel <- sirene44 |>
    dplyr::filter(APET700 == "0893Z")

  mapview::mapview(list(departement_44, epci_d44, sirene44_sel), zcol = c("NOM_DEP", "NOM_EPCI", "NOMEN_LONG"), legend = F)
  sirene44_sel_avec_code_epci <- sirene44_sel |>
    sf::st_join(epci_geo)
  mapview::mapview(list(departement_44, epci_d44, sirene44_sel_avec_code_epci), zcol = c("NOM_DEP", "NOM_EPCI", "NOM_EPCI"), legend = F)
}
