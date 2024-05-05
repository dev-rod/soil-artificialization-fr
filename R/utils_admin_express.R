#' getOreeAnjouCommune AdminExpress Function
#'
#' @description Communes de La Varenne, Champtoceaux etc.. sur la CC Orée d'Anjou
#'
#' @param insee_com character list
#'
#' @noRd
#'
#' @import sf dplyr
#'
getOreeAnjouCommune <- function(insee_com = c("49069")){
  #AdminExpressComAssoDel <- sf::st_read(dsn = 'data/contours_administratifs_france/src/ign_admin_express/2024/ADMIN-EXPRESS_3-2__SHP_LAMB93_FXX_2024-03-25/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2024-03-00237/ADE_3-2_SHP_LAMB93_FXX-ED2024-03-25/COMMUNE_ASSOCIEE_OU_DELEGUEE.shp')
  #save(AdminExpressComAssoDel, file='data/contours_administratifs_france/AdminExpressComAssoDel.RData')
  load('data/contours_administratifs_france/AdminExpressComAssoDel.RData')
  ComAssoDelLimitOreeAnjou2024 <- AdminExpressComAssoDel |>
    dplyr::filter(INSEE_COM %in% insee_com)
  #mymap(df=ComAssoDelLimitOreeAnjou2024, label="Population", label_field="NOM", heat_field="POPULATION")
  return(ComAssoDelLimitOreeAnjou2024)
}

#' getFrArr AdminExpress Function
#'
#' @description Arrondissements au sein des départements, Ex Segré, Saumur, Cholet et Angers sur le 49
#'
#' @param insee_dep character list
#'
#' @noRd
#'
#' @import sf dplyr
#'
getFrArr <- function(insee_dep = c("49")){
  # AdminExpressArr <- sf::st_read(dsn = 'data/contours_administratifs_france/src/ign_admin_express/2024/ADMIN-EXPRESS_3-2__SHP_LAMB93_FXX_2024-03-25/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2024-03-00237/ADE_3-2_SHP_LAMB93_FXX-ED2024-03-25/ARRONDISSEMENT.shp')
  # save(AdminExpressArr, file='data/contours_administratifs_france/AdminExpressArr.RData')
  load('data/contours_administratifs_france/AdminExpressArr.RData')
  ArrOreeAnjou2024 <- AdminExpressArr |>
    dplyr::filter(INSEE_DEP %in% insee_dep)
  #mymap(df=ArrOreeAnjou2024, label="Arrondissements", label_field="NOM")
  return(ArrOreeAnjou2024)
}

#' getFrArrMun AdminExpress Function
#'
#' @description Arrondissements de Paris, Lyon et Marseille
#'
#' @noRd
#'
#' @import sf dplyr
#'
getFrArrMun <- function(){
  # AdminExpressArrMun <- sf::st_read(dsn = 'data/contours_administratifs_france/src/ign_admin_express/2024/ADMIN-EXPRESS_3-2__SHP_LAMB93_FXX_2024-03-25/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2024-03-00237/ADE_3-2_SHP_LAMB93_FXX-ED2024-03-25/ARRONDISSEMENT_MUNICIPAL.shp')
  # save(AdminExpressArrMun, file='data/contours_administratifs_france/AdminExpressArrMun.RData')
  load('data/contours_administratifs_france/AdminExpressArrMun.RData')
  ArrMunicipal2024 <- AdminExpressArrMun
  #mymap(df=ArrMunicipal2024, label="Arrondissements municipal", label_field="NOM")
  return(ArrMunicipal2024)
}

#' getFrCantons AdminExpress Function
#'
#' @description Cantons
#'
#' @param insee_dep character list
#'
#' @noRd
#'
#' @import sf dplyr
#'
getFrCantons <- function(insee_dep = c("49")){
  # AdminExpressCanton <- sf::st_read(dsn = 'data/contours_administratifs_france/src/ign_admin_express/2024/ADMIN-EXPRESS_3-2__SHP_LAMB93_FXX_2024-03-25/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2024-03-00237/ADE_3-2_SHP_LAMB93_FXX-ED2024-03-25/CANTON.shp')
  # save(AdminExpressCanton, file='data/contours_administratifs_france/AdminExpressCanton.RData')
  load('data/contours_administratifs_france/AdminExpressCanton.RData')
  CantonOreeAnjou2024 <- AdminExpressCanton |>
    dplyr::filter(INSEE_DEP %in% insee_dep)
  #mymap(df=CantonOreeAnjou2024, label="Cantons")
  return(CantonOreeAnjou2024)
}

#' getFrChfLieuArrMunicipal AdminExpress Function
#'
#' @description Chef lieu d'arrondissement municipal : Paris, Lyon, Marseille
#'
#' @noRd
#'
#' @import sf dplyr
#'
getFrChfLieuArrMunicipal <- function(){
  # AdminExpressChfLieuArrMun <- sf::st_read(dsn = 'data/contours_administratifs_france/src/ign_admin_express/2024/ADMIN-EXPRESS_3-2__SHP_LAMB93_FXX_2024-03-25/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2024-03-00237/ADE_3-2_SHP_LAMB93_FXX-ED2024-03-25/CHFLIEU_ARRONDISSEMENT_MUNICIPAL.shp')
  # save(AdminExpressChfLieuArrMun, file='data/contours_administratifs_france/AdminExpressChfLieuArrMun.RData')
  load('data/contours_administratifs_france/AdminExpressChfLieuArrMun.RData')
  ChfLieuArrMunicipalOreeAnjou2024 <- AdminExpressChfLieuArrMun
  #mymap(df=ChfLieuArrMunicipalOreeAnjou2024, label="Chef-lieux arrondissements municipal", label_field="NOM")
  return(ChfLieuArrMunicipalOreeAnjou2024)
}

#' getFrChfLieuCommune AdminExpress Function
#'
#' @description Chef lieu de commune
#'
#' @param insee_dep character list
#'
#' @noRd
#'
#' @import sf dplyr
#'
getFrChfLieuCommune <- function(insee_dep = c('49')){
  # AdminExpressChfLieuCommune <- sf::st_read(dsn = 'data/contours_administratifs_france/src/ign_admin_express/2024/ADMIN-EXPRESS_3-2__SHP_LAMB93_FXX_2024-03-25/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2024-03-00237/ADE_3-2_SHP_LAMB93_FXX-ED2024-03-25/CHFLIEU_COMMUNE.shp')
  # save(AdminExpressChfLieuCommune, file='data/contours_administratifs_france/AdminExpressChfLieuCommune.RData')
  load('data/contours_administratifs_france/AdminExpressChfLieuCommune.RData')
  ChfLieuCommuneOreeAnjou2024 <- AdminExpressChfLieuCommune |>
    dplyr::filter(INSEE_DEP %in% insee_dep)
  #mymap(df=ChfLieuCommuneOreeAnjou2024, label="Chef lieu de commune", label_field="NOM")
  return(ChfLieuCommuneOreeAnjou2024)
}

#' getFrCollTerritoriale AdminExpress Function
#'
#' @description Collectivités territoriales, grep insee_cols param on INSEE_COL field
#'
#' @param insee_cols character list
#'
#' @noRd
#'
#' @import sf dplyr
#'
getFrCollTerritoriale <- function(insee_cols = c("49")){
  # AdminExpressCollTerritoriale <- sf::st_read(dsn = 'data/contours_administratifs_france/src/ign_admin_express/2024/ADMIN-EXPRESS_3-2__SHP_LAMB93_FXX_2024-03-25/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2024-03-00237/ADE_3-2_SHP_LAMB93_FXX-ED2024-03-25/COLLECTIVITE_TERRITORIALE.shp')
  # save(AdminExpressCollTerritoriale, file='data/contours_administratifs_france/AdminExpressCollTerritoriale.RData')
  load('data/contours_administratifs_france/AdminExpressCollTerritoriale.RData')
  CollTerritorialeOreeAnjou2024 <- AdminExpressCollTerritoriale |>
    dplyr::filter(grepl(paste(insee_cols, collapse = "|"), INSEE_COL))
  #mymap(df=CollTerritorialeOreeAnjou2024, label="Collectivités territoriales", label_field="NOM")
  return(CollTerritorialeOreeAnjou2024)
}

#' getFrCommunes AdminExpress Function
#'
#' @description Communes
#'
#' @param insee_dep character list
#'
#' @noRd
#'
#' @import sf dplyr
#'
getFrCommunes <- function(insee_dep = c()){
  # AdminExpressCommune <- sf::st_read(dsn = 'data/contours_administratifs_france/src/ign_admin_express/2024/ADMIN-EXPRESS_3-2__SHP_LAMB93_FXX_2024-03-25/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2024-03-00237/ADE_3-2_SHP_LAMB93_FXX-ED2024-03-25/COMMUNE.shp')
  # save(AdminExpressCommune, file='data/contours_administratifs_france/AdminExpressCommune.RData')
  load('data/contours_administratifs_france/AdminExpressCommune.RData')
  if(length(insee_dep) > 0) {
    AdminExpressCommune <- AdminExpressCommune |>
    dplyr::filter(INSEE_DEP %in% insee_dep)
  }
  #mymap(df=AdminExpressCommune, label="Population", label_field="NOM", heat_field="POPULATION")
  return(AdminExpressCommune)
}

#' getFrDept AdminExpress Function
#'
#' @description Départements
#'
#' @noRd
#'
#' @import sf dplyr
#'
getFrDept <- function(){
  # AdminExpressDept <- sf::st_read(dsn = 'data/contours_administratifs_france/src/ign_admin_express/2024/DEPARTEMENT.shp')
  # save(AdminExpressDept, file='data/contours_administratifs_france/AdminExpressDept.RData')
  load('data/contours_administratifs_france/AdminExpressDept.RData')
  DepartementOreeAnjou2024 <- AdminExpressDept
  #mymap(df=DepartementOreeAnjou2024, label="Départements", label_field="NOM")
  return(DepartementOreeAnjou2024)
}

#' getFrEpci AdminExpress Function
#'
#' @description EPCI France
#' TODO jointure avec data/contours_administratifs_france/identifiants-epci-2022.csv
#' ID, CODE_SIREN, NOM, NATURE
#'
#' @noRd
#'
#' @import sf dplyr
#'
getFrEpci <- function(){
  # AdminExpressEpci <- sf::st_read(dsn = 'data/contours_administratifs_france/src/ign_admin_express/2024/ADMIN-EXPRESS_3-2__SHP_LAMB93_FXX_2024-03-25/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2024-03-00237/ADE_3-2_SHP_LAMB93_FXX-ED2024-03-25/EPCI.shp')
  # save(AdminExpressEpci, file='data/contours_administratifs_france/AdminExpressEpci.RData')
  load('data/contours_administratifs_france/AdminExpressEpci.RData')
  EPCIOreeAnjou2024 <- AdminExpressEpci
  #mymap(df=EPCIOreeAnjou2024, label="EPCI", label_field="NOM")
  return(EPCIOreeAnjou2024)
}

#' getFrRegion AdminExpress Function
#'
#' @description Region France
#'
#' @noRd
#'
#' @import sf dplyr
#'
getFrRegion <- function(){
  # AdminExpressRegion <- sf::st_read(dsn = 'data/contours_administratifs_france/src/ign_admin_express/2024/ADMIN-EXPRESS_3-2__SHP_LAMB93_FXX_2024-03-25/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2024-03-00237/ADE_3-2_SHP_LAMB93_FXX-ED2024-03-25/REGION.shp')
  # save(AdminExpressRegion, file='data/contours_administratifs_france/AdminExpressRegion.RData')
  load('data/contours_administratifs_france/AdminExpressRegion.RData')
  RegionOreeAnjou2024 <- AdminExpressRegion
  #mymap(df=RegionOreeAnjou2024, label="Regions", label_field="NOM")
  return(RegionOreeAnjou2024)
}

#' adminexpress UI Function
#'
#' @description Helper to test adminexpress
#' Découpage administratif du territoire français (commune, arrondissement départemental, département, région...).
#' https://geoservices.ign.fr/adminexpress
#' Chargement des couches REGION_CARTO et EPCI_CARTO dans data/contours_administratif_france/adminexpress
#' Trouver les EPCI de la région pays de la loire à cheval sur d'autres régions
#'
#' @noRd
#'
#' @import dplyr sf mapview
#'
# adminexpress <- function(){
#   reg <- sf::st_read(dsn = 'data/contours_administratifs_france/src/ign_admin_express/REGION_CARTO.shp')
#   mapview::mapview(reg)
#   epci <- sf::st_read(dsn = 'data/contours_administratifs_france/src/ign_admin_express/EPCI_CARTO.shp')
#
#   # Data preparation
#   reg_pdl <- reg |>
#     dplyr::filter(INSEE_REG=='52') |>
#     sf::st_buffer(dist=-3000)
#
#   reg_limitrophes_pdl <- reg |>
#     dplyr::filter(INSEE_REG %in% c('53','28','24','75')) |>
#     sf::st_buffer(dist=-3000)
#
#   # Version avec st_intersect
#   epci_pdl <- epci[reg_pdl,,op=st_intersects]
#   mapview::mapview(epci_pdl)
#
#   epci_pdl_plusieurs_regions <- epci_pdl[reg_limitrophes_pdl,,op=st_intersects]
#   mapview::mapview(epci_pdl_plusieurs_regions)
#
#   # Version avec st_overlaps
#   epci_pdl_plusieurs_regions_avec_overlaps <- epci[reg |>
#                                                      dplyr::filter(INSEE_REG=='52'),,
#                                                    op=st_overlaps]
#   # Visualisation
#   mapview::mapview(epci_pdl_plusieurs_regions_avec_overlaps)
#   mapview::mapview(list(epci_pdl_plusieurs_regions,reg_pdl))
# }

#' adminexpress UI Function
#'
#' @description Helper to test adminexpress
#' Carte des départements et des régions à partir d'admin express
#'
#' @noRd
#'
#' @import dplyr sf mapview
#'
# adminexpress2 <- function(){
#
#   load("data/admin_express.RData")
#
#   class(departements_geo)
#
#   mapview(departements_geo, zcol = "NOM_DEP", legend = F)
#
#   departements_geo52 <- departements_geo |>
#     dplyr::filter(INSEE_REG == 52)
#
#   mapview::mapview(departements_geo52, zcol = "NOM_DEP", legend = T)
#
#   regions <- departements_geo |>
#     dplyr::group_by(INSEE_REG) |>
#     dplyr::summarise(AREA = sum(AREA))
#
#   dplyr::glimpse(regions)
#   mapview::mapview(regions, zcol = "INSEE_REG", legend = F)
#
#   regions <- regions |>
#     dplyr::left_join(regions_geo |>
#                 sf::st_drop_geometry(),
#               by = c("INSEE_REG")
#     )
#
#   mapview::mapview(regions, zcol = "NOM_REG", legend = F)
#   class(regions)
#
#   departement_44 <- departements_geo |>
#     dplyr::filter(INSEE_DEP == "44")
#
#   epci_d44 <- epci_geo[departement_44, , op = st_within]
#
#   mapview::mapview(list(departement_44, epci_d44), zcol = c("NOM_DEP", "NOM_EPCI"), legend = F)
#
#   dplyr::glimpse(epci_geo)
#   dplyr::glimpse(regions_geo)
#
#   epci_d44_buffer <- epci_geo[departement_44_buffer, , op = st_within]
#   departement_44_buffer <- departement_44 |>
#     sf::st_buffer(dist = 5000)
#   mapview::mapview(list(departement_44_buffer, departement_44), layer.name = c("Loire-Atlantique avec un buffer de 5 km", "Loire-Atlantique"), zcol = c("NOM_DEP", "NOM_DEP"), col.regions = list("#440154FF", "#FDE725FF"))
#   epci_d44_buffer <- epci_geo[departement_44_buffer, , op = st_within]
#   mapview::mapview(list(departement_44_buffer, epci_d44_buffer), zcol = c("NOM_DEP", "NOM_EPCI"), legend = F)
#
#   departement_44_buffer_negatif <- departement_44 |>
#   sf::st_buffer(dist = -2000)
#
#   epci_d44 <- epci_geo[departement_44_buffer_negatif, , op = st_intersects]
#
#   mapview::mapview(list(departement_44, epci_d44), zcol = c("NOM_DEP", "NOM_EPCI"), legend = F)
#
#   # polygone (a)
#   a_poly <- sf::st_polygon(list(rbind(c(-1, -1), c(1, -1), c(1, 1), c(-1, -1))))
#   a <- sf::st_sfc(a_poly)
#   # ligne (l)
#   l1 <- sf::st_multilinestring(list(rbind(c(0.5, -1), c(-0.5, 1))))
#   l2 <- sf::st_multilinestring(list(rbind(c(.9, -.9), c(.5, 0))))
#   l <- sf::st_sfc(l1, l2)
#
#   # multipoints (p)
#   p_matrix <- matrix(c(0.5, 1, -1, 0, 0, 1, 0.5, 1), ncol = 2)
#   p_multi <- sf::st_multipoint(x = p_matrix)
#   p <- sf::st_cast(sf::st_sfc(p_multi), "POINT")
#   sf::st_intersects(p, a)
#   sf::st_within(p, a, sparse = F)
#
#   sf::st_contains(a, l, sparse = F)
#   sf::st_within(l, a, sparse = F)
# }
