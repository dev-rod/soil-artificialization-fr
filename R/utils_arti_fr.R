#' getArtiData20092022_OLD arti_fr Function
#'
#' @description Mesures de l'artificialisation des communes en France entre 2009 et 2022, cerema data
#'
#' @noRd
#'
#' @import sf dplyr
#'
getArtiData20092022_OLD <- function(){
  #arti_fr_2009_2022 <- sf::st_read(dsn = 'data/mesures_artificialisation_commune/src/2024/obs_artif_conso_com_2009_2022/obs_artif_conso_com_2009_2022.shp')
  #save(arti_fr_2009_2022, file = "data/mesures_artificialisation_commune/arti_fr_2009_2022.RData")
  load("data/mesures_artificialisation_commune/arti_fr_2009_2022.RData")

  arti_fr_2009_2022 <- arti_fr_2009_2022 |>
    dplyr::mutate(
      IDREG = as.factor(IDREG),
      EPCI22 = as.factor(EPCI22)
      # Aires urbaines remplacées par les aires d'attraction des communes
      # https://www.insee.fr/fr/information/4803954
      # https://www.insee.fr/fr/statistiques/fichier/4803954/poster_zaav.png
      #AAV2020 : Aire d’attraction des villes 2020
      #AAV2020TXT : Libellé de l'aire d attraction des villes
      #AAV2020_TY : Typologie de l'aire d attraction des villes
    ) |>
    dplyr::ungroup()

  return(arti_fr_2009_2022)
}

#' getArtiData20092022 arti_fr Function
#'
#' @description Mesures de la consommation d'espaces naturels, agricoles et forestiers (NAF)
#' des communes en France entre 2009 et 2022 en m2 (données Cerema)
#'
#' @noRd
#'
#' @import sf dplyr
#'
getArtiData20092022 <- function(){
  #arti_fr_2009_2022 <- sf::st_read(dsn = 'data/mesures_artificialisation_commune/src/2024/obs_artif_conso_com_2009_2022/obs_artif_conso_com_2009_2022.shp')
  #save(arti_fr_2009_2022, file = "data/mesures_artificialisation_commune/arti_fr_2009_2022.RData")
  load("data/mesures_artificialisation_commune/arti_fr_2009_2022.RData")

  arti_fr_2009_2022_pivoted <- arti_fr_2009_2022 |>
    dplyr::mutate(
      IDREG = as.factor(IDREG),
      EPCI22 = as.factor(EPCI22)
    ) |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(
      #cols = c(NAF09ART10,ART09ACT10,ART09HAB10,ART09MIX10,ART09ROU10,ART09FER10,ART09INC10,NAF10ART11,ART10ACT11,ART10HAB11,ART10MIX11,ART10ROU11,ART10FER11,ART10INC11,NAF11ART12,ART11ACT12,ART11HAB12,ART11MIX12,ART11ROU12,ART11FER12,ART11INC12,NAF12ART13,ART12ACT13,ART12HAB13,ART12MIX13,ART12ROU13,ART12FER13,ART12INC13,NAF13ART14,ART13ACT14,ART13HAB14,ART13MIX14,ART13ROU14,ART13FER14,ART13INC14,NAF14ART15,ART14ACT15,ART14HAB15,ART14MIX15,ART14ROU15,ART14FER15,ART14INC15,NAF15ART16,ART15ACT16,ART15HAB16,ART15MIX16,ART15ROU16,ART15FER16,ART15INC16,NAF16ART17,ART16ACT17,ART16HAB17,ART16MIX17,ART16ROU17,ART16FER17,ART16INC17,NAF17ART18,ART17ACT18,ART17HAB18,ART17MIX18,ART17ROU18,ART17FER18,ART17INC18,NAF18ART19,ART18ACT19,ART18HAB19,ART18MIX19,ART18ROU19,ART18FER19,ART18INC19,NAF19ART20,ART19ACT20,ART19HAB20,ART19MIX20,ART19ROU20,ART19FER20,ART19INC20,NAF20ART21,ART20ACT21,ART20HAB21,ART20MIX21,ART20ROU21,ART20FER21,ART20INC21,NAF21ART22,ART21ACT22,ART21HAB22,ART21MIX22,ART21ROU22,ART21FER22,ART21INC22,NAF09ART22,ART09ACT22,ART09HAB22,ART09MIX22,ART09INC22,ART09ROU22,ART09FER22),
      cols = c(NAF09ART10,NAF10ART11,NAF11ART12,NAF12ART13,NAF13ART14,NAF14ART15,NAF15ART16,NAF16ART17,NAF17ART18,NAF18ART19,NAF19ART20,NAF20ART21,NAF21ART22),
      # names_to = c("typeArti", "year"),
      # names_pattern = "([A-Za-z]+\\d\\d[A-Za-z]+)(\\d+$)", # out x 2 : typeArti & year : Ex (ART09INC)(10)
      # names_pattern = "(\\d+$)", # out x 1 : year : Ex (10)
      names_to = c("year"),
      names_pattern = "[A-Za-z]+(\\d\\d)", # out x 1 : year : Ex (10)
      # values_to = c("m2")
      values_to = c("totalArti")
    ) |>
    dplyr::mutate(year = paste0("20", year)) |>
    #dplyr::mutate(typeArti = stringr::substr(typeArti, 6, 8)) |>
    dplyr::select(year, everything()) |>
    dplyr::mutate(activite = dplyr::case_when(
      year == 2009 ~ ART09ACT10,
      year == 2010 ~ ART10ACT11,
      year == 2011 ~ ART11ACT12,
      year == 2012 ~ ART12ACT13,
      year == 2013 ~ ART13ACT14,
      year == 2014 ~ ART14ACT15,
      year == 2015 ~ ART15ACT16,
      year == 2016 ~ ART16ACT17,
      year == 2017 ~ ART17ACT18,
      year == 2018 ~ ART18ACT19,
      year == 2019 ~ ART19ACT20,
      year == 2020 ~ ART20ACT21,
      year == 2021 ~ ART21ACT22,
      .default = NULL
    )) |>
    dplyr::mutate(habitat = dplyr::case_when(
      year == 2009 ~ ART09HAB10,
      year == 2010 ~ ART10HAB11,
      year == 2011 ~ ART11HAB12,
      year == 2012 ~ ART12HAB13,
      year == 2013 ~ ART13HAB14,
      year == 2014 ~ ART14HAB15,
      year == 2015 ~ ART15HAB16,
      year == 2016 ~ ART16HAB17,
      year == 2017 ~ ART17HAB18,
      year == 2018 ~ ART18HAB19,
      year == 2019 ~ ART19HAB20,
      year == 2020 ~ ART20HAB21,
      year == 2021 ~ ART21HAB22,
      .default = NULL
    )) |>
    dplyr::mutate(mixte = dplyr::case_when(
      year == 2009 ~ ART09MIX10,
      year == 2010 ~ ART10MIX11,
      year == 2011 ~ ART11MIX12,
      year == 2012 ~ ART12MIX13,
      year == 2013 ~ ART13MIX14,
      year == 2014 ~ ART14MIX15,
      year == 2015 ~ ART15MIX16,
      year == 2016 ~ ART16MIX17,
      year == 2017 ~ ART17MIX18,
      year == 2018 ~ ART18MIX19,
      year == 2019 ~ ART19MIX20,
      year == 2020 ~ ART20MIX21,
      year == 2021 ~ ART21MIX22,
      .default = NULL
    )) |>
    dplyr::mutate(route = dplyr::case_when(
      year == 2009 ~ ART09ROU10,
      year == 2010 ~ ART10ROU11,
      year == 2011 ~ ART11ROU12,
      year == 2012 ~ ART12ROU13,
      year == 2013 ~ ART13ROU14,
      year == 2014 ~ ART14ROU15,
      year == 2015 ~ ART15ROU16,
      year == 2016 ~ ART16ROU17,
      year == 2017 ~ ART17ROU18,
      year == 2018 ~ ART18ROU19,
      year == 2019 ~ ART19ROU20,
      year == 2020 ~ ART20ROU21,
      year == 2021 ~ ART21ROU22,
      .default = NULL
    )) |>
    dplyr::mutate(chemin_fer = dplyr::case_when(
      year == 2009 ~ ART09FER10,
      year == 2010 ~ ART10FER11,
      year == 2011 ~ ART11FER12,
      year == 2012 ~ ART12FER13,
      year == 2013 ~ ART13FER14,
      year == 2014 ~ ART14FER15,
      year == 2015 ~ ART15FER16,
      year == 2016 ~ ART16FER17,
      year == 2017 ~ ART17FER18,
      year == 2018 ~ ART18FER19,
      year == 2019 ~ ART19FER20,
      year == 2020 ~ ART20FER21,
      year == 2021 ~ ART21FER22,
      .default = NULL
    )) |>
    dplyr::mutate(autres = dplyr::case_when(
      year == 2009 ~ ART09INC10,
      year == 2010 ~ ART10INC11,
      year == 2011 ~ ART11INC12,
      year == 2012 ~ ART12INC13,
      year == 2013 ~ ART13INC14,
      year == 2014 ~ ART14INC15,
      year == 2015 ~ ART15INC16,
      year == 2016 ~ ART16INC17,
      year == 2017 ~ ART17INC18,
      year == 2018 ~ ART18INC19,
      year == 2019 ~ ART19INC20,
      year == 2020 ~ ART20INC21,
      year == 2021 ~ ART21INC22,
      .default = NULL
    )) |>
    dplyr::select(year,IDCOM,IDCOMTXT,IDREG,IDREGTXT,IDDEP,IDDEPTXT,EPCI22,EPCI22TXT,SCOT,AAV2020,AAV2020TXT,AAV2020_TY,ART09ACT22,ART09HAB22,ART09MIX22,ART09INC22,ART09ROU22,ART09FER22,ARTCOM2020,POP13,POP19,POP1319,MEN13,MEN19,MEN1319,EMP13,EMP19,EMP1319,MEPART1319,MENHAB1319,ARTPOP1319,SURFCOM202,geometry,totalArti,activite,habitat,mixte,route,chemin_fer,autres) |>
    dplyr::arrange(year)

  return(arti_fr_2009_2022_pivoted)
}

# Chargement des Indicateurs de consommation d'espace - fraicheur de données 04/2024
#g_arti_fr_2009_2022 <- getArtiData20092022()
#g_arti_fr_2009_2022_no_geom <- g_arti_fr_2009_2022 |>
#  sf::st_set_geometry(NULL)
#save(g_arti_fr_2009_2022_no_geom, file='data/mesures_artificialisation_commune/g_arti_fr_2009_2022_no_geom.RData')
load('data/mesures_artificialisation_commune/g_arti_fr_2009_2022_no_geom.RData')

#' Fonction calculant les agrégats sur une zone à partir des carreaux qui la recouvrent
#' https://cerema.app.box.com/v/pnb-action7-indicateurs-ff/file/1270338746048
#' fonction inspirée de celle du CEREMA proposée sur https://www.insee.fr/fr/statistiques/4176290?sommaire=4176305
#'
#' @param cheminFichierContoursSHP Chemin des contours de la ou des zones à croiser avec les carreaux de la bdd filosofi
#' @param listeCodesCommune Liste des codes Insee des communes
#'
#' @return Une table contenant une ligne pour chaque zone du fichier de contours
#'
#' @import data.table dplyr sf stringr
#'
#' @examples
#' calculAgregatsZones("c:/mesdocs/moncontour.shp", c("01001","01002"))
#'
calculAgregatsZones <- function(fichierContoursSHP, listeCodesCommune) {

  # Carroyage 200m de consommation d'espace en France métropolitaine
  # https://cerema.app.box.com/v/pnb-action7-indicateurs-ff/file/1270338746048
  #artiCarreauxFr2024 <- sf::st_read(dsn = 'data/mesures_artificialisation_commune/2024/obs_artif_conso_com_2009_2022_carroyage_LEA/obs_artif_conso_com_2009_2022_carroyage_LEA.shp')
  # carreauxFr1km2024_49069INSEE <- sf::st_read(dsn = 'data/filosofi/grille200m_shp/grille200m_metropole_shp/grille200m_metropole.shp') |>

  #cheminFichierCSV <- "data/filosofi/src/Filosofi2015_carreaux_200m_csv/Filosofi2015_carreaux_200m_csv/Filosofi2015_carreaux_200m_metropole.csv"
  # Pour chaque carreau, identifiant, code commune, nombre d'individus et
  # variable indiquant si les données sont des valeurs approchées ou non
  # retrait ici de Id_carr1km, Id_carr_n, Groupe, I_pauv, Id_car2010, I_est_1km
  #listeIndic = unique(c("IdINSPIRE","Depcom","I_est_cr","Ind","Men","Men_pauv","Men_1ind","Men_5ind","Men_prop","Men_fmp","Ind_snv",
  #               "Men_surf","Men_coll","Men_mais","Log_av45","Log_45_70","Log_70_90","Log_ap90",
  #               "Log_inc","Log_soc","Ind_0_3","Ind_4_5","Ind_6_10","Ind_11_17","Ind_18_24",
  #               "Ind_25_39","Ind_40_54","Ind_55_64","Ind_65_79","Ind_80p","Ind_inc"))

  # carreauxFiLoSoFi <- data.table::fread(cheminFichierCSV, select = c(listeIndic)) |>
  #   dplyr::mutate(I_est_cr = as.integer(I_est_cr))
  # save(carreauxFiLoSoFi, file='data/filosofi/carreauxFiLoSoFi.RData')
  # Importation de la table des carreaux et filtrage des observations selon le(s) code(s) commune(s)
  load('data/filosofi/carreauxFiLoSoFi.RData')
  carreaux <- carreauxFiLoSoFi |>
    dplyr::filter(Depcom %in% listeCodesCommune)

  # Liste des identifiants Inspire des carreaux, à partir desquels on récupère leurs coordonnées, taille et code EPSG
  cIdInspire <- as.character(carreaux$IdINSPIRE)

  epsg <- as.integer(stringr::str_sub(stringr::str_extract(carreaux[1,]$IdINSPIRE, "CRS\\d+"), 4))

  tailleCarreaux <- unlist(lapply(X = cIdInspire, FUN = function(ligne){
    return(as.integer(stringr::str_sub(stringr::str_extract(ligne, "RES\\d+"), 4)))
  }))
  ordonneesCarreaux <- unlist(lapply(X = cIdInspire, FUN = function(ligne){
    return(as.integer(stringr::str_sub(stringr::str_extract(ligne, "N\\d+"), 2)))
  }))
  abscissesCarreaux <- unlist(lapply(X = cIdInspire, FUN = function(ligne){
    return(as.integer(stringr::str_sub(stringr::str_extract(ligne, "E\\d+"), 2)))
  }))

  # Ajout de 2 colonnes donnant les coordonnées du coin inférieur gauche du carreau
  carreaux$x <- abscissesCarreaux
  carreaux$y <- ordonneesCarreaux

  # Création d'une colonne geometry contenant les coordonnées des contours des carreaux
  # puis transformation en objets geométriques à l'aide du package sf
  carreaux$geometry <- sprintf("POLYGON ((%i %i, %i %i, %i %i, %i %i, %i %i))",
                               carreaux$x, carreaux$y,
                               carreaux$x + tailleCarreaux, carreaux$y,
                               carreaux$x + tailleCarreaux, carreaux$y + tailleCarreaux,
                               carreaux$x, carreaux$y + tailleCarreaux,
                               carreaux$x, carreaux$y)

  carreauxSf <- sf::st_as_sf(carreaux, wkt = "geometry", crs = epsg)

  # Contours des zones sur lesquelles on souhaite connaître les agrégats
  #zones <- sf::st_read(cheminFichierContoursSHP) |>
  zones <- fichierContoursSHP |>
    sf::st_transform(epsg)

  # Intersection des carreaux avec les zones
  agregatsZones <- carreauxSf |>
    sf::st_join(zones, join = sf::st_intersects, left = FALSE)

  # OR intersection des carreaux avec les zones puis calcul des agrégats
  # agregatsZones <- agregatsZones %>%
  #   sf::st_set_geometry(NULL) %>%
  #   dplyr::mutate(popImp = I_est_cr*Ind) %>%
  #   dplyr::select(-IdINSPIRE, -Depcom, -I_est_cr, -x, -y) %>%
  #   dplyr::group_by_at(setdiff(colnames(zones), "geometry")) %>%
  #   dplyr::summarise_all(sum) %>%
  #   dplyr::mutate(txPopImp = round(100*popImp/Ind, digits = 1)) %>%
  #   dplyr::select(-popImp) %>%
  #   dplyr::ungroup()
  #

  return(dplyr::distinct(agregatsZones, IdINSPIRE, .keep_all = TRUE))
}

#' sur Orée d'Anjou
#' Carroyage sur 200m de la consommation d'espaces
#'
#' @return une carte tmap
#'
#' @import data.table dplyr sf tidyr echarts4r
#'
carto_conso_espace_oree_anjou <- function(){

  # Limites administratives Communes de La Varenne, Champtoceaux etc.. sur la CC Orée d'Anjou
  #ComAssoDelLimitOreeAnjou2024 <- getOrAnjouCommune
  #cheminFichierContoursSHP <- 'data/contours_administratifs_france/ign_admin_express/2024/ADMIN-EXPRESS_3-2__SHP_LAMB93_FXX_2024-03-25/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2024-03-00237/ADE_3-2_SHP_LAMB93_FXX-ED2024-03-25/COMMUNE_ASSOCIEE_OU_DELEGUEE.shp'
  #cheminFichierContoursSHP <- 'data/mesures_artificialisation_commune/src/2024/obs_artif_conso_com_2009_2022_carroyage_LEA/obs_artif_conso_com_2009_2022_carroyage_LEA.shp'
  #obs_artif_conso_com_2009_2022_carroyage_LEA <- sf::st_read(cheminFichierContoursSHP)
  #save(obs_artif_conso_com_2009_2022_carroyage_LEA, file='data/mesures_artificialisation_commune/obs_artif_conso_com_2009_2022_carroyage_LEA.RData')
  #load('data/mesures_artificialisation_commune/obs_artif_conso_com_2009_2022_carroyage_LEA.RData')

  # Conso espace sur Orée d'Anjou
  #cheminFichierContoursSHP <- 'data/mesures_artificialisation_commune/2024/obs_artif_conso_com_2009_2022/obs_artif_conso_com_2009_2022.shp'
  #artiFr2024 <- sf::st_read(dsn = 'data/mesures_artificialisation_commune/2024/obs_artif_conso_com_2009_2022/obs_artif_conso_com_2009_2022.shp')

  #carreaux_oree_anjou <- calculAgregatsZones( obs_artif_conso_com_2009_2022_carroyage_LEA, listeCodesCommune = c("49069")) |>
  #  tidyr::drop_na(NAF09ART22)
  #save(carreaux_oree_anjou, file='data/mesures_artificialisation_commune/carreaux_oree_anjou.RData')
  load('data/mesures_artificialisation_commune/carreaux_oree_anjou.RData')
  heat_map_oree_anjou <- heatmap(
    carreaux_oree_anjou,
    label="Consommation espace en m²\nentre 2009 et 2022",
    heat_field="NAF09ART22"
  )

  return(heat_map_oree_anjou)
}


#' sur l'ECPI ou la commune sélectionnée entre 2009 et 2024
#' Consommation d'espaces :
#' Conversion par 6 types d'activité en hectares avec 2 décimales
#' de surface non artificialisée en surface artificialisée
#' soit un histogrammes avec répartition de part d'activité dans chacunes des colonnes
#'
#' @return un histogramme
#'
#' @import data.table dplyr sf tidyr echarts4r
#'
histo_conso_espace_oree_anjou <- function(){

  arti49069_2009_2022_echart <- g_arti_fr_2009_2022_no_geom |>
    dplyr::filter(grepl(49069, IDCOM)) |>
    echarts4r::e_charts(year) |>
    echarts4r::e_title("Évolution annuelle de la consommation d'espace\n sur Orée d'Anjou entre 2009 et 2021", bottom='5') |>
    echarts4r::e_bar(activite, stack = "grp") |>
    echarts4r::e_bar(habitat, stack = "grp") |>
    echarts4r::e_bar(mixte, stack = "grp") |>
    echarts4r::e_bar(route, stack = "grp") |>
    echarts4r::e_bar(chemin_fer, stack = "grp") |>
    echarts4r::e_bar(autres, stack = "grp") |>
    echarts4r::e_y_axis(name="m² consommés")# |>
    #echarts4r::e_theme("chalk") #walden, westeros, light, dark...
  # TODO to test
  # echarts4r::e_tooltip(formatter = htmlwidgets::JS("
  #  function(params) {
  #    return params.seriesName + '<br/>' + params.name + ' : ' + params.value.toFixed(2) + ' ha';
  #  }
  # "))
  #https://echarts.apache.org/en/option.html#legend.selectedMode
  #echarts4r::e_legend(type=c('plain', "scroll"),selected_mode = "single")
  #echarts4r::e_x_axis("Année") |>
  #echarts4r::e_histogram(stack = TRUE, color = Activite, legend = TRUE)
  #echarts4r::e_histogram(stack = TRUE, series = c("activite", "habitat", "mixte", "route", "chemin_fer", "autres"), color = Activite, legend = TRUE)

  return(arti49069_2009_2022_echart)
}

#' Evolution de la consommation d'espaces sur la commune déléguée de La Varenne (49270)
#'
#' @return un histogramme
#'
#' @import dplyr echarts4r
#'
histo_conso_espace_varenne <- function(){
  #   # Conso espace sur La Varenne
  #   artiCarreaux49069_2009_2022 <- sf::st_read(dsn = 'data/mesures_artificialisation_commune/2024/obs_artif_conso_com_2009_2022_carroyage_LEA/obs_artif_conso_com_2009_2022_carroyage_LEA.shp')
  #   #  join_with getOrAnjouCommune()|>
  #   #  group_by(code_commune)
  # })
}

#' Evolution de la consommation d'espaces par région en France avec
#' - le libellé de la région
#' - le nombre de m² perdu entre l’année n-1 et l’année n
#' TODO le nombre de m² perdu cumulé entre 2009 et l’année n.
#' Dans quelle région cela semble s’améliorer ? Dans quelle région cela semble s’aggraver ?
#'
#' @return un graphique en courbe
#'
#' @import dplyr echarts4r
#'
histo_conso_espace_france <- function() {

  artif_by_reg_0922 <- g_arti_fr_2009_2022_no_geom |>
    #dplyr::filter(IDREG %in% c(84)) |>
    dplyr::select(year, IDREG, IDREGTXT, totalArti) |>
    dplyr::group_by(IDREG, IDREGTXT, year) |>
    dplyr::summarise(totalArtiReg = sum(totalArti, na.rm = T)) |>
    dplyr::arrange(year, IDREGTXT)

  plot <- artif_by_reg_0922 |>
    dplyr::group_by(IDREGTXT) |>
    echarts4r::e_charts(year) |>
    echarts4r::e_line(totalArtiReg) |>
    echarts4r::e_title("Consommation d'espaces en m²", bottom='15')

  return(plot)
}

#' Carte de la consommation d'espaces par département en France avec
#' Pour chaque département :
#'   - le total de la surface artificialisée
#'   - la part de la surface totale qui a été artificialisée entre 2009 et 2022
#'   - la part de la surface artificialisée lié à l’activité et à l’habitat
#' Chargement des Indicateurs de consommation d'espace en 2022
#' TODO traiter les 5 lignes ci-dessus
#'
#' @return une carte
#'
#' @import dplyr forcats echarts4r RPostgres
#'
carto_conso_espace_dept_fr <- function(){

  # dept_fr <- getFrDept()

  # artif_by_fr_dept <- g_arti_fr_2009_2022_no_geom |>
  #   dplyr::group_by(IDDEPTXT, IDDEP) |>
  #   dplyr::summarise(
  #     # surface_totale_artificialisee
  #     totalArti = sum(totalArti, na.rm = T),
  #     # surface_totale_commune
  #     SURFCOM202 = sum(SURFCOM202, na.rm = T),
  #     # surface_totale_artificialisee_activite
  #     ART09ACT22 = sum(ART09ACT22, na.rm = T),
  #     # surface_totale_artificialisee_habitat
  #     ART09HAB22 = sum(ART09HAB22, na.rm = T)
  #   ) |>
  #   dplyr::mutate(
  #     # part_surface_totale_artificialisee = ((surface_totale_artificialisee / surface_totale_commune)*100),
  #     part_totalArti = totalArti * 100 / SURFCOM202,
  #     # part_surface_totale_artificialisee_act = (((surface_totale_artificialisee_activite)/surface_totale_artificialisee)*100)
  #     part_ART09ACT22 = ART09ACT22 * 100 / totalArti,
  #     # part_surface_totale_artificialisee_hab_act = (((surface_totale_artificialisee_activite+surface_totale_artificialisee_habitat)/surface_totale_artificialisee)*100),
  #     part_ART09HAB22 = ART09HAB22 * 100 / totalArti
  #   ) |>
  #   dplyr::ungroup() |>
  #   dplyr::left_join(dplyr::select(dept_fr, geometry, INSEE_DEP), by = c("IDDEP" = "INSEE_DEP"))

  # Porter le libellé du département en une variable factorielle triée par la part de sa surface perdue.
  # artif_by_fr_dept <- artif_by_fr_dept |>
  #   dplyr::arrange(-totalArti) |>
  #   dplyr::mutate(IDDEPTXT = forcats::fct_inorder(IDDEPTXT))

  # artif_by_fr_dept$surface_km2 <- artif_by_fr_dept$totalArti / 1e6
  # save(artif_by_fr_dept, file='data/mesures_artificialisation_commune/artif_by_fr_dept.RData')
load('data/mesures_artificialisation_commune/artif_by_fr_dept.RData')
  #####################################################################"
  # todo test conversion rdata to sql lite
  # https://cran.r-project.org/web/packages/RSQLite/vignettes/RSQLite.html
  # https://github.com/r-dbi/RSQLite/issues/269
  # https://rdrr.io/cran/RSQLite/man/SQLite.html"
  #library(DBI)
  #install.packages("RPostgres")
  #library(RPostgres)
# conn <- RPostgres::dbConnect(RPostgres::Postgres()
#                  , host='xxx'
#                  , port='xxx'
#                  , dbname='xxx'
#                  , user='xxx'
#                  , password='xxxx')
  #sf::st_write(artif_by_fr_dept, dsn = conn, delete_layer = TRUE)

  # install.packages("RSQLite")
  # library(RSQLite)
  # conn <- dbConnect(RSQLite::SQLite(), "artif.sqlite")
  # conn.enable_load_extension(True)
  # cur = conn.cursor()
  # conn.execute("PRAGMA foreign_keys = ON")
  # conn.execute("SELECT load_extension('mod_spatialite')") -- add this line
  # cur.execute("SELECT InitSpatialMetaData(1);")

  #RPostgres::dbWriteTable(conn, "artif_by_fr_dept", artif_by_fr_dept, overwrite = TRUE, append = FALSE)
# artif_by_fr_dept <- sf::st_read(conn, layer = "sf") |>
#   lwgeom::st_snap_to_grid(700)
  #artif_by_fr_dept_pg <- RPostgres::dbReadTable(conn, "artif_by_fr_dept")
  ####################################################################

  map <- heatmap(
    artif_by_fr_dept,
    label="Surface (km²)",
    label_field="surface_km2",
    heat_field="surface_km2",
    snap=TRUE)

  return(map)
}

# Quel département a le plus perdu de m² ?
kpi_max_conso_espace_dept_fr <- function(){
  if(!exists("artif_by_fr_dept")) {
    load('data/mesures_artificialisation_commune/artif_by_fr_dept.RData')
  }
  value <- dplyr::filter(artif_by_fr_dept, totalArti == max(totalArti)) |>
    dplyr::pull(IDDEPTXT)
  return(value)
}

# Quel département a perdu la plus grande part de son territoire ?
kpi_max_percent_conso_espace_dept_fr <- function(){
  if(!exists("artif_by_fr_dept")) {
    load('data/mesures_artificialisation_commune/artif_by_fr_dept.RData')
  }
  value <- dplyr::filter(artif_by_fr_dept, part_totalArti == max(part_totalArti)) |>
    dplyr::pull(IDDEPTXT)
  return(value)
}

# Quel département artificialise le plus pour l’activité ?
kpi_first_conso_espace_dept_fr <- function(){
  if(!exists("artif_by_fr_dept")) {
    load('data/mesures_artificialisation_commune/artif_by_fr_dept.RData')
  }
  value <- dplyr::filter(artif_by_fr_dept, part_ART09ACT22 == max(part_ART09ACT22)) |>
    dplyr::pull(IDDEPTXT)
  return(value)
}

# Pour chaque région :
#   - le total de la surface artificialisée
#   - la part de la surface totale qui a été artificialisée entre 2009 et 2022
#   - la part de la surface artificialisée lié à l’activité et à l’habitat
# Chargement des Indicateurs de consommation d'espace en 2022
carto_conso_espace_reg_fr <- function(){

  # regions_fr <- getFrRegion()
  #
  # artif_by_fr_reg <- g_arti_fr_2009_2022_no_geom |>
  #   dplyr::group_by(IDREGTXT, IDREG) |>
  #   dplyr::summarise(
  #     # surface_totale_artificialisee
  #     totalArti = sum(totalArti, na.rm = T),
  #     # surface_totale_commune
  #     SURFCOM202 = sum(SURFCOM202, na.rm = T),
  #     # surface_totale_artificialisee_activite
  #     ART09ACT22 = sum(ART09ACT22, na.rm = T),
  #     # surface_totale_artificialisee_habitat
  #     ART09HAB22 = sum(ART09HAB22, na.rm = T)
  #   ) |>
  #   dplyr::mutate(
  #     # part_surface_totale_artificialisee = ((surface_totale_artificialisee / surface_totale_commune)*100),
  #     part_totalArti = totalArti * 100 / SURFCOM202,
  #     # part_surface_totale_artificialisee_act = (((surface_totale_artificialisee_activite)/surface_totale_artificialisee)*100)
  #     part_ART09ACT22 = ART09ACT22 * 100 / totalArti,
  #     # part_surface_totale_artificialisee_hab_act = (((surface_totale_artificialisee_activite+surface_totale_artificialisee_habitat)/surface_totale_artificialisee)*100),
  #     part_ART09HAB22 = ART09HAB22 * 100 / totalArti
  #   ) |>
  #   dplyr::ungroup() |>
  #   dplyr::left_join(dplyr::select(regions_fr, geometry, INSEE_REG), by = c("IDREG" = "INSEE_REG"))

  # Porter le libellé de la région en une variable factorielle triée par la part de sa surface perdue.
  # artif_by_fr_reg <- artif_by_fr_reg |>
  #   dplyr::arrange(-totalArti) |>
  #   dplyr::mutate(IDREGTXT = forcats::fct_inorder(IDREGTXT))

  #artif_by_fr_reg$surface_km2 <- artif_by_fr_reg$totalArti / 1e6
  #artif_by_fr_reg <- artif_par_reg
  #save(artif_by_fr_reg, file='data/mesures_artificialisation_commune/artif_by_fr_reg.RData')
  load('data/mesures_artificialisation_commune/artif_by_fr_reg.RData')

  map <- heatmap(
    artif_by_fr_reg,
    label="Surface (km²)",
    label_field="surface_km2",
    heat_field="surface_km2",
    snap=TRUE)

  return(map)
}

# Quelle région a le plus perdu de m² ?
# dplyr::filter(artif_by_fr_reg, totalArti == max(totalArti)) |> dplyr::pull(IDREGTXT)

# Quelle région a perdu la plus grande part de son territoire ?
# dplyr::filter(artif_by_fr_reg, part_totalArti == max(part_totalArti)) |> dplyr::pull(IDREGTXT)

# Quelle région artificialise le plus pour l’activité ?
# dplyr::filter(artif_by_fr_reg, part_ART09ACT22 == max(part_ART09ACT22)) |> dplyr::pull(IDREGTXT)

# Total de la surface artificialisée en France entre 2009 et 2022
totalSurfaceArti <- function(){

  total_arti_fr_m2 <- dplyr::summarise(g_arti_fr_2009_2022_no_geom, totalArti = sum(totalArti, na.rm = T))[1,1]
  total_arti_fr_km2 <- prettyNum(total_arti_fr_m2 / 1e6, big.mark = " ", decimal.mark = ",")
  total_arti_fr_km2_f <- sprintf("%s km²", total_arti_fr_km2)
  #cat("Surface :", total_arti_fr_km2_f, "km2")

  return(total_arti_fr_km2_f)
}

data_conso_espace_france <- function(){

  data <- datatable(g_arti_fr_2009_2022_no_geom,
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

  return(data)
}
