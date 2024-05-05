#' heatmap UI Function
#'
#' @description helper to display heatmap with tmap.
#'
#' @param df Dataframe
#' @param label Character
#' @param label_field Character
#' @param heat_field Character
#'
#' @noRd
#'
#' @import tmap sf
heatmap <- function(df, label, label_field=NA, heat_field, basemap="OpenStreetMap", snap=FALSE) {

  ##########################################################################################
  # TO TEST
  # load('data/mesures_artificialisation_commune/obs_artif_conso_com_2009_2022_carroyage_LEA.RData')
  # df <- calculAgregatsZones( obs_artif_conso_com_2009_2022_carroyage_LEA, listeCodesCommune = c("49069")) |>
  #   tidyr::drop_na(NAF09ART22)
  # label="Consommation espace en m²\nentre 2009 et 2022"
  # label_field="Depcom"
  # heat_field="NAF09ART22"
  # basemap="OpenStreetMap"
  ###########################################################################################

  # TODO library call only required to charge World data, how to remove this line ?
  library(tmap)
  data("World")

  tmap::tmap_options(check.and.fix = TRUE)
  tmap::tmap_mode("view")

  df <- sf::st_as_sf(df)

  # for admin_express in particular whose geometry fineness is very great
  # in order to optimize the map display times with tmap.
  # Tests with postgis did not improve performance either.
  # To dig
  if(isTRUE(snap)){
    df <- df |>
      lwgeom::st_snap_to_grid(700)
  }

  map <- tm_basemap(basemap) +
    tmap::tm_shape(df) +
    tmap::tm_borders(alpha = .5) +
    tmap::tm_fill(heat_field, palette = "YlOrRd", title = label) +
    tmap::tm_scale_bar(position = c("left", "bottom")) +
    tmap::tm_legend(outside = FALSE)

    if (!is.na(label_field)) {
      map +
        tmap::tm_text(label_field, size = 0.5)
    }
  map

  return(map)
}

#' mymap UI Function
#'
#' @description Helper to quickly explore geo data in map
#' https://www.datanovia.com/en/fr/blog/top-palettes-de-couleurs-r-a-connaitre-pour-une-meilleur-visualisation-des-donnees/
#' https://colorbrewer2.org/#type=sequential&scheme=BuGn&n=6
#'
#' @param df Dataframe
#' @param label Character
#' @param label_field Character
#' @param heat_field Character
#'
#' @noRd
#'
#' @import mapview RColorBrewer
#'
mymap <- function(df, label, label_field, heat_field){
  nb_colors <- length(unique(df$heat_field))
  pal <- RColorBrewer::brewer.pal(nb_colors, 'Blues')
  #pal <- magma(n = length(unique(ComAssoDelLimitOreeAnjou2024$POPULATION)), direction = -1)

  #analyse_df(df, IdINSPIRE)
  #col2sum <- grep("regexp", names(df), value = TRUE)
  #df$sum <- rowSums(df[, col2sum], na.rm = TRUE)
  #df$sum <- rowSums(df[, c('NAF09ART22')], na.rm = TRUE)

  mapview::mapview(df, label = label_field, zcol=heat_field, layer.name=label, col.regions = pal)
}

#' generate_map_slideshow UI Function
#'
#' @description Helper to test map slideshow
#'
#' @param df Dataframe
#'
#' @noRd
#'
#' @import tmap dplyr
#'
generate_map_slideshow <- function(df) {
  years <- unique(df$year)
  n_years <- length(years)

  for (i in 1:n_years) {
    year_data <- dplyr::filter(df, year == years[i])

    map <- tmap::tm_shape(world) +
      tmap::tm_borders() +
      tmap::tm_fill(col = "grey80") +
      tmap::tm_shape(year_data) +
      tmap::tm_bubbles(size = "value", col = "red", border.col = "black") +
      tmap::tm_layout(title = paste("Year", years[i]))

    print(map)
    Sys.sleep(5) # 5 seconds break
  }
}
