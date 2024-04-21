#' echarts_earth UI Function
#'
#' @description Helper to test 3D echarts
#'
#' @noRd
#'
#' @import echarts4r
# @importFrom echarts4r assets
#'
echarts_earth <- function(){
  remotes::install_github('JohnCoene/echarts4r.assets')
  library(echarts4r)
  map <- flights |>
    echarts4r::e_charts() |>
    echarts4r::e_globe(
      environment = echarts4r.assets::ea_asset("starfield"),
      base_texture = echarts4r.assets::ea_asset("world topo"),
      height_texture = echarts4r.assets::ea_asset("world topo"),
      displacementScale = 0.05
    ) |>
    echarts4r::e_lines_3d(
      start_lon,
      start_lat,
      end_lon,
      end_lat,
      name = "flights",
      effect = list(show = TRUE)
    ) |>
    echarts4r::e_legend(FALSE)

  return(map)
}

#' tmap example UI Function
#'
#' @description Helper to test 3D echarts
#' https://maeltheuliere.github.io/rspatial/creer-des-cartes-avec-tmap.html
#' https://gganimate.com/index.html
#' https://rstudio.github.io/leaflet/
#'
#' @noRd
#'
#' @import sf paletteer tmap
#'
#if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")};library(tidyverse)
#if("patchwork" %in% rownames(installed.packages()) == FALSE) {install.packages("patchwork")};library(patchwork)
#if("lwgeom" %in% rownames(installed.packages()) == FALSE) {install.packages("lwgeom")};library(lwgeom)
#library(ggspatial)
#remotes::install_github("MaelTheuliere/variousdata")
#library(variousdata)
tmap_example <- function(){
  wgs_84 <- tmap::tm_shape(World, projection = "longlat") +
    tmap::tm_polygons() +
    tmap::tm_layout("Le monde en projection WGS84", inner.margins=c(0,0,.1,0), title.size=.8)

  tmap::tm_shape(World) +
    tmap::tm_polygons(c("HPI", "economy")) +
    tmap::tm_facets(sync = TRUE, ncol = 2)

  #if("shinyjs" %in% rownames(installed.packages()) == FALSE) {install.packages("shinyjs")};library(shinyjs)
  tmaptools::palette_explorer()

  #if("paletteer" %in% rownames(installed.packages()) == FALSE) {install.packages("paletteer")};library(paletteer)
  paletteer::paletteer_c("scico::berlin", n = 10)

  paletteer::paletteer_c("nord:", n = 10)

  urb_anim = tmap::tm_shape(world) + tmap::tm_polygons() +
    tmap::tm_shape(urban_agglomerations) + tmap::tm_dots(size = "population_millions") +
    tmap::tm_facets(along = "year", free.coords = FALSE)

  tmap::tmap_animation(urb_anim, filename = "urb_anim.gif", delay = 25)
}

### MAPVIEW Example
#if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse", source='https://cran.rstudio.com/')};library(tidyverse)
#if("sf" %in% rownames(installed.packages()) == FALSE) {install.packages("sf")};library(sf)
# Pour mapview
#if("leaflet.providers" %in% rownames(installed.packages()) == FALSE) {install.packages('leaflet.providers', source='https://cran.rstudio.com/')};
#if("mapview" %in% rownames(installed.packages()) == FALSE) {install.packages('mapview', source='https://cran.rstudio.com/')};library(mapview)
mapview_example <- function(){
  nantes <- data.frame(lon=-1.553621,lat=47.218371) |>
    st_as_sf(coords = c("lon", "lat")) |>
    st_set_crs(4326)
  nantes_sf <- st_as_sf(nantes, sf_column_name = "geometry")
  # Point indiquant l'hôtel de ville sur openstreetmap
  mapview(nantes_sf)
}

# LEAFLET example
#if("leaflet" %in% rownames(installed.packages()) == FALSE) {install.packages("leaflet")};library(leaflet)
#if("sf" %in% rownames(installed.packages()) == FALSE) {install.packages("sf")};library(sf)
#if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")};library(tidyverse)
#if("tmap" %in% rownames(installed.packages()) == FALSE) {install.packages("tmap")};library(tmap)
# remotes::install_github("MaelTheuliere/variousdata")
# library(variousdata)
#
# library(ggplot2)
# if("patchwork" %in% rownames(installed.packages()) == FALSE) {install.packages("patchwork")};library(patchwork)
# if("lwgeom" %in% rownames(installed.packages()) == FALSE) {install.packages("lwgeom")};library(lwgeom)
# library(variousdata)
# library(ggspatial)
leaflet_example <- function(){
  data("World")
  sdg_indicators_sf <- World |>
    left_join(sdg_indicators)

  # 12.1.1 Carte choroplète (aplats de couleur)
  sdg_indicators_2015_sf <- sdg_indicators_sf |>
    filter(timeperiod ==2015)

  sdg_indicators_2015_sf <- sdg_indicators_sf |>
    filter(timeperiod ==2015)

  bins <-quantile(sdg_indicators_2015_sf$sh_sta_mmr,
                  na.rm=T)

  pal <- colorBin("YlOrRd", domain =
                    sdg_indicators_2015_sf$sh_sta_mmr,
                  bins = bins)

  labels <- sprintf(
    "<strong>%s</strong><br/>%g décès pour 100 000 naissance en 2015",
    sdg_indicators_2015_sf$geoareaname, sdg_indicators_2015_sf$sh_sta_mmr
  ) |> lapply(htmltools::HTML)

  leaflet(sdg_indicators_2015_sf) |>
    addProviderTiles(providers$CartoDB.Positron) |>
    addPolygons(data=sdg_indicators_2015_sf,
                fillColor=~pal(sh_sta_mmr),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"))
}

### GGPLOT example
# https://maeltheuliere.github.io/rspatial/creer-des-cartes-avec-ggplot2.html
# library(sf)
# if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")};library(tidyverse)
# library(ggplot2)
# if("patchwork" %in% rownames(installed.packages()) == FALSE) {install.packages("patchwork")};library(patchwork)
# if("lwgeom" %in% rownames(installed.packages()) == FALSE) {install.packages("lwgeom")};library(lwgeom)
# library(variousdata)
# library(ggspatial)
# remotes::install_github("MaelTheuliere/variousdata")
# library(variousdata)
ggplot_example <- function(){
  sdg_indicators |>
    filter(timeperiod  %in% c(2000, 2005, 2010, 2015),
           geoareaname %in% c("France","Canada","Burkina Faso","China","Australia")) |>
    ggplot() +
    geom_bar(aes(x = geoareaname, weight = sh_sta_mmr, fill = continent)) +
    theme_minimal() +
    scale_fill_viridis_d() +
    coord_flip() +
    scale_y_log10()+
    labs(
      title = "Mortalité maternelle sur quelques pays",
      subtitle = "En 2015",
      y = "Taux de mortalité de la mère \n(pour 100 000 naissances), échelle logarithmique",
      x = "Pays",
      fill = "Pays"
    ) +
    theme(legend.position = "none") +
    facet_wrap(~timeperiod)

  p <- sdg_indicators |>
    filter(timeperiod  %in% c(2000, 2005, 2010, 2015),
           geoareaname %in% c("France","Canada","Burkina Faso","China","Australia")) |>
    ggplot() +
    geom_bar(aes(x = geoareaname, weight = sh_sta_mmr, fill = continent)) +
    theme_minimal() +
    scale_fill_viridis_d() +
    coord_flip() +
    scale_y_log10()+
    labs(
      title = "Mortalité maternelle sur quelques pays",
      subtitle = "En 2015",
      y = "Taux de mortalité de la mère \n(pour 100 000 naissances), échelle logarithmique",
      x = "Pays",
      fill = "Pays"
    ) +
    theme(legend.position = "none") +
    facet_wrap(~timeperiod, ncol = 4)

  ggsave("figures/Mortalité maternelle sur quelques pays du globe.svg", p, width = 12, height = 5)
  #save.image('data/test.RData')
  #save(iris, file = 'data/test.RData')

  data("World")

  sdg_indicators_sf <- World |>
    left_join(sdg_indicators)
}

### Ensemble de données dans divers format à titre pédagogique pour l'enseignement de l'analyse spatiale
# https://rdrr.io/cran/spData/
# ici utilisation de NY_leukemia.gpkg
# library(sf)
# library(spData)
spdata_example <- function(){
  geo_fichier <- system.file("data/teaching_spdata/in/NY8_bna_utm18.gpkg",
                             package = "spData")
  NY_leukemia <- st_read(dsn = geo_fichier)
  st_write(obj = NY_leukemia, dsn = "data/teaching_spdata/out/NY_leukemia.gpkg")
}

# STICKY NOTE to explore dataframe
explore_df <- function(df, pkey){

  print('explore df:')

  # Save multiple objects
  #save(data1, data2, file = "data.RData")
  # load("data.RData")

  # df <- data.frame(
  #   year = as.character(2009:2024),
  #   field = runif(16, 0, 100)
  # )

  # Ajout de données
  # for (i in 2:ncol(df)) {
  #   p <- p |> e_add_series(
  #     df[,1],
  #     df[,i],
  #     name = colnames(df)[i]
  #   )
  # }

  # df |>
  #   dplyr::filter(FIELD == "xxx")
  #   dplyr::filter(grepl(xxx, FIELD))
  #   dplyr::arrange(desc(FIELD))
  #   dplyr::group_by_at("year")
  #   dplyr::summarise_all("field")
  #   tibble::rownames_to_column("year") # TODO test
  #   df$year <- as.Date(as.character(df$year), "%Y")
  #   df$year <- as.Date(paste0(local_artif_0924$year, "-01-01"))
  #   tidyr::pivot_longer(cols = -year, names_to = "naf0910", values_to = "Valeur")

  # Vérification des doublons
  length(df$pkey)
  length(unique(df$pkey))

  # nb colonnes
  length(colnames(df))

  # Vérification Types de champs
  str(df)

  # Liste colonnes
  colnames(df)

  # Vérification valeurs nulles
  summary(df)
}

loadCsvFile <- function(pathToCsv){
  df <- datatable(read.csv(
    pathToCsv,
    sep = ";",
    fileEncoding = "UTF-8"
  ))
  return(df)
}
