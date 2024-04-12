#' plot_heatmap UI Function
#'
#' @description A shiny Util to display heatmap with tmap.
#'
#' @param df Dataframe
#'
#' @noRd
#'
#' @import tmap sf
plot_heatmap <- function(df) {

  library(tmap)
  #library(sf)

  # dataframe to spatialframe convert
  df <- st_as_sf(df)

  # surface in km2
  df$surface_km2 <- df$nafart0917 / 1e6

  tmap_options(check.and.fix = TRUE)

  data("World")
  map <- tm_shape(World) +
    #tm_borders() +
    tm_polygons("grey", alpha = 0.5) #+
    #tm_text("region", size = 0.7) +
    #tm_scale_bar(position = c("left", "bottom"))

  # Ajouter la couche de chaleur
  # style = "heat",
  map <- tm_shape(df) +
    tm_borders() +
    tm_fill("surface_km2", palette = "YlOrRd", title = "Surface (km²)") +
    tm_text("idregtxt", size = 0.5) +
    tm_scale_bar(position = c("left", "bottom")) +
    tm_legend(outside = FALSE)

  return(map)
  #print(map)
}
#plot_heatmap(st_as_sf(artif_par_reg))

#library(tmap)
#library(tidyverse)
generate_map_slideshow <- function(df) {
  years <- unique(df$year)
  n_years <- length(years)

  for (i in 1:n_years) {
    year_data <- filter(df, year == years[i])

    tm_shape(world) +
      tm_borders() +
      tm_fill(col = "grey80") +
      tm_shape(year_data) +
      tm_bubbles(size = "value", col = "red", border.col = "black") +
      tm_layout(title = paste("Year", years[i])) -> map

    print(map)
    Sys.sleep(5) # 5 seconds break
  }
}

echarts_earth <- function(){
  library(echarts4r)
  # remotes::install_github('JohnCoene/echarts4r.assets')
  library(echarts4r.assets)
  flights %>%
    e_charts() %>%
    e_globe(
      environment = ea_asset("starfield"),
      base_texture = ea_asset("world topo"),
      height_texture = ea_asset("world topo"),
      displacementScale = 0.05
    ) %>%
    e_lines_3d(
      start_lon,
      start_lat,
      end_lon,
      end_lat,
      name = "flights",
      effect = list(show = TRUE)
    ) %>%
    e_legend(FALSE)
}

### Données sirene 44
# Système national d'identification et du répertoire des entreprises et de leurs établissements
# https://www.data.gouv.fr/fr/datasets/base-sirene-des-entreprises-et-de-leurs-etablissements-siren-siret/
## @importFrom withr with_package
#library(tidyverse)
sirene <- function(){
  # with_package(
  #   "ggplot2",
  #   r <- ls("package:ggplot2", pattern = "^theme_")
  # )
  load("data/sirene44_2019_03/sirene_zones_commerciales_44.RData")
  sirene44_sel <- sirene44 %>%
    filter(APET700 == "0893Z")

  mapview(list(departement_44, epci_d44, sirene44_sel), zcol = c("NOM_DEP", "NOM_EPCI", "NOMEN_LONG"), legend = F)
  sirene44_sel_avec_code_epci <- sirene44_sel %>%
    st_join(epci_geo)
  mapview(list(departement_44, epci_d44, sirene44_sel_avec_code_epci), zcol = c("NOM_DEP", "NOM_EPCI", "NOM_EPCI"), legend = F)
}

# Découpage administratif du territoire français (commune, arrondissement départemental, département, région...).
# https://geoservices.ign.fr/adminexpress
# Chargement des couches REGION_CARTO et EPCI_CARTO dans data/contours_administratif_france/adminexpress
# Trouver les EPCI de la région pays de la loire à cheval sur d'autres régions

# Chargement des packages -------------------------------------------------
library(sf)
#library(tidyverse)
library(mapview)
adminexpress <- function(){
  # Chargement des données --------------------------------------------------
  reg <- st_read(dsn = 'data/contours_administratifs_france/ign_admin_express/REGION_CARTO.shp')
  mapview(reg)
  epci <- st_read(dsn = 'data/contours_administratifs_france/ign_admin_express/EPCI_CARTO.shp')

  # Data preparation --------------------------------------------------------
  reg_pdl <- reg %>%
    filter(INSEE_REG=='52') %>%
    st_buffer(dist=-3000)

  reg_limitrophes_pdl <- reg %>%
    filter(INSEE_REG %in% c('53','28','24','75')) %>%
    st_buffer(dist=-3000)

  # Version avec st_intersect ----
  epci_pdl <- epci[reg_pdl,,op=st_intersects]
  mapview(epci_pdl)

  epci_pdl_plusieurs_regions <- epci_pdl[reg_limitrophes_pdl,,op=st_intersects]
  mapview(epci_pdl_plusieurs_regions)

  # Version avec st_overlaps ----
  epci_pdl_plusieurs_regions_avec_overlaps <- epci[reg %>%
                                                     filter(INSEE_REG=='52'),,
                                                   op=st_overlaps]
  # Visualisation -----------------------------------------------------------
  mapview(epci_pdl_plusieurs_regions_avec_overlaps)
  mapview(list(epci_pdl_plusieurs_regions,reg_pdl))
}

### Carte des départements et des régions à partir d'admin express
#library(sf)
#library(tidyverse)
#library(mapview)
#library(ggplot2)
adminexpress2 <- function(){

  load("data/admin_express.RData")

  class(departements_geo)

  mapview(departements_geo, zcol = "NOM_DEP", legend = F)

  departements_geo52 <- departements_geo %>%
    filter(INSEE_REG == 52)

  mapview(departements_geo52, zcol = "NOM_DEP", legend = T)

  regions <- departements_geo %>%
    group_by(INSEE_REG) %>%
    summarise(AREA = sum(AREA))

  glimpse(regions)
  mapview(regions, zcol = "INSEE_REG", legend = F)

  regions <- regions %>%
    left_join(regions_geo %>%
                st_drop_geometry(),
              by = c("INSEE_REG")
    )

  mapview(regions, zcol = "NOM_REG", legend = F)
  class(regions)

  departement_44 <- departements_geo %>%
    filter(INSEE_DEP == "44")

  epci_d44 <- epci_geo[departement_44, , op = st_within]

  mapview(list(departement_44, epci_d44), zcol = c("NOM_DEP", "NOM_EPCI"), legend = F)

  glimpse(epci_geo)
  glimpse(regions_geo)

  epci_d44_buffer <- epci_geo[departement_44_buffer, , op = st_within]
  departement_44_buffer <- departement_44 %>%
    st_buffer(dist = 5000)
  mapview(list(departement_44_buffer, departement_44), layer.name = c("Loire-Atlantique avec un buffer de 5 km", "Loire-Atlantique"), zcol = c("NOM_DEP", "NOM_DEP"), col.regions = list("#440154FF", "#FDE725FF"))
  epci_d44_buffer <- epci_geo[departement_44_buffer, , op = st_within]
  mapview(list(departement_44_buffer, epci_d44_buffer), zcol = c("NOM_DEP", "NOM_EPCI"), legend = F)

  departement_44_buffer_negatif <- departement_44 %>%
    st_buffer(dist = -2000)

  epci_d44 <- epci_geo[departement_44_buffer_negatif, , op = st_intersects]

  mapview(list(departement_44, epci_d44), zcol = c("NOM_DEP", "NOM_EPCI"), legend = F)


  # polygone (a)
  a_poly <- st_polygon(list(rbind(c(-1, -1), c(1, -1), c(1, 1), c(-1, -1))))
  a <- st_sfc(a_poly)
  # ligne (l)
  l1 <- st_multilinestring(list(rbind(c(0.5, -1), c(-0.5, 1))))
  l2 <- st_multilinestring(list(rbind(c(.9, -.9), c(.5, 0))))
  l <- st_sfc(l1, l2)

  # multipoints (p)
  p_matrix <- matrix(c(0.5, 1, -1, 0, 0, 1, 0.5, 1), ncol = 2)
  p_multi <- st_multipoint(x = p_matrix)
  p <- st_cast(st_sfc(p_multi), "POINT")
  st_intersects(p, a)
  st_within(p, a, sparse = F)

  st_contains(a, l, sparse = F)
  st_within(l, a, sparse = F)
  ###
}


### TMAP Example
# https://maeltheuliere.github.io/rspatial/creer-des-cartes-avec-tmap.html
# https://gganimate.com/index.html
# https://rstudio.github.io/leaflet/
#library(sf)
#if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")};library(tidyverse)
#if("tmap" %in% rownames(installed.packages()) == FALSE) {install.packages("tmap")};library(tmap)
#library(ggplot2)
#if("patchwork" %in% rownames(installed.packages()) == FALSE) {install.packages("patchwork")};library(patchwork)
#if("lwgeom" %in% rownames(installed.packages()) == FALSE) {install.packages("lwgeom")};library(lwgeom)
#library(ggspatial)
#remotes::install_github("MaelTheuliere/variousdata")
#library(variousdata)
tmap_example <- function(){
  wgs_84 <- tm_shape(World, projection = "longlat") +
    tm_polygons() +
    tm_layout("Le monde en projection WGS84", inner.margins=c(0,0,.1,0), title.size=.8)

  tm_shape(World) +
    tm_polygons(c("HPI", "economy")) +
    tm_facets(sync = TRUE, ncol = 2)

  #if("shinyjs" %in% rownames(installed.packages()) == FALSE) {install.packages("shinyjs")};library(shinyjs)
  tmaptools::palette_explorer()

  #if("paletteer" %in% rownames(installed.packages()) == FALSE) {install.packages("paletteer")};library(paletteer)
  paletteer_c("scico::berlin", n = 10)

  paletteer_c("nord:", n = 10)

  urb_anim = tm_shape(world) + tm_polygons() +
    tm_shape(urban_agglomerations) + tm_dots(size = "population_millions") +
    tm_facets(along = "year", free.coords = FALSE)

  tmap_animation(urb_anim, filename = "urb_anim.gif", delay = 25)
}

### MAPVIEW Example
#if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse", source='https://cran.rstudio.com/')};library(tidyverse)
#if("sf" %in% rownames(installed.packages()) == FALSE) {install.packages("sf")};library(sf)
# Pour mapview
#if("leaflet.providers" %in% rownames(installed.packages()) == FALSE) {install.packages('leaflet.providers', source='https://cran.rstudio.com/')};
#if("mapview" %in% rownames(installed.packages()) == FALSE) {install.packages('mapview', source='https://cran.rstudio.com/')};library(mapview)
mapview_example <- function(){
  nantes <- data.frame(lon=-1.553621,lat=47.218371) %>%
    st_as_sf(coords = c("lon", "lat")) %>%
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
  sdg_indicators_sf <- World %>%
    left_join(sdg_indicators)

  # 12.1.1 Carte choroplète (aplats de couleur)
  sdg_indicators_2015_sf <- sdg_indicators_sf %>%
    filter(timeperiod ==2015)

  sdg_indicators_2015_sf <- sdg_indicators_sf %>%
    filter(timeperiod ==2015)

  bins <-quantile(sdg_indicators_2015_sf$sh_sta_mmr,
                  na.rm=T)

  pal <- colorBin("YlOrRd", domain =
                    sdg_indicators_2015_sf$sh_sta_mmr,
                  bins = bins)

  labels <- sprintf(
    "<strong>%s</strong><br/>%g décès pour 100 000 naissance en 2015",
    sdg_indicators_2015_sf$geoareaname, sdg_indicators_2015_sf$sh_sta_mmr
  ) %>% lapply(htmltools::HTML)

  leaflet(sdg_indicators_2015_sf) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
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
  sdg_indicators %>%
    filter(timeperiod  %in% c(2000, 2005, 2010, 2015),
           geoareaname %in% c("France","Canada","Burkina Faso","China","Australia")) %>%
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

  p <- sdg_indicators %>%
    filter(timeperiod  %in% c(2000, 2005, 2010, 2015),
           geoareaname %in% c("France","Canada","Burkina Faso","China","Australia")) %>%
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

  sdg_indicators_sf <- World %>%
    left_join(sdg_indicators)
  ###
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
