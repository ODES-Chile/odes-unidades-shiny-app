source('global.R')
library(sf)

data_clima_sequia <- tbl(pool, "data_clima_sequia")
data_geo <- sf::read_sf(str_glue("data/vectorial/min/comunas_sim.gpkg"))

d <- data_clima_sequia |>
  filter(unit == 'comunas' & date == "2023-05-01") |>
  collect()

d2 <- left_join(data_geo,d ,by = c('cut_com' = 'code'))

genePal <- function(vals,rev = TRUE){
  palette <- c('#730000','#E60000','#FFAA00','#FFD37F','#FFFF00','#FFFFFF','#8CCDEF','#00BFFF','#1D90FF','#4169E1','#0000FF')
  perc <- c(0,.02,.05,.1,.2,.3,.7,.8,.9,.95,.98,1)
  labels <-  c("Sequía excepcional", "Sequía extrema", "Sequía severa", "Sequía moderada", "Anormalmente seco",'Normal',
             "Anormalmente humedo",'Moderadamente humedo','Severamente humedo','Extramademente humedo', 'Excepcionalmente humedo')

  vals_map <- quantile(vals,perc,na.rm = TRUE) |> unique()
  vals_cut <- cut(vals,vals_map,include.lowest = TRUE)
  n <- levels(vals_cut) |> length()
  levels(vals_cut) <- if(rev) rev(labels[1:n]) else labels[1:n]

  pal <- colorFactor(
    palette = if (rev) rev(palette[1:n]) else palette[1:n],
    domain = vals_cut)
  return(list(pal,vals_cut))
}

v <- genePal(d2$spei_24,rev = FALSE)
pal <- v[[1]]

leaflet() |>
  addProviderTiles(leaflet::providers$OpenStreetMap.Mapnik) |>
  addPolygons(data = d2,
              fillColor = ~pal(v[[2]]),
              stroke = FALSE,
              fillOpacity = 1,
              label = ~v[[2]],
              highlightOptions = highlightOptions(
                color = "white",
                weight = 4,
                fillColor = parametros$color,
                bringToFront = TRUE
              )
  )
