library(tidyverse)

dgpk <- sf::read_sf("data/vectorial/raw/cuencas.gpkg")
plot(dgpk)

dgpk <- sf::read_sf("data/vectorial/raw/comunas.gpkg")
dgpk |>
  dplyr::filter(cut_reg == "01") |>
  plot()

dgpk |>
  dplyr::filter(cut_reg == "01") |>
  sf::st_simplify(dTolerance = 1000) |>
  plot()

dgpk |>
  dplyr::filter(cut_reg == "01") |>
  sf::st_simplify(dTolerance = 1000) |>
  plot()

# test simplify -----------------------------------------------------------
dgpk |>
  filter(region == "Metropolitana de Santiago") |>
  filter(comuna == "Isla de Maipo") |>
  plot()

dgpk |>
  filter(region == "Metropolitana de Santiago") |>
  filter(comuna == "Isla de Maipo") |>
  sf::st_simplify(dTolerance = 10) |>
  plot()



crossing(
  f = fs::dir_ls("data/vectorial/raw/"),
  t = c(10, 100, 500, 1000)
) |>
  purrr::pmap(function(f = "data/vectorial/raw/comunas.gpkg", t = 500){

    dgpkg <- sf::read_sf(f)
    tt    <- str_pad(t, 4, pad = "0")

    newfilename <- f |>
      str_replace("\\.", str_glue("{tt}.")) |>
      str_replace("raw", "min")

    dgpkg |>
      sf::st_simplify(dTolerance = t) |>
      sf::write_sf(newfilename)

  }, .progress = TRUE)


dgpk |>
  filter(region == "Metr")

dgpkmin <- sf::read_sf("data/vectorial/min/comunas0500.gpkg")
dgpkmin <- sf::read_sf("data/vectorial/min/regiones0500.gpkg")

plot(dgpkmin)

dgpkmin <- dgpkmin |>
  dplyr::mutate(value = sample(dplyr::row_number()))

pal <- leaflet::colorNumeric('RdYlGn',dgpkmin$`value`)

leaflet::leaflet() |>
  leaflet::addTiles() |>
  # leaflet::addProviderTiles("Stamen.TonerHybrid") |>
  leaflet::addPolygons(data = dgpkmin,
              fillColor = ~pal(`value`),
              color = "white",
              weight = .5,
              dashArray = "3",
              stroke = NULL,
              fillOpacity = 0.7,
              # layerId = ~COD_SSUBC,
              popup = ~paste0(value)
              )

leaflet::leaflet() |>
  leaflet::addTiles() |>
  # leaflet::addProviderTiles("Stamen.TonerHybrid") |>
  leaflet::addPolygons(data = dgpkmin,
                       fillColor = ~pal(`value`),
                       color = "white",
                       weight = .5,
                       dashArray = "3",
                       stroke = NULL,
                       fillOpacity = 0.7,
                       # layerId = ~COD_SSUBC,
                       popup = ~paste0(value)
  )
