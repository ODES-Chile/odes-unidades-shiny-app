library(sf)
library(rmapshaper)
library(fs)
library(stringr)

files <- dir_ls('data/vectorial/raw/')

names <- str_split_i(str_split_i(files,'/',4),'\\.',1)

lapply(seq_along(files),\(i){
  geo <- read_sf(files[i]) |>
    ms_simplify(keep = 0.01,
              keep_shapes = TRUE) |>
    st_make_valid() |>
    write_sf(paste0('data/vectorial/min/',names[i],'_sim.gpkg'))
})

geom <- read_sf('data/vectorial/min/comunas_sim.gpkg')

tm_shape(geom) +
  tm_polygons()
