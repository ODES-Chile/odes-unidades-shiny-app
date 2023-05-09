# setup -------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(sf)

# archivos ----------------------------------------------------------------
archivos <- fs::dir_ls("data/nombres/")
archivos

columna_nombre <- list(
  distrito_censal = "nom_dis",
  cuencas       = "nom_cuen",
  subcuencas    = "nom_subc",
  subsubcuencas = "nom_ssubc"
)

columna_id <- list(
  distrito_censal = "coddis",
  cuencas       = "cod_cuen",
  subcuencas    = "cod_subc",
  subsubcuencas = "cod_ssubc"
)

walk(archivos, function(fn = "data/nombres/distrito_censal.xlsx"){

  unit <- fn |>
    basename() |>
    str_remove_all("\\.xlsx")

  cli::cli_h3(unit)

  d <- readxl::read_excel(fn) |>
    select(1, 2) |>
    set_names(c(columna_id[[unit]], columna_nombre[[unit]]))

  # fngpk <- fs::dir_ls("data/vectorial/raw/") |>
  #   str_subset(str_c("/", unit))

  fngpk <- fs::dir_ls("data/vectorial/min/") |>
    str_subset(str_c("/", unit)) |>
    str_subset("_sim")

  l <- sf::read_sf(fngpk)

  cli::cli_inform("ori dimensions: {dim(l)}")

  l[[columna_nombre[[unit]]]] <- NULL

  l <- left_join(l, d, multiple = "first")

  cli::cli_inform("new dimensions: {dim(l)}")

  sf::write_sf(l, fngpk)

})
