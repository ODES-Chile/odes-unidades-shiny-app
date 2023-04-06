# packages ----------------------------------------------------------------
# SHINY
library(shiny)
library(leaflet)
library(leaflet.providers)
library(highcharter) # remotes::install_github("jbkunst/highcharter")
library(shinyWidgets)
library(bslib)
library(shinyWidgets)

# DATA
library(tidyverse)
library(lubridate)
library(RPostgres)
library(pool)

# OTHERS
library(cli)

cli::cli_h1("Start global.R")

# options -----------------------------------------------------------------
parametros <- list(
  color = "#236478",
  font_family = "Raleway",
  tabla_datos = "estaciones_datos",
  tabla_estaciones = "estaciones"
)

# source("R/helpers.R")

theme_odes <-  bs_theme(
  # version = version_default(),
  version = 5,
  # bg = "white",
  # fg = "#236478",
  primary = "#236478",
  # bootswatch = "yeti",
  base_font = font_google(parametros$font_family)
)


# helpers -----------------------------------------------------------------
hc_void <- highchart() |>
  hc_add_series(data = NULL, id = "data", showInLegend = FALSE) |>
  hc_xAxis(type = "datetime") |>
  hc_yAxis(endOnTick = FALSE, startOnTick = FALSE) |>
  hc_credits(enabled = TRUE, text = "", href = "")


# data --------------------------------------------------------------------
# data <- readRDS("data/01_dummy_data.rds")
data   <- readRDS("data/02_data_min.rds")
# data   <- readRDS("data/02_data.rds")
dunits <- readRDS("data/01_dunits.rds")
macrozonas <- sf::read_sf("data/macrozonas_chile.gpkg")

dparvar <- readxl::read_excel("data/parametros_variables.xlsx")

unidad_key <-  list(
  regiones   = "cut_reg",
  provincias = "cut_prov",
  comunas    = "cut_com",
  distrito_censal = "coddis",
  cuencas       = "cod_cuen",
  subcuencas    = "cod_subc",
  subsubcuencas = "cod_ssubc"
)

nombre_key <- list(
  comunas = "comuna",
  cuencas = "nom_cuen",
  distrito_censal = "nom_dis",
  provincias = "provincia",
  regiones = "region",
  subcuencas = "nom_subc",
  subsubcuencas = "nom_ssubc"
  )

# input opciones ----------------------------------------------------------
# opt_macrozona <- macrozonas |>
#   as.data.frame() |>
#   pull(macrozona)
opt_macrozona <- c("Todas", "Norte Grande", "Norte Chico", "Zona Central", "Zona Sur", "Zona Austral")
opt_macrozona <- str_to_lower(opt_macrozona)
opt_macrozona <- set_names(opt_macrozona, str_to_title(opt_macrozona))

opt_fecha <- data |>
  distinct(date) |>
  arrange(date) |>
  pull()

opt_variable <- data |>
  select(-unit, -code, -date) |>
  # select(-tipo, -codigo, -fecha) |>
  names()

opt_variable <- list(
  "Demanda evaporativa de la atmosfera" = "pet",
  "Precipitación" = "pre",
  "Temperatura" ="tas",
  "SPEI 1 mes" = "spei_1",
  "SPEI 3 meses" = "spei_3",
  "SPEI 6 meses" = "spei_6",
  "SPEI 9 meses" = "spei_9",
  "SPEI 12 meses" = "spei_12",
  "SPEI 24 meses" = "spei_24",
  "SPI 1 mes" = "spi_1",
  "SPI 3 meses" = "spi_3",
  "SPI 6 meses" = "spi_6",
  "SPI 9 meses" = "spi_9",
  "SPI 12 meses" = "spi_12",
  "SPI 24 meses" = "spi_24"
)

opt_unidad <-  c(
  "Regiones" = "regiones",
  "Provincias" = "provincias",
  "Comunas" = "comunas",
  `Distrito censal` = "distrito_censal",
  "Cuencas" =  "cuencas",
  "Subuencas" = "subcuencas",
  "Subsubcuencas" = "subsubcuencas"
)

opt_opts_leafletproviders <- c("CartoDB.Positron", "Esri.WorldImagery", "Esri.WorldTopoMap")

# opt_opts_yrsdata <- seq(year(fechas_min_max[1]), year(fechas_min_max[2]))

# end ---------------------------------------------------------------------
cli::cli_h1("End global.R")
