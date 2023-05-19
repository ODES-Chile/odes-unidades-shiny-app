# packages ----------------------------------------------------------------
# shiny
library(shiny)
library(leaflet)
library(leaflet.providers)
library(highcharter) # remotes::install_github("jbkunst/highcharter")
library(shinyWidgets)
library(bslib)

# data
library(tidyverse)
library(lubridate)
library(RPostgres)
library(pool)

# helpers
library(cli)

cli::cli_h1("Start global.R")


# helpers -----------------------------------------------------------------
sql_con <- function() {
  pool::dbPool(
    drv = RPostgres::Postgres(),
    dbname = "shiny",
    host = Sys.getenv("HOST"),
    user = "shiny",
    password = Sys.getenv("SHINY_PSQL_PWD")
  )
}

# options -----------------------------------------------------------------
parametros <- list(
  color = "#236478",
  font_family = "Raleway",
  tabla_datos = "estaciones_datos",
  tabla_estaciones = "estaciones"
)

theme_odes <-  bs_theme(
  version = 5,
  primary = parametros$color,
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
# data   <- readRDS("data/02_data_min.rds")
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
opt_macrozona <- c(
  "Todas",
  "Norte Grande",
  "Norte Chico",
  "Zona Central",
  "Zona Sur",
  "Zona Austral"
  )
opt_macrozona <- str_to_lower(opt_macrozona)
opt_macrozona <- set_names(opt_macrozona, str_to_title(opt_macrozona))

opt_fecha <- tbl(sql_con(), "data_clima_sequia") |>
  distinct(date) |>
  collect() |>
  arrange(date) |>
  filter(date <= ymd(20190601)) |>
  # filter(year(date) >= 2011) |> # cuidado con el data_variable que filtra anios
  pull()

# opt_variable <- tbl(sql_con(), "data_clima_sequia") |>
#   select(-unit, -code, -date) |>
#   # select(-tipo, -codigo, -fecha) |>
#   names()

opt_variable <- list(
  # "Variables Meteorológicas" = list(
    "Demanda evaporativa de la atmósfera" = "pet",
    "Precipitación" = "pre",
    "Temperatura" ="tas",
    "Temperatura Mínima" ="tasmin",
    "Temperatura Máxima" ="tasmax",
  #   ),
  # "Indicadores de Sequía" = list(
    "SPEI 1 mes" = "spei_1",
    "SPEI 3 meses" = "spei_3",
    "SPEI 6 meses" = "spei_6",
    "SPEI 12 meses" = "spei_12",
    "SPEI 24 meses" = "spei_24",
    "SPI 1 mes" = "spi_1",
    "SPI 3 meses" = "spi_3",
    "SPI 6 meses" = "spi_6",
    "SPI 12 meses" = "spi_12",
    "SPI 24 meses" = "spi_24"
    # )
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

# end ---------------------------------------------------------------------
cli::cli_h1("End global.R")
