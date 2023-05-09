# setup -------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(sf)

# codigo 1 vez ------------------------------------------------------------
# estos maps fueron para conocer los codigos para cada tipo de archivo
# map(str_subset(archivos, "0010"), function(fn = "data/vectorial/min/cuencas0010.gpkg"){
#
#   message(fn)
#
#   d <- sf::read_sf(fn) |>
#     as.data.frame(d) |>
#     as_tibble() |>
#     select(-geom) |>
#     head(2) |>
#     select(where(is.character)) |>
#     select(last_col()) |>
#     names()
#
#   d
#
# }) |>
#   set_names(tipos) |>
#   dput()
#
# map(str_subset(archivos, "0010"), function(fn = "data/vectorial/min/cuencas0010.gpkg"){
#
#   message(fn)
#
#   sf::read_sf(fn) |>
#     as.data.frame(d) |>
#     as_tibble() |>
#     select(-geom) |>
#     head(2) |>
#     select(where(is.character))
#
# })

# variables ---------------------------------------------------------------
# archivos <- fs::dir_ls("data/vectorial/raw/")
archivos <- fs::dir_ls("data/vectorial/min/")
archivos

# tipos <- archivos |>
#   str_remove(".*raw/") |>
#   str_remove("\\.gpkg")
tipos <- archivos |>
  basename() |>
  str_remove("_sim\\.gpkg")
tipos

columna_id <- list(
  regiones   = "cut_reg",
  provincias = "cut_prov",
  comunas    = "cut_com",
  distrito_censal = "coddis",
  cuencas       = "cod_cuen",
  subcuencas    = "cod_subc",
  subsubcuencas = "cod_ssubc"
)

columna_nombre <- list(
  regiones   = "region",
  provincias = "provincia",
  comunas    = "comuna",
  distrito_censal = "nom_dis",
  cuencas       = "nom_cuen",
  subcuencas    = "nom_subc",
  subsubcuencas = "nom_ssubc"
)

# generar para cada unidad la macrozona asociada --------------------------
# leemos primeros tooodas las unidades de cada archivo
dunits <- map2_df(archivos, tipos, function(fn = "data/vectorial/raw/cuencas.gpkg", tipo = "cuencas"){

  message(fn, tipo)

  d <- sf::read_sf(fn) |>
    as.data.frame(d) |>
    as_tibble() |>
    select(-geom) |>
    select(code = columna_id[[tipo]], unit_name = columna_nombre[[tipo]]) |>
    mutate(unit = tipo, .before = 1)

  d

})

dunits

# leemos las macrozonas para realizar el st_join
mc1 <- sf::read_sf("data/macro_zona_buf.gpkg")
mc2 <- sf::read_sf("data/macro_zona_buf_cuen.gpkg")
# por cada tipo de unidad hacer el st_join
dunitsmc <- dunits |>
  count(unit) |>
  pull(unit) |>
  rev() |>
  map_df(function(u = "distrito_censal"){

    message(u)

    if(u == "cuencas") {
      mc <- mc2
    } else {
      mc <- mc1
    }

    d <- sf::read_sf(str_glue("data/vectorial/raw/{u}.gpkg"))
    d <- sf::st_set_crs(d, 4326)

    mc <- sf::st_transform(mc, crs = 4326)

    d <- st_join(d, mc, join = st_within)

    # d |> count(macrozona)

    d |>
      as_tibble() |>
      select(
        code = columna_id[[u]],
        unit_name = columna_nombre[[u]],
        macrozona
      )

  })

dunitsmc <- dunitsmc |>
  distinct(code, unit_name, macrozona)

dunits <- dunits |>
  distinct(unit, code, .keep_all = TRUE) |>
  left_join(dunitsmc, multiple = "all")

dunits |>
  filter(is.na(macrozona))

# fixs --------------------------------------------------------------------
dunits |> count(macrozona)
dunits |> filter(is.na(macrozona), unit == "regiones")
dunits |> filter(is.na(macrozona), unit == "provincias")
dunits |> filter(is.na(macrozona), unit == "comunas")
dunits |> filter(is.na(macrozona), unit == "cuencas")

# manual
dunits <- dunits |>
  mutate(
    macrozona = case_when(
      unit == "regiones" & is.na(macrozona) & str_detect(unit_name, "^Aysén")       ~ "zona austral",
      unit == "regiones" & is.na(macrozona) & str_detect(unit_name, "^Valparaíso")  ~ "zona central",

      unit == "provincias" & is.na(macrozona) & str_detect(unit_name, "^Valparaíso")  ~ "zona central",
      unit == "provincias" & is.na(macrozona) & str_detect(unit_name, "^Aisén")       ~ "zona austral",
      TRUE ~ macrozona
    )
  )

dunits |>
  filter(is.na(macrozona)) |>
  count(unit)

# dunits |>
#   filter(is.na(macrozona)) |>
#   writexl::write_xlsx('data/unidades_sin_macrozona.xlsx')

# fix de unidades sin macrozona
dunist_fix <- readxl::read_excel("data/unidades_sin_macrozona_corregida.xlsx")

dunits     |> count(macrozona)
dunist_fix |> count(macrozona)

dunits <- dunits |>
  left_join(
    dunist_fix |> select(code, macrozona_fix = macrozona),
    by = join_by(code)
  ) |>
  mutate(
    macrozona_fix = case_when(
      macrozona_fix == "norte_chico" ~ "norte chico",
      macrozona_fix == "centro"      ~ "zona central",
      macrozona_fix == "austral"     ~ "zona austral"
    ),
    macrozona = coalesce(macrozona, macrozona_fix)
  ) |>
  select(-macrozona_fix)

# fix de unidades con macrozona errónea
dunist_fix <- readxl::read_excel("data/correccion_unidades_mal_ubicadas.xlsx")

dunits <- dunits |>
  left_join(
    dunist_fix |> select(code, macrozona_fix = macrozona),
    by = join_by(code)
  ) |>
  mutate(
    macrozona_fix = case_when(
      macrozona_fix == "norte_chico" ~ "norte chico",
      macrozona_fix == "centro"      ~ "zona central",
      macrozona_fix == "austral"     ~ "zona austral",
      macrozona_fix == "sur"         ~ "zona sur"
    ),
    macrozona = coalesce(macrozona_fix, macrozona)
  ) |>
  select(-macrozona_fix)

# dunits |>
#   filter(unit == "comunas", str_detect(unit_name, "Coro"))
#
# dunits |>
#   filter(unit == "comunas", str_detect(unit_name, "oron"))

# save --------------------------------------------------------------------
saveRDS(dunits, "data/01_dunits.rds")


