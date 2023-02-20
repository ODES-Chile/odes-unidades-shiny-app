library(tidyverse)
library(lubridate)

archivos <- fs::dir_ls("data/vectorial/min/", regexp = "0010")

tipos <- archivos |>
  str_remove(".*min/") |>
  str_remove("[0-9]+\\.gpkg")

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

map(str_subset(archivos, "0010"), function(fn = "data/vectorial/min/cuencas0010.gpkg"){

  message(fn)

  d <- sf::read_sf(fn) |>
    as.data.frame(d) |>
    as_tibble() |>
    select(-geom) |>
    head(2) |>
    select(where(is.character)) |>
    select(last_col()) |>
    names()

  d

}) |>
  set_names(tipos) |>
  dput()

map(str_subset(archivos, "0010"), function(fn = "data/vectorial/min/cuencas0010.gpkg"){

  message(fn)

  sf::read_sf(fn) |>
    as.data.frame(d) |>
    as_tibble() |>
    select(-geom) |>
    head(2) |>
    select(where(is.character))

})

dunits <- map2_df(archivos, tipos, function(fn = "data/vectorial/min/cuencas0010.gpkg", tipo = "cuencas"){

  message(fn, tipo)

  d <- sf::read_sf(fn) |>
    as.data.frame(d) |>
    as_tibble() |>
    select(-geom) |>
    select(code = columna_id[[tipo]], unit_name = columna_nombre[[tipo]]) |>
    mutate(unit = tipo, .before = 1)

  d

})

mc <- sf::read_sf("data/macrozonas_chile.gpkg")

dunitsmc <- dunits |>
  count(unit) |>
  pull(unit) |>
  map_df(function(u = "comunas"){

    message(u)

    d <- sf::read_sf(str_glue("data/vectorial/raw/{u}.gpkg"))

    d <- sf::st_join(d, mc)

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
  left_join(dunitsmc)

dunits |>
  filter(is.na(macrozona))

dunits |> saveRDS("data/01_dunits.rds")

dat <- map2(archivos, tipos, function(fn = "data/vectorial/min/cuencas0010.gpkg", tipo = "cuencas"){

  message(fn, tipo)

  d <- sf::read_sf(fn) |>
    as.data.frame(d) |>
    as_tibble() |>
    select(-geom) |>
    select(codigo = columna_id[[tipo]]) |>
    mutate(tipo = tipo, .before = 1)

  d

  })


invisible(map(dat, glimpse))

datc <- bind_rows(dat)

datc |> count(tipo, sort = TRUE)



# dummay data -------------------------------------------------------------
fechas <- seq.Date(ymd(20210101), Sys.Date(), length.out = 50)

gen_var1 <- partial(rnorm, mean = 0, sd = 10)
gen_var2 <- partial(rgamma, shape = 50, rate = 50)
gen_var3 <- partial(rexp, rate = 1/20)

hist(gen_var1(100))
hist(gen_var2(100))
hist(gen_var3(100))

set.seed(123)

datc <- datc |>
  crossing(fecha = fechas) |>
  mutate(
    var1 = gen_var1(n()),
    var2 = gen_var2(n()),
    var3 = gen_var3(n())
  )

datc |> count(fecha)
datc |> count(fecha) |> count(n)
datc |> count(tipo, fecha)
datc |> count(tipo, fecha) |> count(n)

# export ------------------------------------------------------------------
saveRDS(datc, "data/01_dummy_data.rds", compress = "xz")



