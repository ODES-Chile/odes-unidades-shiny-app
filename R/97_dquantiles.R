source("global.R")

# data_clima_sequia <- tbl(pool, "data_clima_sequia")
#
# data_clima_sequia <- data_clima_sequia |>
#   collect() |>
#   arrange(unit, code)
#
# data_clima_sequia <- data_clima_sequia |>
#   select(code, unit, matches("eddi|spei|spi|zndvi"))
#
# data.table::fwrite(data_clima_sequia, "data/data_clima_sequia.gz")

data_clima_sequia <- data.table::fread("data/data_clima_sequia.gz")
data_clima_sequia <- as_tibble(data_clima_sequia)

pbs <- c(0, .02, .05, .1, .2, .3, .7, .8, .9, .95, .98, 1)

dquantiles <- data_clima_sequia |>
  group_by(code, unit) |>
  summarise(
    across(
      matches("eddi|spei|spi|zndvi"),
      ~ list(unique(quantile(.x, probs = pbs, na.rm = TRUE)))
      )
    ) |>
  ungroup()


dquantiles |>
  pull(eddi_1) |>
  map_dbl(length) |>
  table()

dquantiles |>
  filter(map_dbl(eddi_1, length) == 1)

# extraer quantile de region 9, eddi_12
qs <- dquantiles |>
  filter(code == 9, unit == "regiones") |>
  select(eddi_12) |>
  pull() |>
  first()
qs

lbls <- c(
  "Sequía excepcional",
  "Sequía extrema",
  "Sequía severa",
  "Sequía moderada",
  "Anormalmente seco",
  'Normal',
  "Anormalmente humedo",
  'Moderadamente humedo',
  'Severamente humedo',
  'Extramademente humedo',
  'Excepcionalmente humedo'
  )

valor_mensual <- 0.4

semaforo <- cut(valor_mensual, breaks = qs, labels = lbls)

semaforo

lbls[as.numeric(semaforo)]

saveRDS("dquantiles")

dquantiles |>
  pivot_longer(cols = matches("eddi|spei|spi|zndvi")) |>
  unnest(cols = value) |>
  data.table::fwrite("data/dquantiles.gz")

dquantiles |>
  saveRDS("data/dquantiles.rds")

readRDS("data/dquantiles.rds")
