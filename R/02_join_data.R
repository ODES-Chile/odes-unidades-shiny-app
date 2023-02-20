library(tidyverse)
library(lubridate)

d1 <- readRDS("data/data_climatica_mensual.rds")
d2 <- readRDS("data/data_sequia_meteo_mensual.rds")

d1 |> summarise(min(date), max(date))
d1 |> count(date) |> count(n)

d2 |> summarise(min(date), max(date))
d2 |> count(date) |> count(n)

d2 |>
  dtplyr::lazy_dt() |>
  count(code, unit, date, scale, sort = TRUE) |>
  collect()

d2 |>
  head() |>
  mutate(d  = as.character(date))

d2 |>
  dtplyr::lazy_dt() |>
  filter(
    code == "01",
    unit == "regiones",
    as.character(date) == "1979-02-01",
    scale == 12
    ) |>
  collect()

d2 <- d2 |>
  filter(!is.na(spei) | !is.na(spi))

d22 <- d2 |>
  distinct(code, unit, date, scale, .keep_all = TRUE)

d222 <- d22 |>
  pivot_wider(names_from = scale, values_from = c(spei, spi))

d222 |> filter(year(date) == year(max(date)))
d222 |> filter(!is.na(spi_9))

d <- full_join(d1, d222, by = c("code", "unit", "date"))

d

glimpse(d)

# dd <- readRDS("data/01_dummy_data.rds")
#
# inner_join(d, dd |> distinct(tipo, codigo), by = c(code = "codigo", unit = "tipo"))

saveRDS(d, "data/02_data.rds", compress = "xz")

d |>
  count(year(date)) |>
  View()

d |>
  filter(year(date) > 2010) |>
  saveRDS("data/02_data_min.rds", compress = "xz")


# saveRDS(d, "data/02_data.rds", compress = "xz")

