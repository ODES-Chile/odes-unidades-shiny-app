source("global.R")

con <- pool

tbls <- DBI::dbListTables(con)
tbls

tbls |>
  map(tbl, src = con) |>
  setNames(tbls)

datas <- c("data_clima",
           "data_eddis",
           "data_speis",
           "data_spis",
           "data_sma",
           "data_zndvi"
           ) |>
  map(tbl, src = con) |>
  map(collect) |>
  map(janitor::clean_names)

# data <- list(data_clima,
#               data_eddis,
#               data_speis,
#               data_spis,
#               data_sma,
#               data_zndvi) |>
#   map(janitor::clean_names) |>
#   reduce(full_join, by = join_by(code, unit, date))

data <- datas |>
  reduce(full_join, by = join_by(code, unit, date))

glimpse(data)

DBI::dbRemoveTable(con, "data_clima_sequia")

system.time({
  DBI::dbWriteTable(con, value = data, name = "data_clima_sequia", overwrite = TRUE)
})

# spei_12
# sma_100
# eddi_12
# zndvi

tbl(sql_con(), "data_clima_sequia") |>
  filter(year(date) == 2022) |>
  show_query()

