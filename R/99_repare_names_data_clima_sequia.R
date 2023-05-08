
con <-  DBI::dbConnect(
  drv = RPostgres::Postgres(),
  dbname = "shiny",
  host = Sys.getenv("HOST"),
  user = "shiny",
  password = Sys.getenv("SHINY_PSQL_PWD")
)

data <- tbl(con, "data_clima_sequia")
glimpse(data)

data |> count()

data |> glimpse()


DBI::dbSendQuery(con, "ALTER TABLE data_clima_sequia RENAME \"SPEI-1\" TO spei_1;")
DBI::dbSendQuery(con, "ALTER TABLE data_clima_sequia RENAME \"SPEI-12\" TO spei_12;")
DBI::dbSendQuery(con, "ALTER TABLE data_clima_sequia RENAME \"SPEI-24\" TO spei_24;")
DBI::dbSendQuery(con, "ALTER TABLE data_clima_sequia RENAME \"SPEI-3\" TO spei_3;")
DBI::dbSendQuery(con, "ALTER TABLE data_clima_sequia RENAME \"SPEI-6\" TO spei_6;")

DBI::dbSendQuery(con, "ALTER TABLE data_clima_sequia RENAME \"SPI-1\" TO spi_1;")
DBI::dbSendQuery(con, "ALTER TABLE data_clima_sequia RENAME \"SPI-12\" TO spi_12;")
DBI::dbSendQuery(con, "ALTER TABLE data_clima_sequia RENAME \"SPI-24\" TO spi_24;")
DBI::dbSendQuery(con, "ALTER TABLE data_clima_sequia RENAME \"SPI-3\" TO spi_3;")
DBI::dbSendQuery(con, "ALTER TABLE data_clima_sequia RENAME \"SPI-6\" TO spi_6;")



