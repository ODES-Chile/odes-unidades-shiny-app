# input <- list(unidad = "distrito_censal", variable = "tas",  fecha = "2018-03-01")
# input <- list(macrozona = "zona central", unidad = "regiones", variable = "spei_12",  fecha = c("2010-04-01", "2018-12-01"), map_shape_click = list(id = "08"))
# source("global.R")

function(input, output, session) {

  # mapa principal
  output$map <- renderLeaflet({

    leaflet(
      options = leafletOptions(
        attributionControl = FALSE,
        zoomControl = FALSE
        )
      ) |>

      addProviderTiles(providers$CartoDB.Positron,  group = "CartoDB") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "ESRI WI") |>
      addProviderTiles(providers$Esri.WorldTopoMap, group = "ESRI WTM") |>

      addLayersControl(
        baseGroups = c("CartoDB", "ESRI WI", "ESRI WTM"),
        position   = "bottomright",
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      htmlwidgets::onRender("function(el, x) { L.control.zoom({ position: 'topright' }).addTo(this) }") |>
      setView(lng =  -70.64827, lat = -33.45694, zoom = 6) |>
      leafem::addLogo(
        img = "https://odes-chile.org/img/logo.png",
        src= "remote",
        position = "bottomleft",
        offset.x = 5,
        offset.y = 5,
        ) |>
      leaflet.extras::addSearchOSM(
        options = leaflet.extras::searchOptions(
          textErr = "Ubicación no encontrada",
          textCancel = "Cancelar",
          textPlaceholder = "Buscar...",
          position = "bottomright"
        )
      ) |>
      addEasyButton(
        easyButton(
          position = "bottomright",
          icon = "fa-crosshairs",
          title = "Mi ubicación",
          onClick = JS("function(btn, map){ map.locate({setView: true}); }")
          )
      )

  })

  # mini grafico
  output$chart <- renderHighchart(hc_void)

  data_coropleta <- reactive({

    cli::cli_h3("data_coropleta")
    cli::cli_alert_info("unidad {input$unidad}")
    cli::cli_alert_info("fecha  {input$fecha[[2]]}")

    u <- input$unidad
    f <- ymd(input$fecha)[2] # toma el valor máximo

    data_coropleta <- tbl(sql_con(), "data_clima_sequia") |>
      # filter(year(date) == year(f), month(date) == month(f), day(date) == day(f)) |>
      filter(date == f) |>
      filter(unit == u) |>
      rename(!!unidad_key[[u]] := code) |>
      # select(all_of(vars)) |>
      collect()

    # glimpse(data_coropleta)
    # data_coropleta |> filter(cut_com == "08101")
    data_coropleta

  })

  data_geo <- reactive({

    data_coropleta <- data_coropleta()

    cli::cli_h3("data_geo")
    cli::cli_alert_info("unidad {input$unidad}")

    u  <- input$unidad
    un <- nombre_key[[u]]
    uk <- unidad_key[[u]]

    data_geo <- sf::read_sf(str_glue("data/vectorial/raw/{u}.gpkg"))

    data_geo <- data_geo |>
      left_join(data_coropleta, by = unidad_key[[u]]) |>
      rename(
        nombre_unidad := !!un,
        id_unidad     := !!uk
      ) |>
      # filter(!is.na(valor))
      filter(!sf::st_is_empty(data_geo))

    data_geo

  })

  data_geo2 <- reactive({

    data_geo <- data_geo()

    cli::cli_h3("data_geo2 (macrozonas + variable)")
    cli::cli_alert_info("macrozona {input$macrozona}")
    cli::cli_alert_info("variable  {input$variable}")

    v  <- input$variable
    mc <- input$macrozona

    units <- dunits |>
      filter(macrozona %in% input$macrozona) |>
      filter(unit == input$unidad) |>
      pull(code)

    data_geo2 <- data_geo |>
      filter(id_unidad %in% units) |>
      rename(variable := !!v)

    data_geo2

  })

  data_unidad <- reactive({

    cli::cli_h3("data_unidad")
    cli::cli_alert_info("unidad   {input$unidad}")
    cli::cli_alert_info("idunidad {input$map_shape_click$id}")

    if(is.null(input$map_shape_click$id)) return(tibble())

    id <- input$map_shape_click$id
    u  <- input$unidad
    f1 <- ymd(input$fecha)[1]
    f2 <- ymd(input$fecha)[2]

    vr        <- names(which(input$variable == opt_variable))
    unit_name <- dunits |>
      filter(code == input$map_shape_click$id) |>
      pull(unit_name)

    data_unidad <- tbl(sql_con(), "data_clima_sequia") |>
      filter(code == id, unit == u) |>
      filter(f1 <= date) |>
      filter(date <= f2) |>
      # rename(variable := v) |>
      # select(date, code, unique(c("spei_12", "spei_24", "tas", "pre", v))) |>
      arrange(date) |>
      collect()

    attr(data_unidad, "vr")        <- vr
    attr(data_unidad, "unit_name") <- unit_name

    data_unidad

  })

  # observer de mapa
  observe({

    data_geo2 <- data_geo2()

    cli::cli_h3("observer de mapa")

    cols <- dparvar |>
      filter(variable == input$variable) |>
      pull(cols) |>
      str_split(", ", simplify = TRUE) |>
      as.vector()

   # scales::show_col(cols)

    if(str_detect(input$variable, "spi_")){

      colorData <- cut(
        data_geo2[["variable"]],
        breaks = c(-Inf, -2, -1.6, -1.3, -0.8, Inf),
        labels = c("Sequía excepcional", "Sequía extrema", "Sequía severa", "Sequía moderada", "Anormalmente seco")
      )

      data_geo2[["variable_cat"]] <- colorData

      pal <- colorFactor(cols, colorData, levels = levels(colorData))

    } else if (str_detect(input$variable, "spei_")){

      colorData <- cut(
        data_geo2[["variable"]],
        breaks = c(-Inf, -2, -1.5, -1, 1, 1.5, 2, Inf),
        labels = c("Extremadamente seco", "Severamente seco", "Moderadamente seco",
                   "Normal",
                   "Moderamente húmedo", "Muy húmedo", "Extremadamente húmedo")
      )

      data_geo2[["variable_cat"]] <- colorData

      pal <- colorFactor(cols, colorData, levels = levels(colorData))

    } else {

      colorData <- data_geo2[["variable"]]
      pal <- colorBin(cols, colorData, 10, pretty = TRUE, reverse = FALSE)

    }

    if(str_detect(input$variable, "spei_|spi_")) {
      lb <- ~paste0(nombre_unidad , " ",  round(variable, 3), " (", variable_cat, ")")
      fc <- ~pal(`variable_cat`)
    } else {
      lb <-  ~paste0(nombre_unidad , " ",  round(variable, 3))
      fc <- ~pal(`variable`)
    }

    leafletProxy("map") |>
      # leaflet() |> addTiles() |>
      clearShapes() |>
      clearTopoJSON() |>
      leaflet::addPolygons(
        data = data_geo2,
        fillColor = fc,
        weight = .5,
        dashArray = "3",
        stroke = NULL,
        fillOpacity = 0.7,
        layerId = ~id_unidad,
        label = lb,
        highlightOptions = highlightOptions(
          color = "white",
          weight = 4,
          fillColor = parametros$color,
          bringToFront = TRUE
          ),
        labelOptions = labelOptions(
          # offset = c(-20, -20),
          style = list(
            "font-family" = parametros$font_family,
            "box-shadow" = "2px 2px rgba(0,0,0,0.15)",
            "font-size" = "15px",
            "padding" = "15px",
            "border-color" = "rgba(0,0,0,0.15)"
          )
        )
      ) |>
      addLegend(
        position  = "topright",
        na.label = "No disponible",
        pal       = pal,
        values    = colorData,
        labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE)),
        layerId   = "colorLegend",
        title     = dparvar |>
                      filter(variable == input$variable) |>
                      str_glue_data("{desc} {ifelse(is.na(unidad), '', str_c('(',unidad, ')'))}")
      )

  })

  # observer que escucha click para _mostrar_ container mini grafico
  observeEvent(input$map_shape_click, {

    cli::cli_h3("observer de map_shape_click")
    cli::cli_alert_info("id {input$map_shape_click$id}")

    updateCheckboxInput(session, "showchart", value = TRUE)

  })

  # Este observer oculta container si se cambia de unidad
  # el mini gráfico se debe ocultar
  observeEvent(input$unidad, {
    updateCheckboxInput(session, "showchart", value = FALSE)
  })

  # observer de mini grafico
  observe({

    data_unidad <- data_unidad()

    cli::cli_h3("observer de mini grafico")
    cli::cli_alert_info("variable {input$variable}")

    v <- input$variable

    if(!input$showchart) return(TRUE)

    datos <-  data_unidad |>
      rename(variable := !!v) |>
      select(date, variable) |>
      filter(complete.cases(data_unidad)) |>
      select(x = date, y = variable) |>
      mutate(x = datetime_to_timestamp(x), y = round(y, 2))

    typechart <- ifelse(attr(data_unidad, "vr") == "Precipitación", "column", "spline")

    highchartProxy("chart") |>
      hcpxy_update_series(
        id = "data",
        lineWidth = 1,
        type = typechart,
        states = list(hover = list(lineWidthPlus = 0)),
        data = list_parse2(datos),
        name = attr(data_unidad, "vr"),
        color = parametros$color
      ) |>
      hcpxy_update(subtitle = list(text = attr(data_unidad, "unit_name")))

  })

  # observer de reporte
  observeEvent(input$reporte, {

    cli::cli_h3("observer reporte")

    data_unidad  <- data_unidad()

    # data_unidad <- data_unidad |>

    data_unidad[["variable"]] <- data_unidad[[input$variable]]

    data_unidad <- data_unidad |>
      select(date, code, unique(c("spei_12", "spei_24", "tas", "pre", "variable")))

    fs <- data_unidad |>
      summarise(min(date), max(date)) |>
      pivot_longer(cols = everything()) |>
      deframe()

    meses <- c("Enero", "Febrero", "Marzo", "Abril",
               "Mayo", "Junio", "Julio", "Agosto",
               "Septiembre", "Octubre", "Noviembre", "Diciembre")

    datos <- data_unidad |>
      select(date, variable) |>
      mutate(group = year(date)) |>
      mutate(x = month(date) - 1) |>
      mutate(y = variable) |>
      select(x, y, group)

    reprep <- function(n = 10, value1 = "a", value2 = "b"){
      c(rep(value1, n - 1), value2)
    }
    ngroups   <- datos |> distinct(group) |> nrow()
    typechart <- ifelse(attr(datos, "vr") == "Precipitación", "column", "spline")
    mtdta     <- dparvar |>
      filter(desc == attr(data_unidad, "vr")) |>
      pull(metadata)

    hc <- hchart(
      datos,
      typechart,
      hcaes(x, y, group = group),
      color        = reprep(ngroups, "#DDDDDD", parametros$color),
      lineWidth    = reprep(ngroups, 2, 5),
      # showInLegend = reprep(ngroups, FALSE, TRUE),
      ) |>
      hc_tooltip(table = TRUE, sort = TRUE, valueDecimals = 3) |>
      hc_xAxis(title = list(text = ""), categories = meses) |>
      hc_yAxis(title = list(text = attr(datos, "vr"))) |>
      hc_caption(text = mtdta) |>
      hc_plotOptions(spline = list(marker = list(enabled = FALSE))) |>
      # hc_exporting(enabled = TRUE) |>
      hc_legend(layout = "vertical",  align = "right", verticalAlign = "middle") |>
      hc_size(height = 350)

    hc

    un <- paste(as.character(attr(datos, "unit_name")), collapse = " ")

    data_unidad_g <- data_unidad |>
      select(-code, -variable) |>
      pivot_longer(cols = -date) |>
      group_by(name) |>
      summarise(
        last = last(value, na_rm = TRUE),
        maxi = max(value, na.rm = TRUE),
        mini = min(value, na.rm = TRUE),
        data = list(tibble(x = datetime_to_timestamp(date), y = round(value, 2)))
      ) |>
      mutate(
        hc = map(data, function(d){
          hchart(d, type = "line", color = "white",  lineWidth = 0.75) |>
            hc_xAxis(crosshair = TRUE, type = "datetime") |>
            hc_yAxis(crosshair = TRUE) |>
            hc_tooltip(pointFormat = '<b>{point.y}</b>') |>

            hc_add_theme(hc_theme_sparkline_vb()) |>
            hc_size(height = 80) |>
            hc_plotOptions(
              series = list(
                states = list(
                  hover = list(
                    enabled = FALSE
                  )
                )
              )
            )
        })
      ) |>
      mutate(across(where(is.numeric), round, 2))

    value_boxes <- data_unidad_g |>
      pmap(function(name, last, maxi, mini, data, hc){
        value_box(
          str_replace_all(str_to_upper(name), "_", " "),
          h2(HTML(last)),
          span(bsicons::bs_icon("arrow-up"), maxi, "/", bsicons::bs_icon("arrow-down"), mini),
          hc
        )
      }) |>
      map(column, width = 3) |>
      htmltools::tagList()

    showModal(
      modalDialog(
        title =  htmltools::tagList(un, tags$small(str_glue("({fs[1]} a {fs[2]})"))),
        fluidRow(value_boxes),
        tags$br(),
        hc,
        footer = tagList(
          # downloadButton("descargar_reporte", "Descargar reporte", class = "btn-primary btn-sm"),
          downloadButton("descargar_datos", "Descargar datos", class = "btn-primary btn-sm")
          ),
        size = "xl",
        easyClose = TRUE,
        fade = TRUE
      )
    )

  })

  # https://shiny.rstudio.com/articles/generating-reports.html
  output$descargar_reporte <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)

      unit_name <- dunits |>
        filter(code == input$map_shape_click$id) |>
        pull(unit_name)

      params <- list(id = input$map_shape_click$id, unit_name = unit_name)

      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )

  nombre_descarga_datos <- reactive({

    unitname <- dunits |>
      filter(unit == input$unidad, code == input$map_shape_click$id) |>
      pull(unit_name)

    str_glue("{unitname}_{input$fecha[[1]]}-{input$fecha[[2]]}.xlsx")

  })

  output$descargar_datos <- downloadHandler(
    filename = function() {
      nombre_descarga_datos()
    },
    content = function(file) {
      tempdata    <- file.path(tempdir(), "datos.xlsx")
      data_unidad <- data_unidad()
      writexl::write_xlsx(data_unidad, file)

    }
  )

}
