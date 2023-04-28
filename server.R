# input <- list(unidad = "distrito_censal", variable = "tas",  fecha = "2018-03-01")
# input <- list(macrozona = "zona central", unidad = "comunas", variable = "pre",  fecha = "2019-04-01", map_shape_click = list(id = "08"))
# source("global.R")

function(input, output, session) {

  # main --------------------------------------------------------------------
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
    cli::cli_alert_info("unidad   {input$unidad}")
    cli::cli_alert_info("variable {input$variable}")
    cli::cli_alert_info("fecha    {input$fecha}")

    u <- input$unidad
    v <- input$variable
    f <- ymd(input$fecha)[2] # toma el valor máximo

    data_coropleta <- tbl(sql_con(), "data_clima_sequia") |>
      # filter(year(date) == year(f), month(date) == month(f), day(date) == day(f)) |>
      filter(date == f) |>
      filter(unit == u) |>
      rename(!!unidad_key[[u]] := code, valor := v) |>
      select(date, all_of(unidad_key[[u]]), valor) |>
      collect()

    # glimpse(data_coropleta)
    # data_coropleta |> filter(cut_com == "08101")
    data_coropleta

  })

  data_geo <- reactive({

    cli::cli_h3("data_geo")
    cli::cli_alert_info("unidad    {input$unidad}")
    cli::cli_alert_info("macrozona {input$macrozona}")

    mc <- input$macrozona
    u  <- input$unidad
    un <- nombre_key[[u]]
    uk <- unidad_key[[u]]

    data_coropleta <- data_coropleta()
    data_geo       <- sf::read_sf(str_glue("data/vectorial/raw/{u}.gpkg"))

    # if(mc != "todas") {

      units <- dunits |>
        filter(macrozona %in% input$macrozona) |>
        filter(unit == input$unidad) |>
        pull(code)

      rs <- data_geo[[unidad_key[[u]]]] %in% units

      data_geo <- data_geo |>
        filter(rs)

#     } else {
#
#       data_geo <- sf::read_sf(str_glue("data/vectorial/min/{u}1000.gpkg"))
#
#     }

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

  data_unidad <- reactive({

    cli::cli_h3("data_unidad")
    cli::cli_alert_info("idunidad {input$map_shape_click$id}")
    cli::cli_alert_info("variable {input$variable}")

    if(is.null(input$map_shape_click$id)) return(tibble())

    id <- input$map_shape_click$id
    v  <- input$variable
    f1 <- ymd(input$fecha)[1]
    f2 <- ymd(input$fecha)[2]

    vr        <- names(which(input$variable == opt_variable))
    unit_name <- dunits |>
      filter(code == input$map_shape_click$id) |>
      pull(unit_name)

    data_unidad <- tbl(sql_con(), "data_clima_sequia") |>
      filter(code == id) |>
      filter(f1 <= date) |>
      filter(date <= f2) |>
      rename(valor := v) |>
      select(date, code, valor) |>
      arrange(date) |>
      collect()

    attr(data_unidad, "vr")        <- vr
    attr(data_unidad, "unit_name") <- unit_name

    data_unidad

  })

  # observer de mapa
  observe({

    cli::cli_h3("observer de mapa")

    data_geo <- data_geo()

    colorData <- data_geo[["valor"]]

    cols <- dparvar |>
      filter(variable == input$variable) |>
      pull(cols) |>
      str_split(", ", simplify = TRUE) |>
      as.vector()

    pal <- colorBin(cols, colorData, 10, pretty = TRUE, reverse = FALSE)

    leafletProxy("map") |>
      # leaflet() |> addTiles() |>
      clearShapes() |>
      clearTopoJSON() |>
      leaflet::addPolygons(
        data = data_geo,
        fillColor = ~pal(`valor`),
        weight = .5,
        dashArray = "3",
        stroke = NULL,
        fillOpacity = 0.7,
        layerId = ~id_unidad,
        label = ~paste0(nombre_unidad , " ",  round(valor, 3)),
        highlightOptions = highlightOptions(color = "white", weight = 4,fillColor = parametros$color,bringToFront = TRUE),
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
        # popup = ~paste0(nombre_unidad , ": ",  round(valor, 3))
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

  # observer que escucha click para generar mini grafico
  observeEvent(input$map_shape_click, {

    cli::cli_h3("observer de map_shape_click")
    cli::cli_alert_info("id       {input$map_shape_click$id}")

    updateCheckboxInput(session, "showchart", value = TRUE)


  })

  # Este observer dice que si se cambia de unidad
  # el mini gráfico se debe ocultar
  observeEvent(input$unidad, {
    updateCheckboxInput(session, "showchart", value = FALSE)
  })

  # observer de mini grafico
  observe({

    cli::cli_h3("observer de mini grafico")

    data_unidad <- data_unidad()

    if(!input$showchart) return(TRUE)

    datos <- data_unidad |>
      filter(complete.cases(data_unidad)) |>
      select(x = date, y = valor) |>
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

  observeEvent(input$reporte, {

    data_unidad <- data_unidad()

    meses <- c("Enero", "Febrero", "Marzo", "Abril",
               "Mayo", "Junio", "Julio", "Agosto",
               "Septiembre", "Octubre", "Noviembre", "Diciembre")

    datos <- data_unidad |>
      mutate(
        year = year(date),
        x = meses[month(date)],
        x = fct_inorder(x),
        y = valor
        )

    reprep <- function(n = 10, value1 = "a", value2 = "b"){
      c(rep(value1, n - 1), value2)
    }

    ngroups   <- datos |> distinct(year) |> nrow()
    typechart <- ifelse(attr(datos, "vr") == "Precipitación", "column", "spline")
    mtdta     <- dparvar |>
      filter(desc == attr(data_unidad, "vr")) |>
      pull(metadata)

    hc <- hchart(
      datos,
      typechart,
      hcaes(x, y, group = year),
      color        = reprep(ngroups, "#DDDDDD", parametros$color),
      lineWidth    = reprep(ngroups, 2, 5),
      # showInLegend = reprep(ngroups, FALSE, TRUE),
      ) |>
      hc_tooltip(table = TRUE, sort = TRUE) |>
      hc_xAxis( title = list(text = "")) |>
      hc_yAxis( title = list(text =  attr(data_unidad, "variable"))) |>
      hc_caption(text = mtdta) |>
      hc_exporting(enabled = TRUE)

    hc

    un <- paste(as.character(attr(datos, "unit_name")), collapse = " ")

    showModal(
      modalDialog(
        title =  un,
        hc,
        footer = tagList(downloadButton("descargar", "Descargar reporte"), modalButton("Cerrar")),
        size = "l",
        # c("m", "s", "l", "xl"),
        easyClose = TRUE,
        fade = TRUE
      )
    )

  })

  # https://shiny.rstudio.com/articles/generating-reports.html
  output$descargar <- downloadHandler(
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

}
