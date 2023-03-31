# input <- list(unidad = "distrito_censal", variable = "tas",  fecha = "2018-03-01")
# input <- list(macrozona = "norte chico", unidad = "regiones", variable = "pre",  fecha = "2019-04-01", map_shape_click = list(id = "08"))
# source("global.R")

function(input, output, session) {

  # main --------------------------------------------------------------------
  # mapa principal
  output$map <- renderLeaflet({

    leaflet(
      options = leafletOptions(
        attributionControl=FALSE,
        zoomControl = FALSE
        )
      ) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      htmlwidgets::onRender("function(el, x) { L.control.zoom({ position: 'topright' }).addTo(this) }") |>
      setView(lng =  -70.64827, lat = -33.45694, zoom = 6) |>
      leafem::addLogo(img = "https://odes-chile.org/img/logo.png", src= "remote", position = "bottomleft")
  })

  # mini grafico
  output$chart <- renderHighchart(hc_void)

  data_variable <- reactive({

    # cli::cli_h1("data_variable")

    u <- input$unidad
    v <- input$variable
    # f <- ymd(input$fecha)

    data_variable <- data |>
      # filter(year(date) == year(f), month(date) == month(f), day(date) == day(f)) |>
      filter(unit == u) |>
      rename(!!unidad_key[[u]] := code, valor := v) |>
      select(date, all_of(unidad_key[[u]]), valor)

    # glimpse(data_variable)

    data_variable

  })

  # actualiza fechas con valores no NA en la variable seleccionada
  observeEvent(data_variable(), {

    data_variable <- data_variable()

    # fechas con valores no nulos
    fs <- data_variable |>
      filter(!is.na(valor)) |>
      distinct(date) |>
      arrange(date) |>
      pull(date) |>
      as.character()

    updateSliderTextInput(session, "fecha", choices = fs)

  })

  data_coropleta <- reactive({

    data_variable <- data_variable()

    f <- ymd(input$fecha)

    data_coropleta <- data_variable |>
      filter(year(date) == year(f), month(date) == month(f), day(date) == day(f))

    data_coropleta

  })

  data_geo <- reactive({

    cli::cli_h2("data_geo")

    data_coropleta <- data_coropleta()

    u  <- input$unidad
    un <- nombre_key[[u]]
    uk <- unidad_key[[u]]


    if(input$macrozona == "todas"){
      # leemos simplificado
      data_geo <- sf::read_sf(str_glue("data/vectorial/min/{u}1000.gpkg"))
    } else {
      data_geo <- sf::read_sf(str_glue("data/vectorial/raw/{u}.gpkg"))

      units <- dunits |>
        filter(macrozona == input$macrozona) |>
        pull(code)

      data_geo <- data_geo |>
        filter(data_geo[[unidad_key[[u]]]] %in% units)

    }

    data_geo <- data_geo |>
      left_join(data_coropleta, by = unidad_key[[u]]) |>
      rename(
        nombre_unidad := !!un,
        id_unidad     := !!uk
      ) |>
      filter(!sf::st_is_empty(data_geo)) |>
      filter(!is.na(valor))

    data_geo

  })

  # observer de mapa
  observe({

    data_geo       <- data_geo()

    colorData <- data_geo[["valor"]]
    pal <- colorBin("RdYlBu", colorData, 10, pretty = TRUE)

    leafletProxy("map") |>
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
        # "bottomleft",
        "topright",
        pal = pal,
        values = colorData,
        title = names(which(opt_variable == input$variable)),
        # labFormat = labelFormat(suffix = ),
        layerId = "colorLegend"
      )
  })

  observeEvent(input$map_shape_click, {

    # print(input$map_shape_click)

    updateCheckboxInput(session, "showchart", value = TRUE)

    value <- input$map_shape_click$id

    updateSelectizeInput(session, inputId = "idunit", choices = value, selected = value)

  })

  observeEvent(input$unidad, {
    updateCheckboxInput(session, "showchart", value = FALSE)
  })

  data_unidad <- reactive({

    data_variable <- data_variable()

    u <- input$unidad
    un <- nombre_key[[u]]
    uk <- unidad_key[[u]]
    vr <- names(which(input$variable == opt_variable))

    data_unidad <- data_variable |>
      rename(id_unidad := !!uk) |>
      filter(id_unidad == input$map_shape_click$id) |>
      select(x = date, y = valor) |>
      mutate(date = x, x = datetime_to_timestamp(x), y = round(y, 2)) |>
      arrange(x)

    data_unidad <- data_unidad |>
      filter(complete.cases(data_unidad))

    unit_name <- dunits |>
      filter(code == input$map_shape_click$id) |>
      pull(unit_name)

    attr(data_unidad, "unit_name") <- unit_name
    attr(data_unidad, "variable")  <- vr

    data_unidad

  })

  observe({

    if(!input$showchart) return(TRUE)

    data_unidad <- data_unidad()
    datos <- data_unidad |>
      select(-date)

    highchartProxy("chart") |>
      hcpxy_update_series(
        id = "data",
        lineWidth = 1,
        states = list(hover = list(lineWidthPlus = 0)),
        data = list_parse2(datos),
        name = attr(data_unidad, "variable"),
        color = parametros$color
        ) |>
      hcpxy_update(
        title    = list(text = attr(data_unidad, "unit_name")),
        subtitle = list(text = attr(data_unidad, "variable"))
      )

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
        x = fct_inorder(x)
        )

    reprep <- function(n = 10, value1 = "a", value2 = "b"){
      c(rep(value1, n - 1), value2)
    }

    ngroups <- datos |> distinct(year) |> nrow()
    typechart <- ifelse(attr(datos, "variable") == "Precipitación", "column", "spline")

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
      hc_yAxis( title = list(text =  attr(data_unidad, "variable")))

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

  # opciones ----------------------------------------------------------------
  # mapa demo
  output$map_demo <- renderLeaflet({

    map <- leaflet(options = leafletOptions(zoomControl = FALSE)) |>
      setView(lng =  -70.64827, lat = -33.45694, zoom = 6) |>
      addProviderTiles(input$leafletprov) |>
      htmlwidgets::onRender("function(el, x) { L.control.zoom({ position: 'topright' }).addTo(this) }")

    map

  })

}
