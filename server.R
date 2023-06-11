# fmin <- "2010-04-01"
# fmax <- "2014-02-01"
# input <- list(
#   macrozona = "todas", unidad = "regiones", variable = "tas",
#   fecha = c(fmin, fmax), map_shape_click = list(id = "08"),
#   fmin = fmin, fmax = fmax
#   )
# source("global.R")

function(input, output, session) {

  data_clima_sequia <- tbl(pool, "data_clima_sequia")

  # expresion reactiva de fecha para separa minimo y máximo
  # para luego aplicar throll
  # fecha2 <- reactive(input$fecha)

  fecha2min <- reactive(input$fecha[1])
  fmin <- debounce(reactive(fecha2min()), 2000)

  fecha2max <- reactive(input$fecha[2])
  fmax <- debounce(reactive(fecha2max()), 2000)

  fmax2 <- reactiveVal(0)

  # observer para mostrar notificaión
  # A queue of notification IDs
  ids <- character(0)
  # A counter
  n <- 0

  observe({
    # Save the ID for removal later
    id <- showNotification(
      tags$small(
        shiny::icon("spinner", class = "fa-spin"),
        " Cargando información..."
      ),
      duration = 5,
      closeButton = FALSE,
      type = "message"
      )
    ids <<- c(ids, id)
    n <<- n + 1
  }) |>
    bindEvent(data_coropleta())

  # mapa principal
  output$map <- renderLeaflet({

    leaflet(
      options = leafletOptions(
        attributionControl = FALSE,
        zoomControl = FALSE
        )
      ) |>

      addProviderTiles(providers$CartoDB.Positron,  group = "Administrativo") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") |>
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topográfico") |>

      addLayersControl(
        baseGroups = c("Administrativo", "Satélite", "Topográfico"),
        position   = "bottomright",
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      htmlwidgets::onRender("function(el, x) { L.control.zoom({ position: 'topright' }).addTo(this) }") |>
      setView(lng =  -70.64827, lat = -33.45694, zoom = 4) |>
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

    fmax <- fmax()

    cli::cli_h3("data_coropleta")
    cli::cli_alert_info("unidad {input$unidad}")
    cli::cli_alert_info("fecha  {fmax}")

    u <- input$unidad
    f <- fmax

    data_coropleta <- data_clima_sequia |>
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

    # data_geo <- sf::read_sf(str_glue("data/vectorial/raw/{u}.gpkg"))
    data_geo <- sf::read_sf(str_glue("data/vectorial/min/{u}_sim.gpkg"))

    data_geo <- data_geo |>
      left_join(data_coropleta, by = unidad_key[[u]]) |>
      rename(
        nombre_unidad := !!un,
        id_unidad     := !!uk
      )
      # filter(!is.na(valor))
      # filter(!sf::st_is_empty(data_geo))

    # mapview::mapview(data_geo)
    data_geo

  })

  data_geo2 <- reactive({

    data_geo <- data_geo()

    cli::cli_h3("data_geo2 (macrozonas + variable)")
    cli::cli_alert_info("macrozona {input$macrozona}")
    cli::cli_alert_info("variable  {input$variable}")

    v  <- input$variable
    mc <- input$macrozona

    if(input$macrozona %in% "todas"){
      units <- dunits |>
        # filter(macrozona %in% input$macrozona) |>
        filter(unit == input$unidad) |>
        pull(code)
    } else {
      units <- dunits |>
        filter(macrozona %in% input$macrozona) |>
        filter(unit == input$unidad) |>
        pull(code)
    }

    data_geo2 <- data_geo |>
      filter(id_unidad %in% units) |>
      rename(variable := !!v)

    # mapview::mapview(data_geo2)
    data_geo2

  })

  data_unidad <- reactive({

    cli::cli_h3("data_unidad")
    cli::cli_alert_info("unidad   {input$unidad}")
    cli::cli_alert_info("idunidad {input$map_shape_click$id}")

    if(is.null(input$map_shape_click$id)) return(tibble())

    id <- input$map_shape_click$id
    u  <- input$unidad
    isolate(input$fecha)
    f1 <- input$fecha[1]
    f2 <- input$fecha[2]

    vr        <- names(which(input$variable == opt_variable))
    unit_name <- dunits |>
      filter(code == input$map_shape_click$id) |>
      pull(unit_name)

    data_unidad <- data_clima_sequia |>
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

    fmax      <- fmax()
    data_geo2 <- data_geo2()

    # if(fmax == fmax2()) return(TRUE)
    # fmax2(fmax)

    cli::cli_h3("observer de mapa")

    data_variable <- dparvar |>
      filter(variable == input$variable) |>
      as.list()

    cols <- data_variable$cols |>
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

      popp <- ~paste0(
        nombre_unidad , " ",  round(variable, 3), " (", variable_cat, ")",
        str_glue("<br/>{fmt_fecha(fmax)}"),
        tags$br(),
        actionButton(
          "reporte", "Reporte Sequía", class = "btn-primary btn-sm", size = 'xs',
          icon = icon('line-chart'),
          onclick = "Shiny.onInputChange('reporte', Math.random())"
        )
      )
      fc <- ~pal(`variable_cat`)

    } else {
      # numerica
      lb <-  ~paste0(nombre_unidad , " ",  round(variable, 3))

      popp <- ~paste0(
        nombre_unidad , " ",  round(variable, 3), " ", data_variable$unidad, "",
        str_glue("<br/>{fmt_fecha(fmax)}"),
        tags$br(),
        actionButton(
          "reporte", "Reporte Sequía", class = "btn-primary btn-sm", size = 'xs',
          icon = icon('line-chart'),
          onclick = "Shiny.onInputChange('reporte', Math.random())"
        )
      )

      fc <- ~pal(`variable`)
    }

    data_geo2

    # data_geo2 |> select(id_unidad) |> as.data.frame() |> count(id_unidad, sort = TRUE) |> as_tibble()

    # data_geo2 |> filter(id_unidad == 10903) |> sample_frac(1) |> mapview::mapview()

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
        popup = popp,
        label = lb,
        highlightOptions = highlightOptions(
          color = "white",
          weight = 4,
          fillColor = parametros$color,
          bringToFront = TRUE
          ),
        popupOptions = popupOptions(
          # offset = c(-20, -20),
          style = list(
            "font-family" = parametros$font_family,
            "box-shadow" = "2px 2px rgba(0,0,0,0.15)",
            "font-size" = "15px",
            "padding" = "15px",
            "border-color" = "rgba(0,0,0,0.15)"
          )
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
          str_glue_data("{desc} {ifelse(is.na(unidad), '', str_c('(',unidad, ')'))}") |>
          str_c(str_glue("<br/>{fmt_fecha(fmax)}"))
      )

    if (length(ids) > 0)
      removeNotification(ids[1])
    ids <<- ids[-1]

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
      select(date, code, unique(c("spei_12", "eddi_12", "sma_100cm", "zndvi", "variable")))

    fs <- data_unidad |>
      summarise(min(date), max(date)) |>
      pivot_longer(cols = everything()) |>
      deframe()

    meses <- c("Enero", "Febrero", "Marzo", "Abril",
               "Mayo", "Junio", "Julio", "Agosto",
               "Septiembre", "Octubre", "Noviembre", "Diciembre")

    # datos para la variable seleccionada
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

    data_unidad_g <- data_unidad_g |>
      left_join(
        dparvar |> select(name = variable, desc),
        by = "name"
      )

    value_boxes <- data_unidad_g |>
      pmap(function(name, last, maxi, mini, data, hc, desc){
        value_box(
          str_replace_all((desc), "_", " "),
          h2(HTML(last)),
          span(bsicons::bs_icon("arrow-up"), maxi, "/", bsicons::bs_icon("arrow-down"), mini),
          # hc
        )
      }) |>
      map(column, width = 3) |>
      htmltools::tagList()

    hc_sequia <- data_unidad |>
      select(-code, -variable) |>
      pivot_longer(cols = -date) |>
      left_join(
        dparvar |> select(name = variable, desc),
        by = "name"
      ) |>
      select(-name) |>
      hchart("line", hcaes(date, value, group = desc)) |>
      hc_tooltip(table = TRUE, sort = TRUE, valueDecimals = 2)

    hc_sequia

    # report <- tagList(
    #   htmltools::tags$h1(un, tags$small(str_glue("({fs[1]} a {fs[2]})"))),
    #   tags$br(),
    #   fluidRow(value_boxes),
    #   tags$br(),
    #   # hc
    #   hc_sequia
    # )
    #
    # saveRDS(report, session$token)

    showModal(
      modalDialog(
        title =  htmltools::tagList(un, tags$small(str_glue("({fs[1]} a {fs[2]})"))),
        fluidRow(value_boxes),
        tags$br(),
        # hc,
        hc_sequia,
        footer = tagList(
          # downloadButton("descargar_png", "Descargar reporte", class = "btn-primary btn-sm"),
          downloadButton("descargar_datos", "Descargar datos", class = "btn-primary btn-sm")
          ),
        size = "xl",
        easyClose = TRUE,
        fade = TRUE
      )
    )

  })

  # # https://shiny.rstudio.com/articles/generating-reports.html
  # output$descargar_reporte <- downloadHandler(
  #   filename = "report.html",
  #   content = function(file) {
  #     tempReport <- file.path(tempdir(), "report.Rmd")
  #     file.copy("report.Rmd", tempReport, overwrite = TRUE)
  #
  #     unit_name <- dunits |>
  #       filter(code == input$map_shape_click$id) |>
  #       pull(unit_name)
  #
  #     params <- list(id = input$map_shape_click$id, unit_name = unit_name)
  #
  #     rmarkdown::render(tempReport, output_file = file,
  #                       params = params,
  #                       envir = new.env(parent = globalenv())
  #     )
  #   }
  # )

  nombre_descarga_datos <- reactive({
    unitname <- dunits |>
      filter(unit == input$unidad, code == input$map_shape_click$id) |>
      pull(unit_name)
    str_glue("{unitname}_{input$fecha[[1]]}-{input$fecha[[2]]}")
  })

  output$descargar_png <- downloadHandler(
    filename = function() {
      fs::path(nombre_descarga_datos(), ext = "png")
    },
    content = function(file) {
      screenshot(
        download = FALSE,
        # id = "shiny-modal-wrapper",
        filename = "screenshot.png",
        timer = 1,
        server_dir = "."
        )
      fs::file_move("screenshot.png", file)
    }
  )

  output$descargar_datos <- downloadHandler(
    filename = function() {
      fs::path(nombre_descarga_datos(), ext = "xlsx")
    },
    content = function(file) {
      tempdata    <- file.path(tempdir(), "datos.xlsx")
      data_unidad <- data_unidad()
      data_unidad <- data_unidad |>
        select(fecha = date, eddi_12, spei_12, sma_100cm,	zndvi)
      writexl::write_xlsx(data_unidad, file)
    }
  )

}
