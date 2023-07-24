# fmin <- "2010-04-01"
# fmax <- "2014-02-01"
# input <- list(
#   macrozona = "todas", unidad = "regiones", variable = "tas",
#   fecha = c(fmin, fmax), map_shape_click = list(id = "08"),
#   fmin = fmin, fmax = fmax
#   )
# source("global.R")
# data_clima_sequia <- tbl(pool, "data_clima_sequia")

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
        # https://github.com/r-spatial/leafem/issues/16#issuecomment-1560516396
        img = "https://raw.githubusercontent.com/ODES-Chile/odes-unidades-shiny-app/main/www/logo.png",
        src= "remote",
        position = "bottomleft",
        offset.x = 15,
        offset.y = 5,
        ) |>
      # leaflet.extras::addSearchOSM(
      #   options = leaflet.extras::searchOptions(
      #     textErr = "Ubicación no encontrada",
      #     textCancel = "Cancelar",
      #     textPlaceholder = "Buscar...",
      #     position = "bottomright"
      #   )
      # ) |>
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

  # reactivo intermedio para filtrar por macrozona
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
      filter(code == input$map_shape_click$id, unit == u) |>
      pull(unit_name)

    data_unidad <- data_clima_sequia |>
      filter(code == id, unit == u) |>
      filter(f1 <= date) |>
      filter(date <= f2) |>
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

    cli::cli_h3("observer de mapa")

    data_variable <- dparvar |>
      filter(variable == input$variable) |>
      as.list()

    cols <- data_variable$cols |>
      str_split(", ", simplify = TRUE) |>
      as.vector()

    # si es spi, spei, eddi se usa el cortes de sequía
    if(str_detect(input$variable, "swei|spi_|spei_|eddi_|zcndvi_|zcsm_")){
      # input$variable <- "spei_12"

      colorData <- as.numeric(data_geo2[[str_c(input$variable, "_q")]])

      if(str_detect(input$variable, "eddi")) colorData <- 12 - colorData

      colorData <- factor(
        parametros$etiquetas[colorData],
        levels = parametros$etiquetas
        )

      data_geo2[["variable_cat"]] <- colorData

      pal <- colorFactor(palette = parametros$paleta, domain = colorData)

      # pal(colorData)

      # lb <- ~paste0(
      #   nombre_unidad , " ",  round(variable, 3), " (", variable_cat, ")\n",
      #   str_glue("<br/>{fmt_fecha(fmax)}<br/>")
      #   )

      lb <- data_geo2 |>
        str_glue_data("{nombre_unidad} {round(variable, parametros$round_digits)} ({variable_cat})<br>{fmt_fecha(fmax)}") |>
        map(htmltools::HTML)

      popp <- ~paste0(
        nombre_unidad , " ",  round(variable, parametros$round_digits), " (", variable_cat, ")",
        str_glue("<br/>{fmt_fecha(fmax)}<br/>"),
        tags$br(),
        actionButton(
          "reporte", "Reporte Sequía", class = "btn-primary btn-sm", size = 'xs',
          icon = icon('line-chart'),
          onclick = "Shiny.onInputChange('reporte', Math.random())"
        )
      )

    } else {
      # numerica

      colorData <- round(data_geo2[["variable"]], parametros$round_digits)

      pal <- colorBin(cols, colorData, 10, pretty = TRUE, reverse = FALSE)

      # classIntervals jenks kmeans
      cli::cli_alert_info(str_glue("classIntervals {length(colorData)}"))
      t <- Sys.time()
      Nclass <- 5
      algo <- ifelse(input$unidad == "distrito_censal", "kmeans", "jenks")
      cls_pre <- classIntervals(colorData, style = algo, Nclass)
      t <- Sys.time() - t
      cli::cli_alert_info(str_glue("classIntervals {round(t, parametros$round_digits)} {attr(t, \"units\")}"))
      cols <- colorRampPalette(cols)(Nclass)

      colorData <- santoku::chop(
        colorData,
        breaks = round(cls_pre$brks, parametros$round_digits),
        labels = santoku::lbl_glue("{l} - {r}")
        )

      pal <- colorFactor(palette = cols, domain = colorData)

      # pal(colorData)

      lb <- data_geo2 |>
        str_glue_data("{nombre_unidad} {round(variable, parametros$round_digits)} {coalesce(data_variable$unidad, \"\")}<br>{fmt_fecha(fmax)}") |>
        map(htmltools::HTML)

      # lb <- ~paste0(
      #   nombre_unidad , " ",  round(variable, 3), " ", coalesce(data_variable$unidad, ""), "\n",
      #   str_glue("{fmt_fecha(fmax)}")
      #   )

      popp <- ~paste0(
        nombre_unidad , " ",  round(variable, parametros$round_digits), " ", coalesce(data_variable$unidad, ""), "",
        str_glue("<br/>{fmt_fecha(fmax)}<br/>"),
        tags$br(),
        actionButton(
          "reporte", "Reporte Sequía", class = "btn-primary btn-sm", size = 'xs',
          icon = icon('line-chart'),
          onclick = "Shiny.onInputChange('reporte', Math.random())"
        )
      )

    }

    leafletProxy("map") |>
      # leaflet() |> addTiles() |>
      clearShapes() |>
      clearTopoJSON() |>
      leaflet::addPolygons(
        data = data_geo2,
        fillColor        = ~ pal(colorData),
        weight           = .5,
        dashArray        = "3",
        stroke           = NULL,
        fillOpacity      = 0.7,
        layerId          = ~ id_unidad,
        popup            = popp,
        label            = lb,
        highlightOptions = highlightOptions(
          color        = "white",
          weight       = 2,
          fillColor    = parametros$color,
          bringToFront = TRUE
          ),
        labelOptions = labelOptions(
          style = list(
            "font-family"  = parametros$font_family,
            "box-shadow"   = "2px 2px rgba(0,0,0,0.15)",
            "font-size"    = "15px",
            "padding"      = "15px",
            "border-color" = "rgba(0,0,0,0.15)"
          )
        )
      ) |>
      addLegend(
        position  = "topright",
        na.label  = "No disponible",
        pal       = pal,
        values    = colorData,
        # labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE)),
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
      # filter(complete.cases(data_unidad)) |>
      select(x = date, y = variable) |>
      mutate(x = datetime_to_timestamp(x), y = round(y, parametros$round_digits))

    is_special <- str_detect(input$variable, "swei|spi_|spei_|eddi_|zcndvi_|zcsm_")
    is_eddi <- str_detect(input$variable, "eddi_")

    chart_type <- case_when(
      attr(data_unidad, "vr") == "Precipitación" ~ "column",
      is_special                                 ~  "areaspline",
      TRUE                                       ~  "spline"
    )

    chart_color     <- ifelse(is_special, "#0088FF", parametros$color)
    chart_color_neg <- ifelse(is_special , "#FF0000", parametros$color)

    if (is_eddi){
      chart_color     <- ifelse(is_eddi, "#FF0000", parametros$color)
      chart_color_neg <- ifelse(is_eddi , "#0088FF", parametros$color)
    }

    highchartProxy("chart") |>
      hcpxy_update_series(
        id = "data",
        lineWidth = 1,
        type = chart_type,
        states = list(hover = list(lineWidthPlus = 0)),
        data = list_parse2(datos),
        name = attr(data_unidad, "vr"),
        color = chart_color,
        negativeColor = chart_color_neg
      ) |>
      hcpxy_update(subtitle = list(text = attr(data_unidad, "unit_name")))

  })

  # observer de reporte
  observeEvent(input$reporte, {

    cli::cli_h3("observer reporte")

    data_unidad  <- data_unidad()

    data_unidad[["variable"]] <- data_unidad[[input$variable]]

    data_unidad <- data_unidad |>
      select(date, code,
             all_of(
               c("spei_12", "spei_12_q",
                 "eddi_12", "eddi_12_q",
                 "zcsm_12","zcsm_12_q",
                 "zcndvi_3", "zcndvi_3_q",
                 "variable")
               )
             )

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

    reprep    <- function(n = 10, value1 = "a", value2 = "b"){ c(rep(value1, n - 1), value2) }
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
      select(date, spei_12, eddi_12, zcsm_12, zcndvi_3) |>
      pivot_longer(cols = -date) |>
      group_by(name) |>
      summarise(
        last = last(value, na_rm = TRUE),
        maxi = max(value, na.rm = TRUE),
        mini = min(value, na.rm = TRUE),
        data = list(tibble(x = datetime_to_timestamp(date), y = round(value, parametros$round_digits)))
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
      mutate(across(where(is.numeric), ~ round(.x, parametros$round_digits)))

    data_unidad_g2 <- data_unidad |>
      arrange(date) |>
      summarise(across(ends_with("_q"), ~ last(.x, na_rm = TRUE))) |>
      pivot_longer(cols = everything(), names_to = "name", values_to = "q") |>
      mutate(name = str_remove(name, "_q"), q = as.numeric(q))

    data_unidad_g <- data_unidad_g |>
      left_join(dparvar |> select(name = variable, desc, metadata), by = "name") |>
      left_join(data_unidad_g2, by = "name")

    data_unidad_g <- data_unidad_g |>
      mutate(
        q     = ifelse(str_detect(name, "eddi"), 12 - q, q),
        q_lbl = parametros$etiquetas[q],
        name  = str_replace_all(str_to_upper(name), "_", " "),
        name  = ifelse(name == "ZNDVI", "zNDVI", name)
      )

    tooltipIcon <- function(text, ...,
                            link = '#',
                            trigger = 'focus hover',
                            dataplacement = "auto bottom") {
      tags$a(
        ...,
        href = link,
        `data-toggle` = 'tooltip',
        `data-placement` = dataplacement,
        `data-trigger` = trigger,
        `data-html`="true",
        title = text
      )
    }

    value_boxes <- data_unidad_g |>
      pmap(function(name, last, maxi, mini, data, hc, desc, q, q_lbl, metadata){

        tt <- tooltipIcon(metadata, tags$span(icon("info-circle"), style= "opacity: 0.5;"))

        value_box(
          height = "100%",
          title = tags$h6(tags$span(name, tt), align = "center"),
          value = tags$h2(HTML(last), align = "center"),
          tags$span(q_lbl, class = str_glue("badge badge-pal{q}")),
          span("min.:", mini, "/ max.:", maxi, align = "center"),
          tags$br(),
          # tags$span(tooltipIcon(metadata, tags$h5(icon("info-circle"), style= "opacity: 0.5;")), align = "right")
          # span(bsicons::bs_icon("arrow-up"), maxi, "/", bsicons::bs_icon("arrow-down"), mini),
          # tags$em(tags$small(desc))
          # hc
        )
      }) |>
      map(column, width = 3) |>
      htmltools::tagList()

    hc_sequia <- data_unidad |>
      select(date, spei_12, eddi_12, zcsm_12, zcndvi_3) |>
      pivot_longer(cols = -date) |>
      left_join(
        dparvar |> select(name = variable, desc),
        by = "name"
      ) |>
      select(-name) |>
      hchart(
        "line",
        hcaes(date, value, group = desc),
        showInNavigator = TRUE
        # yAxis = 1:4 - 1
        ) |>
      # hc_yAxis_multiples(create_axis(naxis = 4, heights = c(1)))
      hc_tooltip(table = TRUE, sort = FALSE, valueDecimals = 2) |>
      hc_chart(zoomType = "x") |>
      hc_navigator(enabled = TRUE) |>
      hc_size(height = 400) |>
      hc_yAxis(endOnTick = FALSE, startOnTick = FALSE, title = list(text = ""))

    hc_sequia

    showModal(
      modalDialog(
        # title =  htmltools::tagList(un, tags$br(), tags$small(str_glue("De {fmt_fecha(fs[1])} a {fmt_fecha(fs[2])}"))),
        title = htmltools::tagList("Reporte Sequía ", tags$strong(un), str_glue(" {fmt_fecha(fs[2])}")),
        fluidRow(value_boxes),
        # layout_column_wrap(value_boxes, width = 1/4, fillable = TRUE),
        # layout_columns(value_boxes, col_widths = c(3, 3, 3, 3)),

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

  # descargar datos reporte
  output$descargar_datos <- downloadHandler(
    filename = function() {
      fs::path(nombre_descarga_datos(), ext = "xlsx")
    },
    content = function(file) {

      citacion_modal()

      tempdata    <- file.path(tempdir(), "datos.xlsx")
      data_unidad <- data_unidad()
      data_unidad <- data_unidad |>
        select(fecha = date, eddi_12, spei_12, zcsm_12,	zcndvi_3)
      writexl::write_xlsx(data_unidad, file)
    }
  )

  nombre_descarga_datos_mini <- reactive({
    unitname <- dunits |>
      filter(unit == input$unidad, code == input$map_shape_click$id) |>
      pull(unit_name)
    str_glue("{unitname}_{input$variable}_{input$fecha[[1]]}-{input$fecha[[2]]}")
  })

  # descargar datos mini
  output$descargar_datos_mini<- downloadHandler(
    filename = function() {
      fs::path(nombre_descarga_datos_mini(), ext = "xlsx")
    },
    content = function(file) {

      citacion_modal()

      tempdata    <- file.path(tempdir(), "datos.xlsx")
      data_unidad <- data_unidad()
      data_unidad <- data_unidad |>
        select(fecha = date, all_of(input$variable))
      writexl::write_xlsx(data_unidad, file)
    }
  )

}
