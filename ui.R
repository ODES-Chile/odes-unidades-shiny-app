# navbarPage(
page_navbar(
  title  = tags$span(
    class = "title",
    tags$a(
      tags$img(src = "horizontal_SB_blanco.png", height = "30px", style = "margin-top: -5px"),
      href = "https://odes-chile.org/"
    ),
    "Unidades"
  ),
  id = "nav",
  lang = "es",
  theme = theme_odes,
  fillable = TRUE,
  fillable_mobile = TRUE,
  # sidebar -----------------------------------------------------------------
  sidebar = sidebar(
    width = 400,
    selectInput("unidad", tags$small("Unidad administrativa"), opt_unidad),
    selectInput("macrozona", tags$small("Macrozona"), opt_macrozona, multiple = FALSE), # selected = "zona central",
    selectInput("variable", tags$small("Variable"), opt_variable, selected = "pre"),
    sliderTextInput("fecha", tags$small("Fecha"), opt_fecha, selected = c(tail(opt_fecha, 12 * 10)[1], tail(opt_fecha, 1))),

    conditionalPanel(
      "input.showchart",
      # "hchart va en 2do contitaion panel",
      highchartOutput("chart", width = "100%", height = "250px"),
      div(
        style="display:inline-block;float:right",
        downloadButton("descargar_datos_mini", "Descargar datos", class = "btn-primary btn-sm")
      )
      # tags$br(),
    ),
    conditionalPanel(
      "false",
      checkboxInput("showchart", "Mostrar información histórica"),
    ),
    # actionButton("guide", "Guide")
  ),
  # mapa --------------------------------------------------------------------
  bslib::nav_panel(
    title = "Mapa",
    icon  = icon("map-location-dot"),
    tags$head(
      tags$link(href = "Isotip_gradiente_azul.png", rel = "icon"),
      tags$script(src = "https://www.googletagmanager.com/gtag/js?id=G-CYG993XQRT", async = ""),
      tags$script(src = "js/ga.js"),
      includeCSS("www/css/styles.css"),
    ),
    leafletOutput("map", width="100%", height="100%")
  ),
  bslib::nav_panel(
    title = "Ayuda",
    icon  = icon("question"),
    layout_column_wrap(
      width = 1/2,
      fluidRow(
        column(
          width = 6,
          offset = 1,
          tags$br(),
          "Próximamente!"
          # tags$dl(
          #   tags$dt("Macrozona"),tags$dd(str_c(rep("Explicacion", 20), collapse = " ")),
          #   tags$dt("Unidad Administrativa"),tags$dd("Explicacion 2")
          #   )
        )
      )
    )
  )
)
