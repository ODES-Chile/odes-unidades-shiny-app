# navbarPage(
page_navbar(
  title  = tags$span("ODES Unidades", class = "title"),
  id = "nav",
  theme = theme_odes,
  # mapa --------------------------------------------------------------------
  bslib::nav(
    NULL,
    div(class="outer",
      tags$head(
        # Include our custom CSS
        includeCSS("www/css/styles.css"),
        includeScript("www/js/gomap.js")
      ),
      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),
      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(
        id = "controls",
        class = "panel panel-default",
        fixed = TRUE, draggable = FALSE,
        width = "auto", height = "auto",
        top = 46 + 10, left = 10,
        right = "auto", bottom = "auto",

        tags$br(),

        conditionalPanel(
          "input.showpanel",
          selectInput("macrozona", tags$small("Macrozona"), opt_macrozona),
          selectInput("unidad", tags$small("Unidad administrativa"), opt_unidad),
          selectInput("variable", tags$small("Variable"), opt_variable, selected = "pre"),
          sliderTextInput("fecha", tags$small("Fecha"), opt_fecha, selected = max(opt_fecha)),
          conditionalPanel(
            "true",
            # "input.station != ''",
            # checkboxInput("showchart", "Mostrar detalle estacion histórica"),
            conditionalPanel(
              "input.showchart",
              # "hchart va en 2do contitaion panel",
              highchartOutput("chart", width = "100%", height = "250px"),
              actionButton("reporte","Generar reporte", icon = icon("file"),  class = "btn-primary btn-block"),
              tags$br()
            )
          )
        ),
        conditionalPanel(
          "false",
          checkboxInput("showchart", "Mostrar información histórica"),
          ),

        prettyToggle(
          inputId = "showpanel",
          value = TRUE,
          label_on = tags$small("Esconder controles"),
          label_off = tags$small("Mostrar controles"),
          status_on = "primary",
          status_off = "info",
          icon_on = icon("caret-up"),
          icon_off = icon("caret-up", class = "fa-rotate-180")
        )
      )
    )
  )
)
