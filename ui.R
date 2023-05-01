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
          selectInput("macrozona", tags$small("Macrozona"), opt_macrozona, selected = "zona central", multiple = TRUE),
          selectInput("unidad", tags$small("Unidad administrativa"), opt_unidad),
          selectInput("variable", tags$small("Variable"), opt_variable, selected = "pre"),
          sliderTextInput("fecha", tags$small("Fecha"), opt_fecha, selected = c(tail(opt_fecha, 12 * 10)[1], tail(opt_fecha, 1))),

          conditionalPanel(
            "input.showchart",
            # "hchart va en 2do contitaion panel",
            highchartOutput("chart", width = "100%", height = "200px"),
            actionButton("reporte","Generar reporte", icon = icon("file"),  class = "btn-primary btn-sm"),
            tags$br(),
            tags$br(),
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
