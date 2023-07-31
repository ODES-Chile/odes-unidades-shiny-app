### Selectores en barra latera (_sidebar_)

**Identificación de barra lateral (Figura 1)**

![mostrar_panel](https://github.com/ODES-Chile/odes-unidades-shiny-app/assets/128641014/8a16fab6-927a-4c8d-a8c4-e936ce255ead)

**Barra lateral (Figura 1).**

- **Unidades administrativas**: Este selector permite elegir entre 7 unidades administrativas dististintas: Región, provincia, comuna, distrito censal, cuenca, subcuenca y subsubcuenca. Con el fin de obtener diversa información climática agregada a distintas unidades administrativas (Figura 2). Cada vez que se cambia de unidad administrativa, los polígonos del mapa cambiarán a la unidad seleccionada (Figura 3).


![mostrar_un_admin](https://github.com/ODES-Chile/odes-unidades-shiny-app/assets/128641014/bb8c941a-0aa6-46cf-a706-5d2196bf30af)

**Unidad Administrativa (Figura 2).**

![cambio_de_unidad](https://github.com/ODES-Chile/odes-unidades-shiny-app/assets/128641014/406cff55-28a0-42ab-b8c1-4402b0a32736)

**Cambio de Unidad Administrativa (Figura 3).**

- **Macrozona**: En esta pestaña es posible seleccionar alguna macrozona, con el fin de visualizar información sectorizada a cada una de estas (Figura 4 y 5).

![mostrar_macrozona](https://github.com/ODES-Chile/odes-unidades-shiny-app/assets/128641014/157c6253-c75a-4ac8-ae27-d9a36dd672c3)

**Identificación de selector de macrozonas (Figura 4).**

![macrozona_central](https://github.com/ODES-Chile/odes-unidades-shiny-app/assets/128641014/a7efdad3-cfae-48ed-a95e-20e330b106db)

**Información sectorizada a la Macrozona Central (Figura 5).**

- **Variable**: Aquí es posible seleccionar distintas variables climáticas entre precipitación, temperatura, humedad relativa, evapotranspiración y humedad de suelo. También es posible seleccionar distintos indicadores de sequía y vegetación. Todo esto agregado a distintas unidades administrativas (Figura 6 y 7).


![var_pp_region](https://github.com/ODES-Chile/odes-unidades-shiny-app/assets/128641014/a94a3ef5-5e5f-4c38-89df-5dceb7c57a5e)

**Variable de precipitacion agregada a regiones (Figura 6).** 

![var_SPEI12_com](https://github.com/ODES-Chile/odes-unidades-shiny-app/assets/128641014/ac8823dd-88dc-4d8e-8234-a7a38d4d4f03)

**Indicador de sequía SPEI-12 agregado a comunas (Figura 7).**

- **Fecha**: Este slider permite seleccionar distintos intervalos de fecha, desde enero de 1981 a la información más actual que entrega producto satelital. El intervalo seleccionado se visualiza en el mini gráfico (Figura 8).

![slider_fecha](https://github.com/ODES-Chile/odes-unidades-shiny-app/assets/128641014/8d1e574d-b106-46ad-bba3-38725c353fee)

**Slider selector de fecha (Figura 8).**

- **Mini gráfico**: Aquí se visualiza la serie de tiempo de la variable y fechas selecionadas. También es posible descargar en formato excel, los datos brutos del periodo de tiempo seleccionado (Figura 9).


![mini_grafico](https://github.com/ODES-Chile/odes-unidades-shiny-app/assets/128641014/678e9bf3-a008-454b-981d-416f1e73429e)

**Mini gráfico (Figura 9).**


### Secciones

#### Mapa

En el mapa se muestran las variables seleccionadas en el panel lateral. Aquí se puede seleccionar cualquier polígono y obtener el reporte de sequía agregado a la unidad administrativa seleccionada (Figura 10).


![mapa](https://github.com/ODES-Chile/odes-unidades-shiny-app/assets/128641014/b073b38a-f2c4-442e-b4d4-809500440e87)

**Mapa (Figura 10).**

#### Reporte de sequía

El reporte se genera cada vez que se hace clic en algún polígono presente en el mapa. Aquí se muestra el estado de sequía en que se encuentra la unidad administrativa seleccionada. Cada indicador de sequía posee un informativo que explica a lo que se refiere cada uno. También es posible descargar los datos brutos de los indicadores presentados en el reporte (Figura 11).


![reporte](https://github.com/ODES-Chile/odes-unidades-shiny-app/assets/128641014/73c87ba0-b46d-47c6-88d8-400aa725401b)

**Reporte (figura 11).**


### Observación
Cada vez que la página queda inactiva por aproximadamente 10 minutos, se deberá volver a cargar apretando la tecla "F5", ya que después de transcurrido ese tiempo la plataforma se suspende.
