### Selectores en barra latera (_sidebar_)

**Identificación de barra lateral (Figura 1)**
![mostrar_panel](https://github.com/ODES-Chile/odes-unidades-shiny-app/assets/128641014/49a993e5-8fc8-4444-ad9b-91fed62d3764)
**Barra lateral (Figura 1).**

- **Unidades administrativas**: Este selector permite elegir entre 7 unidades administrativas dististintas: Región, provincia, comuna, distrito censal, cuenca, subcuenca y subsubcuenca. Con el fin de obtener diversa información climática agregada a distintas unidades administrativas (Figura 2). Cada vez que se cambia de unidad administrativa, los polígonos del mapa cambiarán a la unidad seleccionada (Figura 3).

![mostrar_un_admin](https://github.com/ODES-Chile/odes-unidades-shiny-app/assets/128641014/28b20c30-079b-43d3-91f9-9d73ecddf885)
**Unidad Administrativa (Figura 2).**

![cambio_de_unidad](https://github.com/ODES-Chile/odes-unidades-shiny-app/assets/128641014/cb0027e8-9f61-40b8-9540-4bc69cb99a02)
**Cambio de Unidad Administrativa (Figura 3).**

- **Macrozona**: En esta pestaña es posible seleccionar alguna macrozona, con el fin de visualizar información sectorizada a cada una de estas (Figura 4 y 5).

![mostrar_macrozona](https://github.com/ODES-Chile/odes-unidades-shiny-app/assets/128641014/e33a7aed-42fd-43f1-bc32-3745bb311258)
**Identificación de selector de macrozonas (Figura 4).**

![macrozona_central](https://github.com/ODES-Chile/odes-unidades-shiny-app/assets/128641014/debd0456-3f8a-42a4-9016-ecd1ccc0db19)
**Información sectorizada a la Macrozona Central (Figura 5).**

- **Variable**: Aquí es posible seleccionar distintas variables climáticas entre precipitación, temperatura, humedad relativa, evapotranspiración y humedad de suelo. También es posible seleccionar distintos indicadores de sequía y vegetación. Todo esto agregado a distintas unidades administrativas (Figura 6 y 7).

![var_pp_region](https://github.com/ODES-Chile/odes-unidades-shiny-app/assets/128641014/860f3d52-89b6-441e-909c-75156e0cd8aa)
**Variable de precipitacion agregada a regiones (Figura 6).** 

![var_SPEI12_com](https://github.com/ODES-Chile/odes-unidades-shiny-app/assets/128641014/9e89b00c-0633-477e-bed1-9990a012e891)
**Indicador de sequía SPEI-12 agregado a comunas (Figura 7).**

- **Fecha**: Este slider permite seleccionar distintos intervalos de fecha, desde enero de 1981 a la información más actual que entrega producto satelital. El intervalo seleccionado se visualiza en el mini gráfico.


- **Mini gráfico**: Aquí se visualiza la serie de tiempo de la variable y fechas selecionadas. También es posible descargar en formato excel, los datos brutos del periodo de tiempo seleccionado.

### Secciones

#### Mapa

En el mapa se muestran las variables seleccionadas en el panel lateral. Aquí se puede seleccionar cualquier polígono y obtener el reporte de sequía agregado a la unidad administrativa seleccionada.

#### Reporte de sequía

Reporte que muestra el estado de sequía en que se encuentra la unidad administrativa seleccionada. Cada indicador de sequía posee un informativo que explica a lo que se refiere cada uno. También es posible descargar los datos brutos de los indicadores presentados en el reporte.
