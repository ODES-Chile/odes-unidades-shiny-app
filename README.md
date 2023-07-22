[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0) [![DOI](https://zenodo.org/badge/556055089.svg)](https://zenodo.org/badge/latestdoi/556055089)

# ODES Unidades

## Descripción

Aplicación web para visualizar información climática y de indicadores de sequía agregados a diferentes unidades administrativas: región, provincia, comuna, distrito censal; y unidades hidrológicas: cuencas, subcuencas y subsubcuencas. Utiliza datos de reanálisis climático de [ERA5-Land](https://www.ecmwf.int/en/era5-land) y del satelitae [MODIS](https://modis.gsfc.nasa.gov/) terra, agregados a nivel mensual. Las variables utilizadas corresponden a:
- Temperatura media, máxima, mínima.
- Precipitación
- Contenido de humedad de suelo a 1 metro de profundidad.
- Contenido de agua equivalente de nieve
- Índice de Diferencia Normalizada de la Vegetación (NDVI)

Desde estas variables se estima la demanda evaporativa atmosférica (AED) y los siguientes indicadores de sequía:
- SPI: índice estandarizado de precipitación
- SPEI: índice estandarizado de precipitación evapotranspiración.
- EDDI: índice de demanda evaporativa
- zcSM: índice estandarizado de humedad de suelo,
- SWEI: índice estandariado de contenido de agua equivalente de nieve.
- zcNDVI: índice estandarizado del NDVI acumulado

## Como citar

```d
Joshua Kunst, Francisco Zambrano Bigiarini, Fabián Llanos, & Joaquín Riquelme. (2023).
  ODES Unidades (v0.2.0). Zenodo. https://doi.org/10.5281/zenodo.8135799
```

