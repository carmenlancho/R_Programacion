# Paquetes necesarios (ejecuta una vez si no los tienes)
# install.packages(c("readr", "dplyr", "leaflet", "htmlwidgets"))

library(readr)
library(dplyr)
library(leaflet)
library(htmlwidgets)



# 2) Limpieza mínima y filtro CDMX ------------------------------------------
# Convertimos lat/lon a numérico y filtramos por un bounding box de CDMX
# (aprox: lat 19.0–19.8, lon -99.4–-98.9)
df_cdmx <- df %>%
  mutate(
    latitud  = suppressWarnings(as.numeric(latitud)),
    longitud = suppressWarnings(as.numeric(longitud))
  ) %>%
  filter(
    !is.na(latitud), !is.na(longitud),
    dplyr::between(latitud, 19.0, 19.8),
    dplyr::between(longitud, -99.4, -98.9)
  )

# 3) Popups útiles -----------------------------------------------------------
popup_fun <- function(row) {
  # arma un popup con info básica (ajusta campos a tu gusto)
  paste0(
    "<b>Fecha:</b> ", row[["fecha_evento"]], "<br/>",
    "<b>Hora:</b> ",  row[["hora_evento"]], "<br/>",
    "<b>Tipo:</b> ",  row[["tipo_evento"]], "<br/>",
    "<b>Alcaldía:</b> ", row[["alcaldia"]], "<br/>",
    "<b>Colonia:</b> ",  row[["colonia"]], "<br/>",
    "<b>Sexo:</b> ",     row[["sexo"]], "<br/>",
    "<b>Condición:</b> ",row[["condicion_persona"]], "<br/>",
    "<b>Total:</b> ",    row[["total"]]
  )
}

popups <- apply(df_cdmx, 1, popup_fun)


# 4) Mapa leaflet (solo CDMX) -----------------------------------------------
# Si no hay puntos tras el filtro, evitamos error:
if (nrow(df_cdmx) == 0) {
  stop("No se encontraron puntos dentro del bounding box de CDMX. Revisa las columnas latitud/longitud o ajusta los límites.")
}

# Centrado automático a la extensión de los puntos
bb <- list(
  minLat = min(df_cdmx$latitud, na.rm = TRUE),
  maxLat = max(df_cdmx$latitud, na.rm = TRUE),
  minLon = min(df_cdmx$longitud, na.rm = TRUE),
  maxLon = max(df_cdmx$longitud, na.rm = TRUE)
)

mapa <- leaflet(df_cdmx, options = leafletOptions(minZoom = 9)) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    lng = ~longitud, lat = ~latitud,
    radius = 4, stroke = FALSE, fillOpacity = 0.7,
    popup = popups,
    clusterOptions = markerClusterOptions()  # quita esto si no quieres clustering
  ) %>%
  fitBounds(lng1 = bb$minLon, lat1 = bb$minLat, lng2 = bb$maxLon, lat2 = bb$maxLat)

# 5) Mostrar en Viewer y guardar como HTML ----------------------------------
mapa  # lo verás en el Viewer de RStudio

# (Opcional) Guardar como archivo HTML
# saveWidget(mapa, "mapa_cdmx_puntos.html", selfcontained = TRUE)



library(readr)
library(leaflet)

df <- read_csv("nuevo_acumulado_ht_personas_lesionadas_o_fallecidas_por_sexo_2023_12.csv",
               show_col_types = FALSE)


df$latitud  <- as.numeric(df$latitud)
df$longitud <- as.numeric(df$longitud)

# Hay datos que no cuadran con Ciudad de México
df_cdmx <- df[((df$latitud >= 19.0) & (df$latitud <= 19.8) & (df$longitud >= -99.4) & (df$longitud <= -98.9)), ]
df_cdmx <- df_cdmx[!is.na(df_cdmx$latitud) & !is.na(df_cdmx$longitud), ]





popup_fun <- function(row) {
  # arma un popup con info básica (ajusta campos a tu gusto)
  paste0(
    "<b>Fecha:</b> ", row[["fecha_evento"]], "<br/>",
    "<b>Hora:</b> ",  row[["hora_evento"]], "<br/>",
    "<b>Tipo:</b> ",  row[["tipo_evento"]], "<br/>",
    "<b>Alcaldía:</b> ", row[["alcaldia"]], "<br/>",
    "<b>Colonia:</b> ",  row[["colonia"]], "<br/>",
    "<b>Sexo:</b> ",     row[["sexo"]], "<br/>",
    "<b>Condición:</b> ",row[["condicion_persona"]], "<br/>",
    "<b>Total:</b> ",    row[["total"]]
  )
}

popups <- apply(df_cdmx, 1, popup_fun)

leaflet(df_cdmx) |>
  addTiles() |>
  addCircleMarkers(lng = ~longitud, lat = ~latitud, radius = 3, stroke = FALSE, fillOpacity = 0.7,
                   popup = popups)


mapa2 <- leaflet(df_cdmx) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(lng = ~longitud, lat = ~latitud, radius = 3, stroke = FALSE, fillOpacity = 0.7,
                   popup = popups,
                   clusterOptions = markerClusterOptions())

mapa2

