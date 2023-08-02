library(ggplot2)


# datos -------------------------------------------------------------------
library(rvest)  # descargar datos de paginas web
url <- "https://www.sismologia.cl/sismicidad/catalogo/2023/08/20230802.html"

read_html(url) |>
  html_table() |>
  dplyr::nth(2) |>
  janitor::clean_names() |>
  tidyr::separate(
    latitud_longitud,
    into = c("latitud", "longitud"),
    sep = " ", convert = TRUE
  )

datos <- read_html(url) |>
  html_table() |>
  dplyr::nth(2) |>
  janitor::clean_names() |>
  tidyr::separate(
    latitud_longitud,
    into = c("latitud", "longitud"),
    sep = " ", convert = TRUE
  ) |> 
  mutate(mag = as.numeric(str_remove(magnitud_2, "Ml|Mw")))

datos



# plotly ------------------------------------------------------------------
library(plotly)

plot_ly(economics, x = ~date, y = ~pce) |> 
  add_lines() |> 
  rangeslider()


plot_ly(z = ~volcano, type = "surface")

p <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point(aes(color = Species), size = 2.5) +
  scale_color_viridis_d(end = .9) +
  geom_smooth(method = "lm") +
  facet_wrap(vars(Species)) +
  theme_minimal()

ggplotly(p)


# highcharter -------------------------------------------------------------
library(highcharter)
library(forecast)

data("AirPassengers")

modelo <- forecast(auto.arima(AirPassengers))

hchart(modelo) 

hchart(modelo) |>
  hc_navigator(enabled = TRUE) |>
  hc_rangeSelector(enabled = TRUE) |>
  hc_title(text = "Proyección")


d <- datos |> 
  select(name = fecha_local_lugar, lat = latitud, lon = longitud, z = mag)

hcmap("countries/cl/cl-all", showInLegend = FALSE) |>
  hc_add_series(
    data = d, 
    type = "mapbubble",
    name = "Terremotos", 
    minSize = "1%",
    maxSize = "5%"
  ) |>
  hc_mapNavigation(enabled = TRUE)


# leaflet -----------------------------------------------------------------
library(leaflet)

leaflet(datos) |>
  addTiles() |>
  addMarkers(
    lng = ~longitud,
    lat = ~latitud,
    popup = ~as.character(magnitud_2),
    label = ~as.character(`fecha_local_lugar`)
  ) |>
  addProviderTiles("Esri.WorldImagery")



# DT ----------------------------------------------------------------------
library(DT)

datatable(datos)

# lineupjs ----------------------------------------------------------------
library(lineupjs)

lineup(datos, height = "95vh")

