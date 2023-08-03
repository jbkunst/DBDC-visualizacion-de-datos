library(ggplot2)
library(tidyverse)

# datos -------------------------------------------------------------------
library(rvest)  # descargar datos de paginas web
url <- "https://www.sismologia.cl/sismicidad/catalogo/2023/08/20230802.html"

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

economics

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

p

ggplotly(p)


# highcharter -------------------------------------------------------------
library(highcharter)
library(forecast)

data("AirPassengers")

plot(AirPassengers)

modelo <- forecast(auto.arima(AirPassengers))

plot(AirPassengers)
plot(modelo)

autoplot(AirPassengers)
autoplot(modelo)

ggplotly(autoplot(AirPassengers))
ggplotly(autoplot(modelo))

hchart(AirPassengers)
hchart(modelo)

hchart(modelo) |>
  hc_navigator(enabled = TRUE) |>
  hc_rangeSelector(enabled = TRUE) |>
  hc_title(text = "Proyección")

d <- datos |> 
  select(name = fecha_local_lugar, lat = latitud, lon = longitud, z = mag)

d

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


# diagrammer --------------------------------------------------------------
library(DiagrammeR)

example_graph <- create_graph() |>
  add_pa_graph(
    n = 50, m = 1,
    set_seed = 23
  ) |>
  add_gnp_graph(
    n = 50, p = 1/100,
    set_seed = 23
  )

example_graph <- example_graph|>
  join_node_attrs(df = get_betweenness(example_graph))

example_graph <- example_graph |>
  join_node_attrs(df = get_degree_total(example_graph)) |>
  colorize_node_attrs(
    node_attr_from = total_degree,
    node_attr_to = fillcolor,
    palette = "Greens",
    alpha = 90
  ) |>
  rescale_node_attrs(
    node_attr_from = betweenness,
    to_lower_bound = 0.5,
    to_upper_bound = 1.0,
    node_attr_to = height
  ) |>
  select_nodes_by_id(nodes = get_articulation_points(.)) |>
  set_node_attrs_ws(node_attr = peripheries, value = 2) |>
  set_node_attrs_ws(node_attr = penwidth, value = 3) |>
  clear_selection() |>
  set_node_attr_to_display(attr = NULL)

render_graph(example_graph, layout = "nicely")


# rayshader ---------------------------------------------------------------
library(rayshader)

ggvolcano <- volcano |> 
  reshape2::melt() |> 
  as_tibble() |> 
  ggplot() +
  geom_tile(aes(x=Var1,y=Var2,fill=value)) +
  geom_contour(aes(x=Var1,y=Var2,z=value),color="black") +
  scale_x_continuous("X",expand = c(0,0)) +
  scale_y_continuous("Y",expand = c(0,0)) +
  scale_fill_gradientn("Z",colours = terrain.colors(10)) +
  coord_fixed()
ggvolcano


plot_gg(ggvolcano, multicore = TRUE, raytrace = TRUE, width = 7, height = 4, 
        scale = 300, windowsize = c(1400, 866), zoom = 0.6, phi = 30, theta = 30)
render_snapshot()

ggdiamonds <-  ggplot(diamonds, aes(x, depth)) +
  stat_density_2d(aes(fill = after_stat(nlevel)), geom = "polygon", 
                  n = 200, bins = 50,contour = TRUE) +
  facet_wrap(clarity~.) +
  scale_fill_viridis_c(option = "A")

plot_gg(ggdiamonds)


