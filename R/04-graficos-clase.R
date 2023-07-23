# paquetes ----------------------------------------------------------------
library(ggplot2)
library(datos)
library(showtext)
library(tidyverse)
library(patchwork)

theme_set(theme_minimal(base_size = 12))
font_add_google("IBM Plex Sans", "ibm")
showtext_auto()

paises2007 <- paises |> filter(anio == max(anio))

# scatterplot
ggplot(paises2007) +
  geom_point(aes(pib_per_capita, esperanza_de_vida))

ggplot(paises2007) +
  geom_point(aes(pib_per_capita, esperanza_de_vida)) +
  scale_x_log10()

ggplot(paises2007) +
  geom_point(aes(pib_per_capita, esperanza_de_vida, size = poblacion, color = continente)) +
  scale_color_viridis_d() +
  theme(legend.position = "none")

# barplot
capa <- geom_col(aes(continente, n), fill = "cyan4", width = 0.5)

d <- paises2007 |> 
  count(continente)

p0 <- d |> 
  mutate(continente = fct_inorder(continente)) |> 
  ggplot() + capa

p0

set.seed(123)  
p1 <- d |> 
  sample_frac(1) |> 
  mutate(continente = fct_inorder(continente)) |> 
  ggplot() + capa

p1

p2 <- d |> 
  arrange(desc(n)) |> 
  mutate(continente = fct_inorder(continente)) |> 
  ggplot() + capa
  
p2

# circle parking
library(packcircles)

packing <- circleProgressiveLayout(d$n, sizetype='area')
d       <- bind_cols(d, packing)
dat.gg  <- circleLayoutVertices(paises2007, npoints=50)

p3 <- ggplot(d) +
  ggforce::geom_circle(aes(x0 = x, y0 = y, r = radius), fill = "cyan4", color = "gray70") +
  geom_text(aes(x = x, y = y, label = continente), color = "gray80") + 
  coord_equal() +
  theme_void()

p3

(p3 + 
    labs(
      x = NULL, 
      y = NULL,
      subtitle = "Separado, pero no ordenado alineado"
    )
) +
  (p1 + 
     labs(
       x = NULL, 
       y = NULL,
       subtitle = "Separado y alineado, pero no ordenado"
     )
  ) +
  (p2 + 
     labs(
       x = NULL, 
       y = NULL,
       subtitle = "Separado, alineado y ordenado"
     )
   
  
  ) 



# stacked -----------------------------------------------------------------
d2 <- paises |> 
  group_by(anio, continente) |> 
  summarise(poblacion = sum(poblacion), .groups = "drop") |> 
  ungroup() |> 
  arrange(anio)

d2

d2 |> 
  filter(anio >= 1992) |>
  mutate(anio = as.character(anio)) |> 
  ggplot() +
  geom_col(aes(anio, poblacion, fill = continente)) + 
  scale_fill_viridis_d() +
  theme(legend.position = "none")

# streamgraph -------------------------------------------------------------
library(ggstream)

ggplot(d2, aes(anio, y = poblacion, fill = continente)) +
  geom_area() +
  scale_fill_viridis_d() +
  theme(legend.position = "none")

ggplot(d2, aes(anio, y = poblacion, fill = continente)) +
  geom_col() +
  scale_fill_viridis_d() +
  theme_void() +
  theme(legend.position = "none") 
 

# line --------------------------------------------------------------------
d3 <- paises |> 
  group_by(anio) |> 
  summarise(poblacion = sum(poblacion/1e6), .groups = "drop") |> 
  ungroup() |> 
  arrange(anio)

pp <- ggplot(d3, aes(anio, poblacion)) +
  geom_point(size = 3)

pp2 <- pp +
  geom_line(linewidth = 1)

pp | pp2



