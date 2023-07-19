# Paquetes ----------------------------------------------------------------
library(tidyverse)
library(datos)
library(scales)

# Repaso ggplot2 ----------------------------------------------------------
ggplot(
  millas,  # data
  aes(cilindrada, autopista)  # aesthetics
) +
  geom_point(aes(color = traccion), size = 4) +  # geometries
  facet_wrap(vars(anio)) +  # facets
  stat_smooth(color = "red", method = "lm") +  # stats
  scale_x_continuous(labels = comma_format(suffix = "Lts.")) +  # coords
  scale_y_continuous(labels = comma_format(suffix = "Km/l")) +  # coords
  theme_minimal(base_size = 5) +  # theme
  theme(legend.position = "bottom") +  # theme
  # otros
  scale_color_viridis_d() +
  labs(
    title = "Un hermoso título",
    subtitle = "Un bellos y extenso subtítulo",
    caption = "Un texto que nadie mira",
    x = "Cilindrada vehículo cc",
    y = "Rendimiento en autopista km/lts",
    color = "Tipo tracción"
  )


# Ejercicio 1 -------------------------------------------------------------
# Cargue los paquetes datos, ggplot2 y dplyr.
# Ejecute glimpse(vuelos).
# Objtenga una muestra de 10.000 registros para responder las preguntas utilizando la funcion sample_n.
# ¿Cuántos filas/columnas tienen los datos?
# ¿Cuántos datos son numéricos?
# Explore la relación entre distancia y tiempo_vuelo.
# ¿Qué otras preguntas tienes? ¿Como podríamos obtener QUE vuelo es el más largos?
# Reutiliza el código del ejemplo paso a paso para utilizar la función facet_wrap con estos datos.




# Ejercicio 2 -------------------------------------------------------------
# Con los datos de lego realice tipos de gráficos/idiomas revisados previamente:
# - Scatterplot
# - Barplots
# - Line chart
# - Dot Chart
url_datos <- "https://raw.githubusercontent.com/seankross/lego/master/data-tidy/legosets.csv"
legos <- read_csv(url_datos) |> 
  mutate(year2 = floor(Year/10)*10) |> 
  sample_n(3000)



# Ejercicio 3 -------------------------------------------------------------
# Con los datos de pasies:
# - Stackedbarchart
# - Steamgraph
# - heatmap


# Ejercicio 4 -------------------------------------------------------------
# Con los datos mpg
library(palmerpenguins)
penguins

penguins_completos <- penguins |> 
  filter(complete.cases(penguins)) |> 
  sample_n(200)

# scatterplot matrices
library(ggforce) 

ggplot(penguins_completos) +
  geom_autopoint(aes(color = species)) +
  facet_matrix(rows = vars(species:sex))

# Parallel Coordinate
library(ggpcp)

penguins_completos |> 
  select(-year) |> 
  select(species, where(is.numeric)) |> 
  pcp_select(everything())  |> 
  pcp_scale(method="uniminmax")  |> 
  pcp_arrange() %>%
  ggplot(aes_pcp()) + 
  geom_pcp_axes() + 
  geom_pcp(aes(colour = species))


# value/pixel visualization
library(visdat)

vis_dat(penguins)

vis_dat(penguins, facet = species)

vis_miss(penguins)

vis_miss(penguins,  cluster = TRUE)

penguins |> 
  select(where(is.numeric)) |> 
  vis_value()
