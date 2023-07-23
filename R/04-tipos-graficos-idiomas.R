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
# Con los datos de lego realice tipos de gráficos/idiomas revisados previamente:
# - Scatterplot, que sucede?
# - Barplots
# - Line chart
# - Dot Chart
url_datos <- "https://raw.githubusercontent.com/seankross/lego/master/data-tidy/legosets.csv"
legos <- read_csv(url_datos) |> 
  mutate(year2 = floor(Year/10)*10) |> 
  sample_n(3000)



# Ejercicio 2 -------------------------------------------------------------
# penguins
# - Stackedbarchart
# - Steamgraph
# - heatmap


# Ejercicio 3 -------------------------------------------------------------
# 
library(palmerpenguins)
penguins

penguins_completos <- penguins |> 
  filter(complete.cases(penguins)) |> 
  sample_n(200)

# scatterplot matrices
library(ggforce) 

penguins_completos |> 
  select(where(is.numeric), species) |> 
  select(-year) |> 
  ggplot() +
  geom_autopoint(aes(color = species), alpha = 0.6) +
  facet_matrix(rows = vars(everything())) + 
  scale_color_manual(values = c("#4f2d7f", "#f2ad4b", "#e22d36")) + 
  theme(legend.position = "bottom")

# Parallel Coordinate
library(ggpcp)

penguins_completos |> 
  select(-year) |> 
  select(species, where(is.numeric)) |> 
  pcp_select(everything())  |> 
  pcp_scale(method="uniminmax")  |> 
  pcp_arrange() |> 
  ggplot(aes_pcp()) + 
  # geom_smooth(aes(group = species, color = species)) +
  geom_pcp_axes() + 
  geom_pcp(aes(colour = species)) +
  theme(legend.position = "bottom") + 
  scale_color_manual(values = c("#4f2d7f", "#f2ad4b", "#e22d36"))

modrpley::ripley_col

# value/pixel visualization
library(visdat)

vis_dat(penguins)

vis_dat(penguins, facet = species)

vis_miss(penguins)

vis_miss(penguins,  cluster = TRUE)

penguins |> 
  select(where(is.numeric)) |> 
  vis_value()
