# Paquetes ----------------------------------------------------------------
library(ggplot2) # lo principal
library(datos)   # datos
library(dplyr)   # manipulación datos

# Datos -------------------------------------------------------------------
paises

# inspeccionar datos
glimpse(paises)

# filtrar datos (por que?)
paises2 <- filter(paises, anio == max(anio))
paises2


# animaciones -------------------------------------------------------------
library(gganimate)
# https://gganimate.com/

# notar que usamos paises (no paises 2)
panim <- ggplot(
  paises,
  aes(pib_per_capita, esperanza_de_vida, size = poblacion, color = continente)
) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  # Particular para gganimate
  labs(title = 'Anio: {frame_time}', x = 'GDP per capita', y = 'Esperanza de vida') +
  transition_time(anio) +
  ease_aes('linear')

print(panim)


# interactivos ------------------------------------------------------------
library(plotly)

p <- ggplot(
  data = paises2,                            # Data
  aes(pib_per_capita,                        # Esteticas
      esperanza_de_vida
  )
) +
  geom_point(                                # Geometría
    aes(size = poblacion,
        color = continente
    )
  ) +

  facet_wrap(vars(continente)) +             # Facetas

  geom_smooth(method = "lm", se = FALSE)   + # Estadísticos

  coord_cartesian() +                        # Coordenadas
  scale_x_log10(labels = scales::dollar) +

  theme_minimal() +                          # Tema
  labs(
    x = "PIB per cápita",
    y = "Esperanza de vida"
  )

p

ggplotly(p)

# interactivos 2 ----------------------------------------------------------
library(plotly)

fig <- plot_ly(
  paises,
  x     = ~pib_per_capita,
  y     = ~esperanza_de_vida,
  size  = ~poblacion,
  color = ~continente,
  frame = ~anio,
  text  = ~pais,
  hoverinfo = "text",
  type = 'scatter',
  mode = 'markers'
)

fig <- layout(fig, xaxis = list(type = "log"))

fig
