# Paquetes ----------------------------------------------------------------
# paquetes que se instalan 1 vez
# install.packages("ggplot2")
# install.packages("datos")
# install.packages("dplyr")

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


# Estructura Componentes --------------------------------------------------
# 
# - Data
# - Esteticas
# - Geometría
# - Facetas
# - Stats        (a veces)
# - Coordenadas  (no se usa mucho)
# - Tema

ggplot(
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


# Básico ------------------------------------------------------------------
# Argumentos:
log(x = 100, base = 10)
log(100, 10)


ggplot(data = paises2) +
  geom_point(mapping = aes(x = pib_per_capita, y = esperanza_de_vida))

ggplot(paises2) +
  geom_point(aes(pib_per_capita, esperanza_de_vida))

# Modificar atributos de la geometría que no dependa de los datos
# fuera del `aes`
ggplot(paises2) +
  geom_point(aes(pib_per_capita, esperanza_de_vida),
             color = "blue", size = 4, shape = 3)


# Crear objetos/variables:
grafico <- ggplot(paises2) +
  geom_point(aes(pib_per_capita, esperanza_de_vida),
             color = "red", shape = 21, fill = "gray80", size = 3)

grafico


# Más estéticas/más variables
ggplot(paises2) +
  geom_point(aes(pib_per_capita, esperanza_de_vida, size = poblacion, color = continente))

p <- ggplot(paises2) +
  geom_point(
    aes(pib_per_capita, 
        esperanza_de_vida, 
        size = poblacion, 
        color = continente
        )
    )

p

# Funcionalidades
colores_continentes <- c(`África`   = "#442288",
                         `Américas` = "#6ca2ea",
                         Asia       = "#b5d33d", 
                         Europe     = "#fed23f",
                         `Oceanía`  = "#eb7d5b")

p +
  scale_color_manual(values = colores_continentes)

p <- p +
  scale_color_manual(values = colores_continentes) +
  scale_x_continuous(labels = scales::dollar) +
  labs(x = "PIB per cápita", 
       y = "Esperanza de vida",
       color = "Continente",
       size = "Población",
       title = "Título",
       subtitle = "Subtítutlo",
       caption = "Caption"
       )

p


# Más funcionalidades
p +
  scale_x_log10()


p +
  scale_x_log10() +
  geom_smooth(aes(pib_per_capita, esperanza_de_vida), method = "lm")


p2 <- p +
  scale_x_log10(labels = scales::dollar) +
  geom_smooth(aes(pib_per_capita, esperanza_de_vida, color = continente), method = "lm") +
  facet_wrap(vars(continente)) +
  theme_minimal()

p2

# guardar!
ggsave(plot = p2, filename = "outputs/pib_per_capita.jpg", width = 16, height = 9, scale = 1/2)

