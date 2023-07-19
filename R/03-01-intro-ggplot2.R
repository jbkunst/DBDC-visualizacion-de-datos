# Paquetes ----------------------------------------------------------------
# paquetes que se instalan 1 vez
# install.packages("ggplot2")
# install.packages("datos")
# install.packages("dplyr")
# CTRL + SHIFT + F10, reinicia R.
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

  geom_smooth(method = "lm", se = FALSE) + # Estadísticos

  coord_cartesian() +                        # Coordenadas
  scale_x_log10(labels = scales::dollar) +

  theme_minimal() +                          # Tema
  labs(
    x = "PIB per cápita",
    y = "Esperanza de vida"
    )

qplot(paises2$pib_per_capita) +
  scale_x_log10()

theme_minimal()
# Básico ------------------------------------------------------------------
# Argumentos:
log(x = 100, base = 10)
log(100, 10)
log(base = 10, 100)
log(10, x = 100)

ggplot(data = paises2) +
  geom_point(mapping = aes(x = pib_per_capita, y = esperanza_de_vida))

ggplot(paises2) +
  geom_point(aes(pib_per_capita, esperanza_de_vida))

# Modificar atributos de la geometría que no dependa de los datos
# fuera del `aes`
ggplot(paises2) +
  geom_point(aes(pib_per_capita, esperanza_de_vida),
             color = "blue", size = 4, shape = 6)

ggplot(paises2) +
  geom_point(aes(pib_per_capita, esperanza_de_vida,
                 color = continente, size = poblacion, shape = continente))


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
# vectores nombrados
x <- c(1, 2)
sum(x)
x

x2 <- c(elemento1 = 1, elemento2 = 2, `elemento tres` = 3)
sum(x2)
x2

colores_continentes <- c(África   = "#442288",
                         Américas = "#6ca2ea",
                         Asia     = "#b5d33d",
                         Europa   = "#fed23f",
                         Oceanía  = "#eb7d5b")
colores_continentes

p +
  scale_color_manual(values = colores_continentes)

# paquete::funcion
# no cargamos (library) el paquete scales, pero queremos
# usar la funcion `dollar` del paquete.
# (previamente instalad)
# scales::dollar

p <- p +
  scale_color_manual(values = colores_continentes) +
  scale_x_continuous(labels = scales::dollar) +
  labs(x = "PIB per cápita",
       y = "Esperanza de vida",
       color = "Continente",
       size = "Leyenda de\nPoblación",
       title = "Título",
       subtitle = "Subtítulo",
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
ggsave(plot = p2, filename = "pib_per_capita.svg", width = 16, height = 9, scale = 1/2)

