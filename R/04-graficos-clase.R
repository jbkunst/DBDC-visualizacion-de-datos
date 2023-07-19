# paquetes ----------------------------------------------------------------
library(ggplot2)
library(datos)

paises2007 <- paises |> filter(anio == max(anio))

# circle parking ----------------------------------------------------------
library(packcircles)

paises2007

# Add a column with the text you want to display for each bubble:
# data$text <- paste("name: ",data$group, "\n", "value:", data$value, "\n", "You can add a story here!")

# Generate the layout
packing    <- circleProgressiveLayout(paises2007$poblacion, sizetype='area')
paises2007 <- bind_cols(paises2007, packing)
dat.gg <- circleLayoutVertices(paises2007, npoints=50)

ggplot(paises2007) +
  ggforce::geom_circle(aes(x0 = x, y0 = y, r = radius, fill = continente), color = "gray70") +
  geom_text(aes(x = x, y = y, label = pais), ) + 
  coord_equal() +
  theme_void()
