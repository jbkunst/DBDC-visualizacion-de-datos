# circulo de tonos --------------------------------------------------------
# remotes::install_github("gastonstat/colortools")
library(colortools)

# https://www.canva.com/colors/color-wheel/
# https://paletton.com/

wheel("tomato")
wheel("#47E3FF")

wheel("#c7ff00", num = 18, bg = "gray20", cex = 0.7)

wheel("#ff0000")

wheel("tomato")

analogous("tomato")

complementary("tomato")

splitComp("tomato")

triadic("tomato")

tetradic("tomato")

square("tomato")

sequential("tomato")


# categorica --------------------------------------------------------------
pals()

pals("cheer")

pizza(pals("cheer"))


# ordenada ----------------------------------------------------------------
sequential("tomato")

sequential("tomato", 10)

sequential("tomato", 10, what = "saturation")

sequential("tomato", 10, what = "saturation", fun = "log")

sequential("tomato", 10, what = "saturation", fun = "sqrt")


# Brewer ------------------------------------------------------------------
library(RColorBrewer)

display.brewer.all()

scales::show_col(brewer.pal(10, "BrBG"))

scales::show_col(brewer.pal(name = "Set1", n = 9))

# ggplot ------------------------------------------------------------------
library(tidyverse)
library(palmerpenguins)

ggplot(penguins) +
  geom_point(
    aes(bill_length_mm, bill_depth_mm, color = species),
    size = 3
  ) 


# mis defaults
library(showtext)
font_add_google("IBM Plex Sans", family = "ibm")
showtext_auto()
theme_set(theme_minimal())

p <- ggplot(penguins) +
  geom_point(
    aes(bill_length_mm, bill_depth_mm, color = species),
    size = 3
    ) 

p

set.seed(596)
diamin <- diamonds[sample(nrow(diamonds), 100),]
diamin <- diamin |> 
  mutate(depth_est = depth - mean(depth)) |> 
  arrange(desc(depth))

p2 <- ggplot(diamin, aes(carat, price)) +
    geom_point(aes(colour = clarity), size = 2)

p2

# scale_color_*
# scale_fill_*

# viridis -----------------------------------------------------------------
p
p + scale_color_viridis_d()
p + scale_color_viridis_d(option = "B")
p + scale_color_viridis_d(option = "B", begin = 0.2, end = 0.8)

p2
p2 + scale_color_viridis_d(option = "B", begin = 0.2, end = 0.8)
p2 + scale_color_viridis_d(option = "B")

# brewer ------------------------------------------------------------------
p
p + scale_color_brewer(palette = "Set1")
p + scale_color_brewer(palette = "Set3")

p2

# si es que claridad fuera SIN orden
p2 + scale_color_brewer(palette = "Set1")



# otros ggplot2 -----------------------------------------------------------
# punto de referencia (promedio)
p3 <- ggplot(diamin, aes(carat, price))  +
  geom_point(aes(colour = depth_est), size = 2) 

p3

p3 +
  scale_color_gradient2(low = "red", high = "blue", mid = "gray90", midpoint = 0)

ggplot(diamin) +
  geom_histogram(aes(depth_est))

# otros ejemplos
p3
p3 + scale_color_viridis_c()
p3 + scale_color_viridis_b(n.breaks = 7)


# :)
p3 + scale_color_gradientn(colours = rainbow(100))


# extensiones, un paso más ------------------------------------------------
library(ggtext) 

ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point(size = 3) +
  scale_color_manual(
    name = NULL,
    values = c(setosa = "#0072B2", virginica = "#009E73", versicolor = "#D55E00"),
    labels = c(
      setosa = "<i style='color:#0072B2'>I. setosa</i>",
      virginica = "<i style='color:#009E73'>I. virginica</i>",
      versicolor = "<i style='color:#D55E00'>I. versicolor</i>")
  ) +
  labs(
    title = "**Fisher's *Iris* dataset**  
    <span style='font-size:11pt'>Sepal width vs. sepal length for 
    <span style='color:#0072B2;'>setosa</span>, 
    <span style='color:#D55E00;'>versicolor</span>, and
    <span style='color:#009E73;'>virginica</span>
    </span>",
    x = "Sepal length (cm)", y = "Sepal width (cm)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_markdown(lineheight = 1.1),
    legend.text = element_markdown(size = 11)
  )


# manual (ejemplo institución) --------------------------------------------
colores <- c("#4f2d7f", "#f2ad4b", "#e22d36")
colores

p + 
  scale_color_manual(values = colores) +
  geom_smooth(aes(bill_length_mm, bill_depth_mm))

colores <- c(Gentoo = "#4f2d7f", Adelie = "#f2ad4b",  Chinstrap  = "#e22d36")
colores

p + scale_color_manual(values = colores)


p3
p3 + scale_color_gradientn(colors = colores)



# ejemplo definir escalar propias -----------------------------------------
# https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2
# https://rfortherestofus.com/2023/01/how-to-make-your-own-color-palettes-in-ggplot/
ripley_pal <- function(primary = "purple",
           other = "red",
           direction = 1) {
    ripley_colors <- list(
      `purple`     = "#4f2d7f",
      `yellow`     = "#f2ad4b",
      `red`        = "#e22d36",
      `dark grey`  = "#4c4c4c",
      `light grey` = "#E8E3E9"
    )
    
    stopifnot(primary %in% names(ripley_colors))
    
    function(n) {
      if (n > 5)
        warning("Branded Color Palette only has 5 colors.")
      
      if (n == 2) {
        other <- if (!other %in% names(ripley_colors)) {
          other
        } else {
          ripley_colors[other]
        }
        color_list <- c(other, ripley_colors[primary])
      } else {
        color_list <- ripley_colors[1:n]
      }
      
      color_list <- unname(unlist(color_list))
      if (direction >= 0)
        color_list
      else
        rev(color_list)
    }
  }

scale_color_ripley_d <- function(primary = "purple", other = "red", direction = 1,...) {
  ggplot2::discrete_scale(
    "colour", "ripley",
    ripley_pal(primary, other, direction),
    ...
  )
}
  
geom_nueva_geometria
geom_line


p



p + scale_color_ripley_d()

p + scale_color_ripley_d(direction = -1)