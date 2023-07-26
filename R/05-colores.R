# remotes::install_github("gastonstat/colortools")
library(colortools)

# https://www.canva.com/colors/color-wheel/
# https://paletton.com/

# circulo de tonos --------------------------------------------------------
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



