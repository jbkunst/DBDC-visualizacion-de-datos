# data --------------------------------------------------------------------
library(tidyverse)

url_datos <- "https://raw.githubusercontent.com/seankross/lego/master/data-tidy/legosets.csv"

legos <- read_csv(url_datos) |>   
  mutate(year2 = floor(Year/10)*10) 

glimpse(legos)


# plotly ------------------------------------------------------------------
library(plotly)

p <- ggplot(legos) +
  geom_point(aes(USD_MSRP, Pieces))
p 
  
ggplotly(p)

plot_ly(data = legos, x = ~USD_MSRP, y = ~Pieces)



# highcharter -------------------------------------------------------------
library(highcharter)

hchart(legos, "scatter", hcaes(USD_MSRP, Pieces))

tt <- tooltip_table(
  x = c("Nombre", "Tema", "Subtema", "Piezas", "Precio US"),
  y = c("{point.Name}", "{point.Theme}", "{point.Subtheme}",
        "{point.Pieces:,.0f}", "${point.USD_MSRP:,.0f}"),
  img = tags$img(src = "{point.Image_URL}", height = "155px",
                 width = "100%", style = " object-fit: cover")
  )

cat(tt)

set.seed(125)

N <- 7

dlegos <- legos |> 
  mutate(
    Minifigures = coalesce(Minifigures, 0),
    USD_MSRP = coalesce(USD_MSRP, mean(USD_MSRP, na.rm = TRUE)),
    Theme = fct_lump_n(Theme, N)
    ) |> 
  filter(Theme != "Other") 
  
dlegos

hchart(
  dlegos,
  "bubble",
  hcaes(USD_MSRP, Pieces, size = Minifigures, group = Theme),
  minSize = 2,
  maxSize = 20,
  opacity = 0.8,
  showInLegend = TRUE
  )  |> 
  hc_colors(viridis::cividis(N)) |>
  hc_title(text = "Set de Legos de acuerdo a su Precio y cantidad de Piezas") |> 
  hc_subtitle(text = "Datos de cada set de Lego producido desde 1970 hasta 2015, incluyendo el número de piezas por set, el número de minifiguras por set y el MSR específico de la región.") |> 
  hc_add_theme(hc_theme_hcrt()) |> 
  hc_yAxis(min = 1, type = "logarithmic") |> 
  hc_xAxis(min = 1, type = "logarithmic") |> 
  hc_tooltip(useHTML = TRUE, pointFormat = tt) |> 
  hc_chart(zoomType = "xy")





