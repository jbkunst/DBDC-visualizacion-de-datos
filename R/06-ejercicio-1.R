library(tidyverse)
library(plotly)

url_datos <- "https://raw.githubusercontent.com/seankross/lego/master/data-tidy/legosets.csv"

legos <- read_csv(url_datos) |>   
  mutate(year2 = floor(Year/10)*10) 

glimpse(legos)

p <- ggplot(legos) +
  geom_point(aes(USD_MSRP, Pieces))
p 
  
ggplotly(p)

plot_ly(data = legos, x = ~USD_MSRP, y = ~Pieces)

hchart(legos, "scatter", hcaes(USD_MSRP, Pieces))

legos |> 
  select(USD_MSRP, Pieces, Image_URL) |> 
  hchart("scatter", hcaes(USD_MSRP, Pieces)) |> 
  hc_tooltip(pointFormatter = "<img src='{point.Image_URL}'></img>") |> 
  hc_xAxis(type = "log")
  




