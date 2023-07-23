# packages ----------------------------------------------------------------
library(tidyverse)

# datos -------------------------------------------------------------------
url_datos <- "https://raw.githubusercontent.com/seankross/lego/master/data-tidy/legosets.csv"

legos <- read_csv(url_datos)

legos <- legos |> 
  mutate(year2 = floor(Year/10)*10)

legos |> 
  count(Year, year2)
  
glimpse(legos)

# scatterplot -------------------------------------------------------------
# 2 variables cuantitativas
# - piezas
# - precio
# ver1. ggplot(legos) + aes() + geom_point()
# ver2. ggplot(legos, aes()) + + geom_point()
# ver3. ggplot(legos) + + geom_point(aes())
  
ggplot(legos) +
  geom_point(aes(Pieces, USD_MSRP))


ps <- ggplot(legos, aes(Pieces, USD_MSRP)) +
  geom_point(color = "gray70") +
  theme_minimal()

ps

# tienes sus limitantes
# - 

# 
library(ggforce)

glimpse(legos)

ps +
  geom_mark_circle(
    aes(
      filter = coalesce(Pieces, 0) > 3800, 
      label = Name, 
      group = Name,
      description = Availability
      ),
    color = "gray70",
    fill = "gray90",
    # control width text
    label.minwidth = unit(100, "mm"),
    # how much distance before show legend
    label.buffer = unit(2.5, "mm"),
    label.colour = "gray30"
  )

ps +
  geom_density_2d_filled() +
  scale_x_log10() + 
  scale_y_log10()

# barplot -----------------------------------------------------------------
glimpse(legos)

legos_temas <- legos |> 
  count(Theme)

legos_temas <- legos_temas |> 
  arrange(n) |> 
  mutate(
    Theme = fct_inorder(Theme),
    cat = ggplot2::cut_number(n, 4),
    cat = fct_rev(cat)
    ) 

legos_temas

ggplot(legos_temas) +
  geom_col(aes(y = Theme, x = n)) +
  facet_wrap(vars(cat), scales = "free_y")



legos |> 
  mutate(Theme = fct_lump_n(Theme, 20)) |> 
  count(Theme) |> 
  arrange(desc(n)) |> 
  mutate(Theme = fct_inorder(Theme)) |> 
  ggplot() +
  geom_col(aes(y = Theme, x = n))
