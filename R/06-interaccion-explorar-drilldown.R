library(highcharter)
library(dplyr)
library(forcats)
library(purrr)
library(stringr)

# ejemplo 0 ---------------------------------------------------------------
data(gapminder, package = "gapminder")

gapminder_2007 <- gapminder::gapminder %>%
  filter(year == max(year)) %>%
  mutate(pop_mm = round(pop / 1e6))

dout <- data_to_hierarchical(gapminder_2007, c(continent, country), pop_mm)

hchart(dout, type = "sunburst")

hchart(dout, type = "sunburst", allowDrillToNode = TRUE)

hchart(dout, type = "treemap", allowDrillToNode = TRUE)

# ejemplo 1 ---------------------------------------------------------------
data(gapminder, package = "gapminder")

gapminder2007 <- gapminder |>
  filter(year == max(year)) |>
  select(-year) |>
  mutate(pop = pop/1e6) |>
  arrange(desc(pop))

gapminder_column <- gapminder2007 |>
  group_by(continent) |>
  summarise(
    lifeExp = weighted.mean(lifeExp, pop),
    gdpPercap = weighted.mean(gdpPercap, pop),
    pop = sum(pop)
  ) |>
  mutate_if(is.numeric, round) |>
  arrange(desc(pop)) |>
  mutate(continent = fct_inorder(continent))

gapminder_column

gapminder_drilldown <- gapminder2007 |>
  group_nest(continent) |>
  mutate(
    id = continent,
    type = "column",
    # in the drilldown we'll give the mapping via creating the columns
    data = map(data, mutate, name = country, y  = pop),
    data = map(data, list_parse)
  )

gapminder_drilldown

x <- c("Population (MM)", "Life expectancy at birth", "GDP per capita (US$)")
y <- c("{point.pop}", "{point.lifeExp}", "$ {point.gdpPercap}")

tt <- tooltip_table(x, y)

hchart(
  gapminder_column,
  "column",
  hcaes(x = continent, y = pop, name = continent, drilldown = continent),
  name = "Population",
  colorByPoint = TRUE
) |>
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = list_parse(gapminder_drilldown)
  ) |>
  hc_tooltip(
    pointFormat = tt, # "{point.name} {point.pop}"
    useHTML = TRUE,
    valueDecimals = 0
  ) |>
  hc_yAxis(
    title = list(text = "Population in millions (log scale)"),
    type = "logarithmic",
    minorTickInterval = 'auto'
  ) |>
  hc_xAxis(
    title = ""
  )

# ejemplo 2 ---------------------------------------------------------------
data(pokemon)

pkmn_min <- pokemon |>
  count(type_1, color = type_1_color) |>
  mutate(type_1 = fct_reorder(type_1, .x = n)) |>
  arrange(desc(type_1))

pkmn_ddn <- pokemon |>
  count(type_1, type_2, color = type_mix_color) |>
  arrange(type_1, desc(n)) |>
  mutate(type_2 = ifelse(is.na(type_2), str_c("only ", type_1), type_2)) |>
  group_nest(type_1) |>
  mutate(
    id = type_1,
    type = "column",
    # in the drilldown we'll give the mapping via creating the columns
    data = map(data, mutate, name = type_2, y  = n),
    data = map(data, list_parse)
  )

hchart(
  pkmn_min,
  type = "column",
  hcaes(x = type_1, y = n, color = color, drilldown = type_1),
  name = "Pokémons"
) |>
  hc_drilldown(
    activeAxisLabelStyle = list(textDecoration = "none"),
    allowPointDrilldown = TRUE,
    series = list_parse(pkmn_ddn)
  ) |>
  hc_yAxis(
    title = list(text = ""),
    endOnTick = FALSE,
    opposite = TRUE
  ) |>
  hc_xAxis(
    title = list(text = ""),
    endOnTick = FALSE,
    gridLineWidth = 0,
    tickWidth = 0
  ) |>
  hc_chart(
    style = list(fontFamily = "Gloria Hallelujah")
  )




