# grammar of graphics -----------------------------------------------------
library(tidyverse)
data("economics_long")

economics_long

economics_long <- economics_long |> 
  mutate(
    variable = case_when(
      variable == "" ~ "",
      variable == "pce" ~ "Gastos Consumo B$",
      variable == "pop" ~ "Población",
      variable == "psavert" ~ "Tasa de ahorro",
      variable == "uempmed" ~ "Duración media desempleo",
      variable == "unemploy" ~ "Desempleados"
    )
  )



economics_long

economics_long |> 
  distinct(variable)

ggplot(economics_long) +
  geom_line(aes(x = date, y = value, group = variable))
# no tiene mucho sentido realizar esta comparación entre distintas
# unidades de medida
# tampoco queda claro cual es cada indicador

ggplot(economics_long) +
  geom_line(aes(date, value, group = variable, color = variable)) 

ggplot(economics_long) +
  geom_line(aes(date, value, group = variable, color = variable)) +
  facet_wrap(vars(variable), scales = "free_y")

ggplot(
  economics_long, 
  aes(date, value, group = variable, color = variable)
  ) +
  geom_line() +
  geom_smooth() + 
  facet_wrap(vars(variable), scales = "free_y")


# tipografía
# colores
library(showtext)
font_add_google("Oswald")
showtext_auto()

p <- ggplot(economics_long) +
  geom_line(aes(date, value, group = variable, color = variable)) +
  facet_wrap(vars(variable), scales = "free_y") +
  theme_minimal() + 
  theme(legend.position = "bottom") +
  scale_color_viridis_d(option = "B", end = 0.9) +
  scale_y_continuous(labels = scales::comma_format())

p


# interactividad ----------------------------------------------------------
library(plotly)

ggplotly(p)

library(highcharter)

hc <- hchart(
  economics_long,
  "line",
  hcaes(date, value01, group = variable)
  )

hc

hc |> 
  hc_yAxis(max = 1, min = 0) |> 
  hc_navigator(enabled = TRUE) |> 
  hc_rangeSelector(enabled = TRUE) |> 
  hc_add_theme(hc_theme_hcrt()) |> 
  hc_tooltip(table = TRUE, valueDecimals = 3)

# dashboards --------------------------------------------------------------
library(shiny)
library(bslib)
library(bsicons)

hc_void <- hchart(
  economics_long |> filter(variable == "Gastos Consumo B$"),
  "line",
  hcaes(date, value),
  id = "serieid"
)

ui <- page_navbar(
  theme = bs_theme(bootswatch = "flatly"),
  lang = "es",
  title = "Shiny",
  fillable = TRUE,
  sidebar = sidebar(
    width = 300, 
    selectInput(
      "serie", 
      label = "Variable", 
      choices = unique(economics_long$variable)
      )
    ),
  nav_panel(
    title = "",
    icon  = icon("dashboard"),
    layout_column_wrap(
      width = 1/2,
      height = "150px",
      fillable = TRUE,
      value_box(
        title = "Max",
        value = tags$h1(textOutput("var_max")),
        showcase = bs_icon("arrow-up")
      ),
      value_box(
        title = "Min",
        value = tags$h1(textOutput("var_min")),
        showcase = bs_icon("arrow-down")
      ),
    ),
    layout_column_wrap(
      width = 1,
      highchartOutput("chart")
    )
  )
)

server <- function(input, output, session) {
  # input <- list(serie = "Gastos Consumo B$")
  data <- reactive({
    data <- economics_long |> 
      filter(variable == input$serie)
    data
  })
  output$var_max <- renderText(scales::comma(max(data()$value)))
  output$var_min <- renderText(scales::comma(min(data()$value)))
  output$chart   <- renderHighchart(hc_void)
  
  observeEvent(input$serie, {
    
    data <- data()
    datos <-  data |>
      select(date, value) |>
      select(x = date, y = value) |>
      mutate(x = datetime_to_timestamp(x), y = round(y, 2))
    
    highchartProxy("chart") |>
      hcpxy_update_series(
        id = "serieid",
        lineWidth = 1,
        color = "gray",
        # type = typechart,
        states = list(hover = list(lineWidthPlus = 0)),
        data = list_parse2(datos),
        name = input$serie
      ) |>
      hcpxy_update(subtitle = list(text = input$serie))
    
  })
  
}

shinyApp(ui, server)





