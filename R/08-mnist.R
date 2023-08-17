# packages ----------------------------------------------------------------
library(tidyverse)
library(uwot)
library(ggforce)
library(plotly)

theme_set(theme_minimal())

# data --------------------------------------------------------------------
url <- "https://github.com/jbkunst/random-data/raw/main/mnist/train.gz"

data <- data.table::fread(url) |> 
  as_tibble() |> 
  mutate(label = factor(label)) 

d <- head(data, 5000)

d

# PCA ---------------------------------------------------------------------
pc <- prcomp(d |> select(starts_with("pixel")))
pc

# 
# el 2 es suuuuper arbitrario, solamente para visualizar
# en general uno tiene que ver la varianza explica a.k.a. el codo
# 
d <- bind_cols(as_tibble(pc$x[, 1:2]), d)

ggplot(d) +
  geom_point(aes(PC1, PC2), alpha = 0.5)


# UMAP --------------------------------------------------------------------
um <- uwot::umap(
  d |> select(starts_with("pixel")),
  n_neighbors = 100,
  min_dist = 0.01
  )

# d <- select(d, -V1, -V2)
d <- bind_cols(as_tibble(um), d)

d

ggplot(d) +
  geom_point(aes(V1, V2), alpha = 0.5)

# entonces? ---------------------------------------------------------------
ggplot(d) +
  geom_point(aes(PC1, PC2, color = label), size = 3) +
  scale_color_viridis_d()

ggplot(d) +
  geom_point(aes(V1, V2, color = label), size = 3) +
  scale_color_viridis_d()

# que son estos datos? ----------------------------------------------------
d |> 
  mutate(id = row_number(), .before = 1) |> 
  filter(id %in% c(3, 7, 10, 1000, 60, 666, 4, 5000, 456)) |> 
  select(id, label, starts_with("pixel")) |> 
  pivot_longer(cols = starts_with("pixel")) |> 
  mutate(
    y = -as.numeric(str_sub(name,  7,  8)),
    x = as.numeric(str_sub(name, 10, 11))
  ) |> 
  ggplot(aes(x, y, fill = value)) +
  geom_tile() +
  facet_wrap(~ str_c(id, ":" ,label)) +
  coord_equal()
  
datcentro <- d |> 
  group_by(label) |> 
  summarise(
    v1 = median(V1),
    v2 = median(V2)
  )

datcentro  

ggplot(d) +
  geom_point(aes(V1, V2, color = label), size = 3) +
  geom_mark_hull(
    data = datcentro,
    aes(v1, v2, group = label, label = label),
    color = "gray"
    ) +
  scale_color_viridis_d()


# 3D ----------------------------------------------------------------------
um3d <- uwot::umap(
  d |> select(starts_with("pixel")), 
  n_components = 3
  )

d <- select(d, -V1, -V2)
d <- bind_cols(as_tibble(um3d), d)

d

# d  data
# 3d 3d
d3d <- d |> 
  head(1000) |> 
  select(starts_with("V"), label)

fig <- plot_ly(
  d3d,
  x = ~V1, y = ~V2, z = ~V3, name = ~label,
  color = ~d3d$label, 
  colors = viridisLite::viridis(10)
  ) |> 
  add_markers(size = 12)

fig


ggplot(d3d) +
  geom_point(aes(V1, V3, color = label), size = 3) +
  scale_color_viridis_d()

ggplot(d3d) +
  geom_point(aes(V2, V3, color = label), size = 3) +
  scale_color_viridis_d()

ggplot(d3d) +
  geom_point(aes(V1, V2, color = label), size = 3) +
  scale_color_viridis_d()



# splom -------------------------------------------------------------------
axis <-  list(showline=FALSE,
              zeroline=FALSE,
              gridcolor='#ffff',
              ticklen=4,
              titlefont=list(size=13)
              )


fig <- d3d |>  plot_ly()

fig <- fig |> 
  add_trace(
    type = 'splom',
    dimensions = list(
      list(label='V1', values=~V1),
      list(label='V2', values=~V2),
      list(label='V3', values=~V3)
    ),
    color = ~d3d$label, 
    colors = viridisLite::viridis(10),
    marker = list(
      size = 7,
      line = list(
        width = 1,
        color = 'rgb(230,230,230)'
      )
    )
  )
fig <- fig |>  style(diagonal = list(visible = FALSE))
fig <- fig |> 
  layout(
    hovermode='closest',
    dragmode= 'select',
    plot_bgcolor='rgba(240,240,240, 0.95)',
    xaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4),
    yaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4),
    xaxis2=axis,
    xaxis3=axis,
    yaxis2=axis,
    yaxis3=axis
  )

fig
