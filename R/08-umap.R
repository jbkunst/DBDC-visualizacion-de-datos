# packages ----------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(uwot)

theme_set(theme_minimal())

# data --------------------------------------------------------------------
N <- 100

set.seed(123)

d <- tibble(
  x1 = sort(rnorm(N)),
  x2 = 5 + 1.5 * x1 + rnorm(N),
  # x2 = 5 + 0.1 * x1 + rnorm(N),
  # x2 = 5 + 1.5 * x1 + rnorm(N)* 0.2,
  id = 1:N
)

d

# UMAP --------------------------------------------------------------------
p1 <- ggplot(d, aes(x1, x2, color = id)) +
  geom_point(size = 3) +
  scale_color_viridis_c()

p1

um <- uwot::umap(
  d |> select(-id),
  n_components = 2
)

um

dumap <- bind_cols(d, as_tibble(um))
dumap


p3 <- ggplot(dumap, aes(V1, V2, color = id)) +
  geom_point(size = 3) +
  scale_color_viridis_c()

p3


p1 + p3 &  ggrepel::geom_text_repel(aes(label = id)) 

p1 + coord_equal() 

p3 + coord_equal()

p1 + p3 & coord_equal()

