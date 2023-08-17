# packages ----------------------------------------------------------------
library(tidyverse)
library(patchwork)

theme_set(theme_minimal())

# data --------------------------------------------------------------------
N <- 1000

set.seed(123)

d <- tibble(
  x1 = sort(rnorm(N)),
  # caso usual, hay correlacion
  x2 = 5 + 4 * x1 + rnorm(N),
  
  # caso independencia, poca dependencia
  # x2 = 5 + 0.1 * x1 + rnorm(N),
  
  # caso total dependencia alta redundancia
  x2 = 5 + 1.5 * x1 + rnorm(N)* 0.2,
  
  id = 1:N
)

d


# PCA ---------------------------------------------------------------------
p1 <- ggplot(d, aes(x1, x2, color = id)) +
  geom_point(size = 3) +
  scale_color_viridis_c()

p1
  
pc <- prcomp(
  # eliminamos id dado que es una variable para identificar/nombrar.
  d |> select(-id)
  # usualmente uno pide acá TRUE para scale/center
  # scale = FALSE,
  # center = FALSE
  )

pc

d |> 
  summarise(
    sd(x1),
    sd(x2)
  ) |> 
  glimpse()

# 1.177005 + 0.972558
# 1.042035 + 1.115963

dpc <- bind_cols(d, as_tibble(pc$x))
dpc

dpc |> 
  mutate(
    PC1_version_manual = pc$rotation[1, 1] * x1 + pc$rotation[2, 1] * x2,
    PC2_version_manual = pc$rotation[1, 2] * x1 + pc$rotation[2, 2] * x2
  ) 


p2 <- ggplot(dpc, aes(PC1, PC2, color = id)) +
  geom_point(size = 3) +
  scale_color_viridis_c()

p2


p1 + p2 &  ggrepel::geom_text_repel(aes(label = id)) 
 
p1 + coord_equal() 

p2 + coord_equal()

p1 + p2 & coord_equal()




