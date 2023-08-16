# packages ----------------------------------------------------------------
library(tidyverse)
library(datos)
library(skimr)
library(rsample)
library(ranger)
library(rpart)
library(partykit)
library(celavi)

# datos --------------------------------------------------------------------
datos <- datos_credito

datos <- filter(datos, complete.cases(datos))
datos <- janitor::clean_names(datos)
datos <- datos |> 
  mutate(estado = as.numeric(estado == "bueno")) 

glimpse(datos)

skimr::skim(datos)

datos <- as_tibble(datos)

datos

set.seed(123)
datos <- datos |> 
  mutate(muestra = if_else(runif(n()) < .2, "tst", "trn"))

datos |>
  count(muestra) |> 
  mutate(p = n/sum(n))

datos_trn <- datos |> 
  filter(muestra == "trn") |> 
  select(-muestra)


# tri ---------------------------------------------------------------------
# m1 <- rpart(estado~ ., data = datos_trn, control = rpart.control(maxdepth = 4))
# plot(m1)
# text(m1)

m2 <- ctree(estado ~ ., data = datos_trn, control = ctree_control(maxdepth = Inf))
plot(m2)

# rl ----------------------------------------------------------------------
m3 <- glm(estado ~ ., data = datos_trn, family = binomial())
m3 <- step(m3)
m3

predict(m3, type = "response")

broom::tidy(m3)


# rf ----------------------------------------------------------------------
m4 <- ranger(estado ~ ., data = datos_trn, probability = TRUE, max.depth = 10)

# evaluar modelos ---------------------------------------------------------
datos <- datos |> 
  mutate(
    # como se ejecuta regresion no da 2 columnas
    pred_arb = partykit:::predict.party(m2, newdata = datos),
    pred_rl  = predict(m3, newdata = datos, type = "response"),
    pred_rf  = predict(m4, data = datos, type = "response")$predictions[,1]
  )

datos |>
  select(muestra, estado, starts_with("pred_")) |>
  # mutate(estado = as.numeric(estado == "bueno")) |>
  pivot_longer(cols = starts_with("pred"), names_to = "modelo", values_to = "pred") |>
  group_by(muestra, modelo) |>
  summarise(auc = Metrics::auc(estado, pred), .groups = "drop") |>
  ungroup() |>
  pivot_wider(names_from = muestra, values_from = auc) |> 
  arrange(desc(tst))


# vi ----------------------------------------------------------------------
datos_tst <- datos |>
  filter(muestra == "tst") |> 
  select(-starts_with("pred"), -muestra)

datos_tst

vi_2 <- variable_importance(m2, data = datos_tst, iterations = 10)
plot(vi_2)

vi_3 <- variable_importance(
  m3, 
  data = datos_tst, 
  iterations = 10,
  predict_function = function(m, d){  predict(m, newdata = d, type = "response") }
  )
plot(vi_3)

vi_4 <- variable_importance(
  m4, 
  response = "estado",
  data = datos_tst, 
  iterations = 10,
  predict_function = function(m, d){  predict(m, data = d, type = "response")$predictions[,1] }
)
plot(vi_4)

plot(vi_2, vi_3, vi_4) +
  scale_y_continuous(
    name = "1 - AUC",
    position = "right",
    limits = c(0.1, 0.4),
    sec.axis = sec_axis(
      ~ 1 - ., 
      labels = scales::percent, 
      name = "AUC",
      )
  ) 

