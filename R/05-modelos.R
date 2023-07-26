library("DALEX")
library("randomForest")


titanic_imputed <- archivist::aread("pbiecek/models/27e5c")

titanic_rf <- archivist::aread("pbiecek/models/4e0fc")

explainer_rf <- DALEX::explain(
  model = titanic_rf,  
  data = titanic_imputed[, -9],
  y = titanic_imputed$survived, 
  label = "Random Forest"
  )

pdp_rf <- model_profile(explainer = explainer_rf, variables = "age")
pdp_rf

plot(pdp_rf)

library("ggplot2")
plot(pdp_rf) + 
  ggtitle("Partial-dependence profile for age") 

plot(pdp_rf, geom = "profiles") + 
  ggtitle("Ceteris-paribus and partial-dependence profiles for age") 

pdp_rf_clust <- model_profile(explainer = explainer_rf,  variables = "age", k = 3)

plot(pdp_rf_clust)

plot(pdp_rf_clust, geom = "profiles") + 
  ggtitle("Clustered partial-dependence profiles for age") 



# otro --------------------------------------------------------------------
library(ranger)

data_cred <- tibble(datos::datos_credito)
data_cred$Estado <- as.numeric(data_cred$Estado) - 1
data_cred$Activos <- log(data_cred$Activos + 10)

data_cred <- data_cred |> 
  filter(complete.cases(data_cred))

glimpse(data_cred)

mod <- randomForest::randomForest(Estado ~ ., data = data_cred)

# c <- celavi::variable_importance(mod, data = data_cred, iterations = 10)
# plot(c)

# explainer
explainer_rf <- DALEX::explain(
  model = mod,  
  data = data_cred[, -1],
  y = data_cred$Estado, 
  label = "Random Forest Cred"
)

mp <- model_parts(
  explainer = explainer_rf, 
  # loss_function = loss_accuracy,
  B = 10,
  variables = colnames(explainer_rf$data)
  )

plot(mp)


# antiguedad
pdp_rf_ant <- model_profile(explainer = explainer_rf, variables = "Antiguedad")
pdp_rf_ant

plot(pdp_rf_ant) 

ggplot(data_cred, aes(Antiguedad, Estado)) +
  geom_point() +
  geom_smooth(aes(group = 1))

ggplot(data_cred, aes(Antiguedad, Estado)) +
  geom_jitter(width = 0.5, height = 0.2, alpha = 0.05) +
  geom_smooth(aes(group = 1))


# edad
pdp_rf_edad <- model_profile(explainer = explainer_rf, variables = "Edad")
pdp_rf_edad

plot(pdp_rf_edad)  +
  ylim(0, 1)

ggplot(data_cred, aes(Edad, Estado)) +
  geom_point() +
  geom_smooth(aes(group = 1))


# deuda
pdp_rf_dda <- model_profile(explainer = explainer_rf, variables = "Deuda")
pdp_rf_dda

plot(pdp_rf_dda)  +
  ylim(0, 1)

ggplot(data_cred, aes(Deuda, Estado)) +
  geom_point() +
  geom_smooth(aes(group = 1))


# activos
pdp_rf_act <- model_profile(explainer = explainer_rf, variables = "Activos")
pdp_rf_act

plot(pdp_rf_act)  +
  ylim(0, 1)

ggplot(data_cred, aes(Activos, Estado)) +
  geom_point() +
  geom_smooth(aes(group = 1)) 
