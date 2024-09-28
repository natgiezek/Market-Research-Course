rm(list = ls())
library(tidyverse)
library(apollo)
###load("/Users/nataliagiezek/Downloads/MMNL_modeChoice_normal_asc.RData")
library(readr)
apollo_modeChoiceData <- read_csv("Desktop/EIO/apollo_modeChoiceData.csv")
as_tibble(apollo_modeChoiceData) # doesn't change anything

as_tibble(apollo_modeChoiceData) %>%
  filter(RP == 1) %>% 
  select(-starts_with("SP"), -starts_with("service")) #service only for SP

as_tibble(apollo_modeChoiceData) %>%
  filter(RP == 1) %>%
  group_by(choice) %>%
  summarise(n = n()) %>%
  mutate(marketshare = (n / sum(n)) * 100)

### Initialise code
apollo_initialise()
### Set core controls
apollo_control = list(
  modelName       = "MNL_RP",
  modelDescr      = "Simple MNL model on mode choice RP data",
  indivID         = "ID", # The identifyer column in the data.
  outputDirectory = "output" # Folder to store output in.
)
database = apollo_modeChoiceData
### Use only RP data
database = subset(database,database$RP==1)

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["car"]]  = asc_car  + b_tt_car  * time_car                           + b_cost * cost_car
  V[["bus"]]  = asc_bus  + b_tt_bus  * time_bus  + b_access * access_bus  + b_cost * cost_bus 
  V[["air"]]  = asc_air  + b_tt_air  * time_air  + b_access * access_air  + b_cost * cost_air   
  V[["rail"]] = asc_rail + b_tt_rail * time_rail + b_access * access_rail + b_cost * cost_rail  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(car=1, bus=2, air=3, rail=4), 
    avail         = list(car=av_car, bus=av_bus, air=av_air, rail=av_rail), 
    choiceVar     = choice,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}
### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_car   = 0,
              asc_bus   = 0,
              asc_air   = 0,
              asc_rail  = 0,
              b_tt_car  = 0,
              b_tt_bus  = 0,
              b_tt_air  = 0,
              b_tt_rail = 0,
              b_access  = 0,
              b_cost    = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their
### starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_car")

apollo_inputs = apollo_validateInputs()

model = apollo_estimate(apollo_beta,
                        apollo_fixed,
                        apollo_probabilities,
                        apollo_inputs)

modelOutput_settings <- list(printPVal = 2, printClassical = FALSE)
apollo_modelOutput(model, modelOutput_settings)

apollo_saveOutput(model, modelOutput_settings)

b_cost_hat <- model$estimate[names(model$estimate) == "b_cost"]
- model$estimate / b_cost_hat

deltaMethod_settings = list(
  expression = c(
    WTP_car_min = "b_tt_car/b_cost",
    WTP_car_hour = "60*b_tt_car/b_cost",
    diff_WTP_bus_car = "b_tt_bus/b_cost - b_tt_car/b_cost"
  )
)
apollo_deltaMethod(model, deltaMethod_settings)

predictions_base = apollo_prediction(model,
                                     apollo_probabilities,
                                     apollo_inputs)

as_tibble(predictions_base)

b_access_hat <- model$estimate[names(model$estimate) == "b_access"]

me_access_formula <- as_tibble(predictions_base) %>%
  left_join(., database, by = c("ID" = "ID", "Observation" = "RP_journey")) %>%
  mutate(
    across(bus:rail, #There is no access time for car.
           ~ b_access_hat * .x * (1 - .x), # .x = choice probs
           .names = "meAccess_{.col}")
  )

me_access_formula %>%
  select(car:rail, starts_with("meAccess")) %>%
  slice(55:60)

me_access_formula %>%
  filter((av_car + av_bus + av_air + av_rail) == 4) %>% 
  select(car:rail, starts_with("meAccess")) %>%
  summarise(across(everything(), mean))

as_tibble(predictions_base) %>%
  left_join(., database, by = c("ID" = "ID", "Observation" = "RP_journey")) %>% 
  filter((av_car + av_bus + av_air + av_rail) == 4) %>%
  summarise(across(everything(), mean)) %>% 
  mutate(
    across(bus:rail, #There is no access time for car.
           ~ b_access_hat * .x * (1 - .x), # .x = choice probs
           .names = "meAccess_{.col}")
  ) %>% 
  select(car:rail, starts_with("meAccess"))

database_mean <- database %>% 
  filter((av_car + av_bus + av_air + av_rail) == 4) %>%
  mutate(across(time_car:last_col(), mean))

apollo_inputs_mean = apollo_validateInputs(database = database_mean)
predictions_mean = apollo_prediction(model,
                                     apollo_probabilities,
                                     apollo_inputs_mean)

as_tibble(predictions_mean) %>%
  mutate(
    across(bus:rail, #There is no access time for car.
           ~ b_access_hat * .x * (1 - .x), # .x = choice probs
           .names = "meAccess_{.col}")
  ) %>% 
  select(car:rail, starts_with("meAccess")) %>% 
  slice(1)

### Now imagine the access time for rail increases by 1 unit
database$access_rail = 1 + database$access_rail

### Rerun predictions with the new data
apollo_inputs = apollo_validateInputs()
predictions_access_rail = apollo_prediction(model,
                                            apollo_probabilities,
                                            apollo_inputs)

###Return to original data
database$access_rail = database$access_rail - 1

as_tibble(predictions_access_rail)

me_rail_approx <- as_tibble(predictions_access_rail) %>%
  left_join(.,
            predictions_base,
            by = c("ID" = "ID", "Observation" = "Observation")) %>%
  select(starts_with("rail")) %>% 
  mutate(me_rail_access_ap = rail.x - rail.y)

me_rail_approx %>%
  slice(55:60) #For 57 and 58, rail is not available.

me_rail_approx %>% 
  filter(me_rail_access_ap != 0) %>%
  ggplot() +
  geom_density(aes(me_rail_access_ap)) +
  geom_density(data = me_access_formula %>% filter(av_rail == 1),
               aes(meAccess_rail),
               color = "green") +
  labs(x = "Marginal Effect of Access Time on Choice Prob. of Rail")

elas_rail_formula <- as_tibble(predictions_base) %>% 
  left_join(., database, by = c("ID" = "ID", "Observation" = "RP_journey")) %>% 
  mutate(
    own_elas_rail = b_cost_hat * cost_rail * (1 - rail),
    cross_elas_rail = -b_cost_hat * cost_rail * rail
  )

elas_rail_formula %>%
  select(car:rail, starts_with("cost_"),
         own_elas_rail, cross_elas_rail) %>%
  slice(55:60) #For 57 and 58, rail is not available, get 0 elasticity.

elas_rail_formula %>%
  summarise(
    own_elas_rail = mean(own_elas_rail),
    cross_elas_rail = mean(cross_elas_rail)
  )

elas_rail_formula %>%
  filter(av_rail == 1) %>%
  summarise(
    own_elas_rail = mean(own_elas_rail),
    cross_elas_rail = mean(cross_elas_rail)
  )

elas_rail_formula %>% 
  filter(av_rail == 1) %>% 
  ggplot(aes(x = cost_rail, y = own_elas_rail)) +
  geom_point() +
  geom_smooth()

### Now imagine the cost for rail increases by 1%
database$cost_rail = 1.01 * database$cost_rail

### Rerun predictions with the new data
apollo_inputs = apollo_validateInputs()
predictions_rail = apollo_prediction(model,
                                     apollo_probabilities,
                                     apollo_inputs)

###Return to original data
database$cost_rail = 1 / 1.01 * database$cost_rail

as_tibble(predictions_rail)

elas_rail_approx <- as_tibble(predictions_rail) %>%
  left_join(.,
            predictions_base,
            by = c("ID" = "ID", "Observation" = "Observation")) %>%
  mutate(
    own_elas_rail_ap = log(rail.x / rail.y) / log(1.01),
    elas_rail_car_ap = log(car.x / car.y) / log(1.01),
    elas_rail_bus_ap = log(bus.x / bus.y) / log(1.01),
    elas_rail_air_ap = log(air.x / air.y) / log(1.01)
  )

elas_rail_approx %>% select(ends_with("_ap"))%>%
  slice(55:64) #For 57 and 58, rail is not available.

elas_rail_approx %>% 
  drop_na(own_elas_rail_ap) %>% # Get NaN where rail is not available, remove.
  ggplot() +
  geom_density(aes(own_elas_rail_ap)) +
  geom_density(data = elas_rail_formula %>% filter(av_rail == 1),
               aes(own_elas_rail),
               color = "green") +
  labs(x = "Own Price Elasticity")

### Set core controls
apollo_control = list(
  modelName       = "MNL_RP_commonTtCoef",
  modelDescr      = "Simple MNL model on mode choice RP data
                      with common travel time coefficients",
  indivID         = "ID", 
  outputDirectory = "output"
)

apollo_probabilities_commonTtCoef = function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["car"]] = asc_car + b_tt * time_car + b_cost * cost_car
  V[["bus"]] = asc_bus + b_tt * time_bus + b_access * access_bus + b_cost * cost_bus
  V[["air"]] = asc_air + b_tt * time_air + b_access * access_air + b_cost * cost_air
  V[["rail"]] = asc_rail + b_tt * time_rail + b_access * access_rail + b_cost * cost_rail
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(car=1, bus=2, air=3, rail=4), 
    avail         = list(car=av_car, bus=av_bus, air=av_air, rail=av_rail), 
    choiceVar     = choice,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

apollo_beta_commonTtCoef = c(
  asc_car   = 0,
  asc_bus   = 0,
  asc_air   = 0,
  asc_rail  = 0,
  b_tt      = 0,
  b_access  = 0,
  b_cost    = 0
)

apollo_inputs_commonTtCoef = apollo_validateInputs(
  apollo_beta = apollo_beta_commonTtCoef
)

model_commonTtCoef = apollo_estimate(
  apollo_beta_commonTtCoef,
  apollo_fixed,
  apollo_probabilities_commonTtCoef,
  apollo_inputs_commonTtCoef
)

apollo_saveOutput(model_commonTtCoef, modelOutput_settings)

apollo_lrTest(model_commonTtCoef, model)

