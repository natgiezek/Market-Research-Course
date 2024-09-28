rm(list = ls())
library(tidyverse)
library(apollo)
###load("/Users/nataliagiezek/Downloads/MMNL_modeChoice_normal_asc.RData")
library(readr)
apollo_modeChoiceData <- read_csv("Desktop/EIO/apollo_modeChoiceData.csv")
### Initialise code
{apollo_initialise()
### Set core controls
apollo_control = list(
  modelName       = "MNL_SP",
  modelDescr      = "Simple MNL model on mode choice SP data",
  indivID         = "ID",
  outputDirectory = "output"
)
database = apollo_modeChoiceData
database = subset(database, database$SP == 1)
### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(
  asc_car      = 0,
  asc_bus      = 0,
  asc_air      = 0,
  asc_rail     = 0,
  b_tt_car     = 0,
  b_tt_bus     = 0,
  b_tt_air     = 0,
  b_tt_rail    = 0,
  b_access     = 0,
  b_cost       = 0,
  b_no_frills  = 0,
  b_wifi       = 0,
  b_food       = 0
)
apollo_fixed = c("asc_car", "b_no_frills")
apollo_inputs = apollo_validateInputs()
apollo_probabilities = function(apollo_beta,
                                apollo_inputs,
                                functionality = "estimate") {
  ### "estimate": For model estimation, produces likelihood of the full model, at the level of individual decision-makers, after averaging across draws
  ### Attach inputs and detach after function exit - what does it do?
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  ### Create list of probabilities P
  P = list()
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["car"]]  = asc_car  + b_tt_car * time_car + b_cost * cost_car
  V[["bus"]]  = asc_bus  + b_tt_bus  * time_bus  + b_access * access_bus  + b_cost * cost_bus
  V[["air"]]  = asc_air  + b_tt_air  * time_air  + b_access * access_air  + b_cost * cost_air    + b_no_frills * (service_air == 1)  + b_wifi * (service_air == 2)  + b_food * (service_air == 3)
  V[["rail"]] = asc_rail + b_tt_rail * time_rail + b_access * access_rail + b_cost * cost_rail   + b_no_frills * (service_rail == 1) + b_wifi * (service_rail == 2) + b_food * (service_rail == 3)
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(
      car = 1,
      bus = 2,
      air = 3,
      rail = 4
    ),
    avail         = list(
      car = av_car,
      bus = av_bus,
      air = av_air,
      rail = av_rail
    ),
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
apollo_inputs = apollo_validateInputs()
model_MNL_SP = apollo_estimate(apollo_beta,
                               apollo_fixed,
                               apollo_probabilities,
                               apollo_inputs) 
modelOutput_settings <- list(printPVal = 2, printClassical = FALSE)
apollo_saveOutput(model_MNL_SP) 
}
estimates_MNL_SP <- as.list(model_MNL_SP$estimate)
database_Estimates_Util_Prob <- database %>%
  as_tibble() %>%
  bind_cols(estimates_MNL_SP %>% as_tibble_row()) %>% # adds all the estimated coefficients in every row
  mutate(# calculate observed utilities, same utilities as in the model specification
    V_car  = asc_car  + b_tt_car  * time_car  + b_cost * cost_car,
    V_bus  = asc_bus  + b_tt_bus  * time_bus  + b_access * access_bus  + b_cost * cost_bus,
    V_air  = asc_air  + b_tt_air  * time_air  + b_access * access_air  + b_cost * cost_air + 
      b_no_frills * (service_air == 1)  + b_wifi * (service_air == 2)  + b_food * (service_air == 3),
    V_rail = asc_rail + b_tt_rail * time_rail + b_access * access_rail + b_cost * cost_rail +
      b_no_frills * (service_rail == 1) + b_wifi * (service_rail == 2) + b_food * (service_rail == 3)
  ) %>%
  mutate(# make sure that utility is removed where alternative is not available
    V_car = ifelse(av_car, V_car, NA),
    V_bus = ifelse(av_bus, V_bus, NA),
    V_air = ifelse(av_air, V_air, NA),
    V_rail = ifelse(av_rail, V_rail, NA)
  ) %>% 
  left_join(# add choice probabilities
    apollo_prediction(model_MNL_SP, apollo_probabilities, apollo_inputs) %>% select(-chosen) %>% rename_with( ~paste0("prob_", .x)),
    by = c("ID" = "prob_ID", "SP_task" = "prob_Observation")
  )
database_Estimates_Util_Prob %>%
  rowwise() %>% # such that sum() gives the desired result
  mutate(CS = 1 / (-b_cost) * log(sum(
    exp(V_car), exp(V_bus), exp(V_air), exp(V_rail), na.rm = TRUE
  ))) %>%
  select(starts_with("time"), starts_with("cost"), CS)
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "MNL_SP_covariates",
  modelDescr      = "MNL model with socio-demographics on mode choice SP data",
  indivID         = "ID", 
  outputDirectory = "output"
)

### Loading data from package
database = apollo_modeChoiceData

### Use only SP data
database = subset(database,database$SP==1)

### Create new variable with average income
database$mean_income = mean(database$income)

### Define settings for analysis of choice data to be conducted prior to model estimation
choiceAnalysis_settings <- list(
  alternatives = c(car=1, bus=2, air=3, rail=4),
  avail        = list(
    car = database$av_car,
    bus = database$av_bus,
    air = database$av_air,
    rail = database$av_rail
  ), 
  choiceVar    = database$choice,
  explanators  = database[,c("female","business","income")],
  rows         = (database$SP==1)
)

### Run function to analyse choice data
apollo_choiceAnalysis(choiceAnalysis_settings, apollo_control, database)

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  ### Create alternative specific constants and coefficients using interactions with socio-demographics
  asc_bus_value   = asc_bus  + asc_bus_shift_female * female
  asc_air_value   = asc_air  + asc_air_shift_female * female
  asc_rail_value  = asc_rail + asc_rail_shift_female * female
  b_tt_car_value  = b_tt_car + b_tt_shift_business * business
  b_tt_bus_value  = b_tt_bus + b_tt_shift_business * business
  b_tt_air_value  = b_tt_air + b_tt_shift_business * business
  b_tt_rail_value = b_tt_rail + b_tt_shift_business * business
  b_cost_value    = ( b_cost +  b_cost_shift_business * business ) * ( income / mean_income ) ^ cost_income_elast
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["car"]]  = asc_car        + b_tt_car_value  * time_car                           + b_cost_value * cost_car
  V[["bus"]]  = asc_bus_value  + b_tt_bus_value  * time_bus  + b_access * access_bus  + b_cost_value * cost_bus 
  V[["air"]]  = asc_air_value  + b_tt_air_value  * time_air  + b_access * access_air  + b_cost_value * cost_air   + b_no_frills * ( service_air == 1 )  + b_wifi * ( service_air == 2 )  + b_food * ( service_air == 3 )
  V[["rail"]] = asc_rail_value + b_tt_rail_value * time_rail + b_access * access_rail + b_cost_value * cost_rail  + b_no_frills * ( service_rail == 1 ) + b_wifi * ( service_rail == 2 ) + b_food * ( service_rail == 3 )
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives = c(car=1, bus=2, air=3, rail=4),
    avail        = list(car=av_car, bus=av_bus, air=av_air, rail=av_rail),
    choiceVar    = choice,
    utilities    = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

### Vector of parameters, including
### any that are kept fixed in estimation
apollo_beta = c(
  asc_car               = 0,
  asc_bus               = 0,
  asc_air               = 0,
  asc_rail              = 0,
  asc_bus_shift_female  = 0,
  asc_air_shift_female  = 0,
  asc_rail_shift_female = 0,
  b_tt_car              = 0,
  b_tt_bus              = 0,
  b_tt_air              = 0,
  b_tt_rail             = 0,
  b_tt_shift_business   = 0,
  b_access              = 0,
  b_cost                = 0,
  b_cost_shift_business = 0,
  cost_income_elast     = 0,
  b_no_frills           = 0,
  b_wifi                = 0,
  b_food                = 0
)

### Vector with names (in quotes) of parameters to be kept fixed
### at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_car", "b_no_frills")

### Read in starting values for at least some
### parameters from existing model output file
apollo_beta = apollo_readBeta(
  apollo_beta,
  apollo_fixed,
  "MNL_SP",
  overwriteFixed = FALSE
)

apollo_inputs = apollo_validateInputs()

model_SP_covariates = apollo_estimate(apollo_beta, apollo_fixed,
                                      apollo_probabilities, apollo_inputs)

apollo_saveOutput(model_SP_covariates)

modelOutput_settings <- list(printClassical = FALSE)
apollo_modelOutput(model_SP_covariates, modelOutput_settings)

apollo_lrTest("MNL_SP", model_SP_covariates) 
###Calculates the likelihood ratio test value between two models and reports the corresponding p-value

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "NL_two_levels",
  modelDescr      = "Two-level NL model with socio-demographics
                      on mode choice SP data",
  indivID         = "ID", 
  outputDirectory = "output"
)

apollo_probabilities = function(apollo_beta,
                                apollo_inputs,
                                functionality = "estimate") {
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### Create alternative specific constants and coefficients using interactions with socio-demographics
  asc_bus_value   = asc_bus  + asc_bus_shift_female * female
  asc_air_value   = asc_air  + asc_air_shift_female * female
  asc_rail_value  = asc_rail + asc_rail_shift_female * female
  b_tt_car_value  = b_tt_car + b_tt_shift_business * business
  b_tt_bus_value  = b_tt_bus + b_tt_shift_business * business
  b_tt_air_value  = b_tt_air + b_tt_shift_business * business
  b_tt_rail_value = b_tt_rail + b_tt_shift_business * business
  b_cost_value    = (b_cost +  b_cost_shift_business * business) * (income / mean_income) ^ cost_income_elast
  
  ### List of utilities: these must use the same names as in nl_settings, order is irrelevant
  V = list()
  V[["car"]]  = asc_car        + b_tt_car_value  * time_car                           + b_cost_value * cost_car
  V[["bus"]]  = asc_bus_value  + b_tt_bus_value  * time_bus  + b_access * access_bus  + b_cost_value * cost_bus
  V[["air"]]  = asc_air_value  + b_tt_air_value  * time_air  + b_access * access_air  + b_cost_value * cost_air   + b_no_frills * (service_air == 1)  + b_wifi * (service_air == 2)  + b_food * (service_air == 3)
  V[["rail"]] = asc_rail_value + b_tt_rail_value * time_rail + b_access * access_rail + b_cost_value * cost_rail  + b_no_frills * (service_rail == 1) + b_wifi * (service_rail == 2) + b_food * (service_rail == 3)
  
  ### Specify nests for NL model
  nlNests      = list(root = 1, PT = lambda_PT)
  
  ### Specify tree structure for NL model
  nlStructure = list()
  nlStructure[["root"]]   = c("car", "PT")
  nlStructure[["PT"]]     = c("bus", "air", "rail")
  
  ### Define settings for NL model
  nl_settings <- list(
    alternatives = c(
      car = 1,
      bus = 2,
      air = 3,
      rail = 4
    ),
    avail        = list(
      car = av_car,
      bus = av_bus,
      air = av_air,
      rail = av_rail
    ),
    choiceVar    = choice,
    utilities    = V,
    nlNests      = nlNests,
    nlStructure  = nlStructure
  )
  
  ### Compute probabilities using NL model
  P[["model"]] = apollo_nl(nl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

### Vector of parameters, including
### any that are kept fixed in estimation
apollo_beta = c(
  asc_car               = 0,
  asc_bus               = 0,
  asc_air               = 0,
  asc_rail              = 0,
  asc_bus_shift_female  = 0,
  asc_air_shift_female  = 0,
  asc_rail_shift_female = 0,
  b_tt_car              = 0,
  b_tt_bus              = 0,
  b_tt_air              = 0,
  b_tt_rail             = 0,
  b_tt_shift_business   = 0,
  b_access              = 0,
  b_cost                = 0,
  b_cost_shift_business = 0,
  cost_income_elast     = 0,
  b_no_frills           = 0,
  b_wifi                = 0,
  b_food                = 0,
  lambda_PT             = 1
)

### Vector with names (in quotes) of parameters to be kept fixed
### at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_car", "b_no_frills")

### Read in starting values for at least some
### parameters from existing model output file
apollo_beta = apollo_readBeta(
  apollo_beta,
  apollo_fixed,
  "MNL_SP_covariates",
  overwriteFixed = FALSE
)

apollo_inputs = apollo_validateInputs()

model_NL_two_levels = apollo_estimate(apollo_beta,
                                      apollo_fixed,
                                      apollo_probabilities,
                                      apollo_inputs)

apollo_saveOutput(model_NL_two_levels)

apollo_modelOutput(model_NL_two_levels, modelOutput_settings)

apollo_lrTest(model_SP_covariates,
              model_NL_two_levels)

predictions_base = apollo_prediction(model_NL_two_levels,
                                     apollo_probabilities,
                                     apollo_inputs)

### Now imagine the cost for rail increases by 1%
database$cost_rail = 1.01 * database$cost_rail

### Rerun predictions with the new data
apollo_inputs = apollo_validateInputs()
predictions_rail = apollo_prediction(model_NL_two_levels,
                                     apollo_probabilities,
                                     apollo_inputs)

###Return to original data
database$cost_rail = 1 / 1.01 * database$cost_rail

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
  slice(sample(nrow(.), size = 10)) #Show randomly 10 observations

predictions_list <-
  list() #initialize list for counterfactual predictions

for (mode in c("car", "bus", "air", "rail")) {
  ### Now imagine the cost for mode increases by 1%
  database[paste0("cost_", mode)] = 1.01 * database[paste0("cost_", mode)]
  
  ### Rerun predictions with the new data
  apollo_inputs = apollo_validateInputs()
  predictions_list[[mode]] = apollo_prediction(model_NL_two_levels,
                                               apollo_probabilities,
                                               apollo_inputs)
  
  ###Return to original data
  database[paste0("cost_", mode)] = 1 / 1.01 * database[paste0("cost_", mode)]
}

elasticities <- predictions_list %>%
  map( # map() applies the following to every element in the list
    ~ as_tibble(.x) %>%
      left_join(
        .,
        predictions_base,
        by = c("ID" = "ID", "Observation" = "Observation")
      ) %>%
      mutate(
        elas_car = log(car.x / car.y) / log(1.01),
        elas_bus = log(bus.x / bus.y) / log(1.01),
        elas_air = log(air.x / air.y) / log(1.01),
        elas_rail = log(rail.x / rail.y) / log(1.01)
      ) %>%
      select(starts_with("elas_"))
  )

elasticities_aggregated <- elasticities %>%
  map(~ drop_na(.x) %>% # only look at obs where all alts avail
        summarise(across(.fns = mean))) %>%
  unlist() %>% # creates named vector
  tibble(name = names(.), # names of vector
         value = .) %>% # values of vector
  separate(name, into = c("Price_Increase", "elas", "Prob_Increase")) %>%
  select(-elas) %>%
  mutate(across(ends_with("Increase"), ~ as_factor(.x)))
# as_factor() keeps the order of alternatives for the plot
# Otherwise they would be ordered alphabetically

matrixplot_elasticities <-
  elasticities_aggregated %>% ggplot(aes(y = Prob_Increase, x = Price_Increase)) +
  geom_raster(aes(fill = value)) +
  geom_label(
    aes(label = round(value, 2)),
    fill = "white",
    label.size = 0,
    label.padding = unit(0.5, "lines"),
    fontface = "bold"
  ) +
  scale_fill_gradient2() +
  labs(
    title = "Price Elasticities of Demand",
    subtitle = "A Price Increase of 1% for x leads to a ... % Increase of Market Share for y.",
    x = "",
    y = "",
    caption = "Only considering Observations with all Modes available."
  ) +
  theme(legend.position = "none")

matrixplot_elasticities

