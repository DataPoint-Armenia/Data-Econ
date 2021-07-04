
# AdCsv --> Debt

######## Loading in the data
AdCSV <- read.csv("~/Desktop/FormSpotter/Data-Econ/Synthetic_Control/Synthetic_Control_Data/AdCSV.csv")
Tourism <- read.csv("~/Desktop/FormSpotter/Data-Econ/Synthetic_Control/Synthetic_Control_Data/ArmTourismCsv.csv")

### Schema Exploration
library(dplyr)
str(AdCSV)
str(Tourism)

## Spectrum
library(dplyr)
AdCSV %>% group_by(State) %>% tally()  
AdCSV %>% group_by(State) %>% tally() %>% group_by(n, State) %>% tally() 
#One case where only 17, Burma, probably either maximum or minimum


Tourism %>% group_by(State) %>% tally()  
View(Tourism %>% group_by(State) %>% tally() %>% group_by(n, State) %>% tally() )
#One case where only 36, Bahrain,

# Removing the unusual ones
Tourism2 <- Tourism %>% filter(State != "Bahrain")
AdCSV2 <- AdCSV %>% filter(State != "Burma")
AdCSV2$Year <- as.numeric(AdCSV2$Year)

## Visualization

Tourism2 %>% select(State) %>% distinct() 
# Well its clear which ones were added

AdCSV2 %>% select(State) %>% distinct() 
# Well its clear which ones were added

countries_want_visualized_Tourism <- c("Arm", "Afghanistan", "Egypt", "Congo", "Cyprus")
countries_want_visualized_AdCSV<- c("Arm", "Afghanistan", "Egypt", "Congo", "Cyprus")

library(ggplot2)

Tourism2 %>% filter(State %in% countries_want_visualized_Tourism) %>% ggplot(aes(x=Year,y=Y,colour=State,group=State)) + geom_line()

AdCSV2 %>% filter(State %in% countries_want_visualized_AdCSV) %>% ggplot(aes(x=Year,y=Y,colour=State,group=State)) + geom_line()

#### Running Synthetic Control
Tourism2$State <- as.factor(Tourism2$State)
library(tidysynth)
beggining_year <- 2002
shock_year <- 2016
Tourism2 %>% 
  
  # initial the synthetic control object
  synthetic_control(outcome = Y, # outcome
                    unit = State, # unit index in the panel data
                    time = Year, # time index in the panel data
                    i_unit = "Arm", # unit where the intervention occurred
                    i_time = shock_year, # time period when the intervention occurred
                    generate_placebos=T # generate placebo synthetic controls (for inference)
  ) %>%
  
  # Generate the aggregate predictors used to fit the weights
  generate_predictor(time_window = beggining_year:shock_year,
                     Y = mean(Y, na.rm = TRUE))  %>% 
  
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = beggining_year:shock_year, # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
  ) %>%
  
  # Generate the synthetic control
  generate_control()






AdCSV2 %>% 
  
  # initial the synthetic control object
  synthetic_control(outcome = Y, # outcome
                    unit = State, # unit index in the panel data
                    time = Year, # time index in the panel data
                    i_unit = "Arm", # unit where the intervention occurred
                    i_time = shock_year, # time period when the intervention occurred
                    generate_placebos=T # generate placebo synthetic controls (for inference)
  ) %>%
  
  # Generate the aggregate predictors used to fit the weights
  generate_predictor(time_window = beggining_year:shock_year,
                     Y = mean(Y, na.rm = TRUE))  %>% 
  
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = beggining_year:shock_year, # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
  ) %>%
  
  # Generate the synthetic control
  generate_control()

















