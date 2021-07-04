# first Draft
########### 
#install.packages('tidysynth')

AdCSV <- read.csv("~/Downloads/AdCSV.csv")
library(tidysynth)


Inflation.World.Bank <- read.csv("~/Downloads/Inflation World Bank - Sheet1.csv")
shock_year <- 2017
beggining_year <- min(AdCSV$Year)

AdCSV_Synth <-
  
  AdCSV %>% na.omit() %>%
  
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

AdCSV_Synth %>% plot_trends()


AdCSV_Synth %>% plot_differences()

AdCSV_Synth %>% plot_weights()

AdCSV_Synth %>% plot_placebos()


####### Inflation #########


beggining_year <- 1996
original_group <- c("Armenia", "Georgia", "Estonia", "Latvia", "Lithuania", "Moldova", "Albania")
neighbors <- c("Armenia", "Russia", "Turkey", "Syria", "Georgia", "Azerbaijan", "Iran", "Iraq")
#neighbors <- c("Armenia", "Russia", "Turkey", "Syria", "Georgia", "Azerbaijan", "Iran", "Iraq", 
#               "Russian Federation", "Syrian Arab Republic", "Iran, Islamic Rep.")



Inflation.World.Bank_Synth <-
  
  Inflation.World.Bank %>% filter(Country.Name %in% c(original_group)) %>% 
  filter(Year > beggining_year) %>% na.omit() %>%
  
  # initial the synthetic control object
  synthetic_control(outcome = value, # outcome
                    unit = Country.Name, # unit index in the panel data
                    time = Year, # time index in the panel data
                    i_unit = "Armenia", # unit where the intervention occurred
                    i_time = shock_year, # time period when the intervention occurred
                    generate_placebos=T # generate placebo synthetic controls (for inference)
  ) %>%
  
  # Generate the aggregate predictors used to fit the weights
  generate_predictor(time_window = beggining_year:shock_year,
                     Inflation = mean(value, na.rm = TRUE))  %>% 
  
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = beggining_year:shock_year, # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
  ) %>%
  
  # Generate the synthetic control
  generate_control()



Inflation.World.Bank_Synth %>% plot_trends()


Inflation.World.Bank_Synth %>% plot_differences()

Inflation.World.Bank_Synth %>% plot_weights()

Inflation.World.Bank_Synth %>% plot_placebos()



# https://www.r-project.org/nosvn/pandoc/WDI.html
# https://www.r-project.org/nosvn/pandoc/WDI.html

# > WDIsearch('gdp')[1:10,]
# indicator              name                                                                      
# [1,] "BG.GSR.NFSV.GD.ZS"    "Trade in services (% of GDP)"                                            
# [2,] "BM.KLT.DINV.GD.ZS"    "Foreign direct investment, net outflows (% of GDP)"                      
# [3,] "BN.CAB.XOKA.GD.ZS"    "Current account balance (% of GDP)"                                      
# [4,] "BN.CUR.GDPM.ZS"       "Current account balance excluding net official capital grants (% of GDP)"
# [5,] "BN.GSR.FCTY.CD.ZS"    "Net income (% of GDP)"                                                   
# [6,] "BN.KLT.DINV.CD.ZS"    "Foreign direct investment (% of GDP)"                                    
# [7,] "BN.KLT.PRVT.GD.ZS"    "Private capital flows, total (% of GDP)"                                 
# [8,] "BN.TRF.CURR.CD.ZS"    "Net current transfers (% of GDP)"                                        
# [9,] "BNCABFUNDCD_"         "Current Account Balance, %GDP"                                           
# [10,] "BX.KLT.DINV.WD.GD.ZS" "Foreign direct investment, net inflows (% of GDP)" 
# WDIsearch uses grep and ignores cases, so you can also use regular expressions. For instance, if you are looking for GDP per capita in constant dollars:
#   
#   WDIsearch('gdp.*capita.*constant')
# indicator           name                                                 
# [1,] "GDPPCKD"           "GDP per Capita, constant US$, millions"             
# [2,] "NY.GDP.PCAP.KD"    "GDP per capita (constant 2000 US$)"                 
# [3,] "NY.GDP.PCAP.KN"    "GDP per capita (constant LCU)"                      
# [4,] "NY.GDP.PCAP.PP.KD" "GDP per capita, PPP (constant 2005 international $)"


#install.packages('WDI')
#install.packages("sjlabelled")
library("WDI")
library("dplyr")
library("sjlabelled")
WDI_GDP_per_capita <- WDI(indicator="NY.GDP.PCAP.KD", country= "all", start=1994, end=2021)
names(WDI_GDP_per_capita) <- c("iso2c", "country", "GDP_per_capita_2010_USD", "year")
#wdi <- WDIbulk(timeout = 600)
#test2 <- WDIcache()

countries_of_interest = c()

countries_to_select <- WDI_GDP_per_capita %>% select(country) %>% unique()
subsetted_countries <- WDI_GDP_per_capita %>% filter(country %in% c("Armenia", countries_of_interest))

original_group <- c("Armenia", "Georgia", "Estonia", "Latvia", "Lithuania", "Moldova", "Albania")
neighbors <- c("Armenia", "Russia", "Turkey", "Syria", "Georgia", "Azerbaijan", "Iran", "Iraq", 
               "Russian Federation", #"Syrian Arab Republic", 
               "Iran, Islamic Rep.")

countries_in_range <- WDI_GDP_per_capita %>% dplyr::filter((year >= beggining_year) & (year <= end_year) )  %>% select(country,GDP_per_capita_2010_USD) %>% group_by(country) %>% 
  summarise_all(~sum(is.na(.))) %>% filter(GDP_per_capita_2010_USD == 0)

countries_in_range <- c("Albania", "Algeria", "Andorra", "Angola", 
                        "Argentina", "Armenia", "Australia", "Austria", 
                        "Azerbaijan",  "Bahrain", "Bangladesh", "Barbados", 
                        "Belarus", "Belgium", "Belize", "Benin", "Bermuda", "Bhutan", 
                        "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", 
                        "Bulgaria",  "Cambodia", 
                        "Cameroon", "Canada",  "Central African Republic", 
                        "Central Europe and the Baltics", "Chad", "Chile", "China", "Colombia", 
                        "Comoros", "Congo, Dem. Rep.", "Congo, Rep.", "Costa Rica", 
                        "Cyprus", "Czech Republic", "Denmark", "Dominica", "Dominican Republic", 
                        "Ecuador", "Egypt, Arab Rep.", 
                        "El Salvador", "Equatorial Guinea", "Estonia", "Eswatini", "Ethiopia", 
                        "Fiji", "Finland", 
                        "France", "Gabon", "Gambia, The", "Georgia", "Germany", "Ghana", 
                        "Greece", "Grenada", "Guatemala", "Guinea", "Guinea-Bissau", 
                        "Guyana", "Haiti",  "Honduras",  "Hungary", 
                        "Iceland", "India", "Indonesia", "Iran, Islamic Rep.", "Iraq", 
                        "Ireland", "Israel", "Italy", "Jamaica", "Japan", "Jordan", "Kazakhstan", 
                        "Kenya", "Kiribati", "Korea, Rep.", "Kyrgyz Republic",  
                        "Lebanon", "Lesotho", 
                        "Luxembourg", "Macao SAR, China", "Madagascar", "Malawi", "Malaysia", 
                        "Mali", "Malta", "Mauritania", "Mauritius", "Mexico",  "Mongolia", "Morocco", "Mozambique", "Myanmar", 
                        "Namibia", "Nepal", "Netherlands", "New Zealand", "Nicaragua", 
                        "Niger", "Nigeria", "North America", "North Macedonia", "Norway", 
                        "Oman", "Pacific island small states", "Pakistan", 
                        "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", 
                        "Poland", "Portugal", 
                        "Puerto Rico", "Romania", "Russian Federation", "Rwanda", "Samoa", 
                        "Saudi Arabia", "Senegal", "Seychelles", "Sierra Leone", "Singapore", 
                        "Slovak Republic", "Slovenia", "Solomon Islands", "South Africa", 
                        "Spain", "Sri Lanka", 
                        "Sweden", "Switzerland", "Tajikistan", "Tanzania", "Thailand", 
                        "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", 
                        "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", 
                        "United States", "Uruguay", "Uzbekistan", 
                        "Vanuatu", "Vietnam", "Yemen, Rep.", 
                        "Zambia", "Zimbabwe")

top_10 <- c("Armenia", "Ethiopia", "Congo, Dem. Rep.", "Sierra Leone", "Mozambique", "Central African Republic", 
            "Malawi", "Niger", "Madagascar", "Rwanda", "Nepal")

library(tidysynth)
library(dplyr)
df <- WDI_GDP_per_capita
#df$iso2c < as.factor(df$iso2c)
#df$country <- as.factor(df$country)
beggining_year = 1995
shock_year = 2017
end_year = 2019

Synth_Data <- df %>% dplyr::filter((country %in% countries_in_range) & (year <= end_year) ) %>%
  # initial the synthetic control object
  synthetic_control(outcome = GDP_per_capita_2010_USD, # outcome
                    unit = country, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "Armenia", # unit where the intervention occurred
                    i_time = shock_year, # time period when the intervention occurred
                    generate_placebos=T # generate placebo synthetic controls (for inference)
  ) %>%
  # Generate the aggregate predictors used to fit the weights
  generate_predictor(time_window = beggining_year:shock_year,
                     GDP_per_capita_2010_USD = mean(GDP_per_capita_2010_USD, na.rm = TRUE))  %>% 
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = beggining_year:shock_year, # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
  ) %>%
  # Generate the synthetic control
  generate_control()

# my_synth_func <- function(df, countries, beggining_year, shock_year){
#   # ind a way to include min year and year that happen
#   
#   
#   return(Synth_Data)
# }
# 
# 
# my_synth_func(WDI_GDP_per_capita, neighbors, 1994, 2018)

Synth_Data %>% plot_trends()


Synth_Data %>% plot_differences()

Synth_Data %>% plot_weights()


Synth_Data %>% plot_placebos()


