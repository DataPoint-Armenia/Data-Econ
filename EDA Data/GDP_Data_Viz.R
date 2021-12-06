
#######
## Copyright (R) DataPoint Armenia
## Authors: Gary Vartanian, Nanneh Chehras, Lusine Khachatryan, and Ruben Dermoyan
######


###
# So this started with an interest in trying to find out the economic variables
# of armenia and how is it doing
# this serves as tricky, as there is not an easy way to compare and pull up
# data on this region. 

# the best organization that we were able to find that can provide a neat 
# arsenal of economic data and statistics was the world bank. Luckily, 
# they have an API that allowed for the easy fetching of this data. 
# it is with this availability that we want to examine the economic data of 
# Armenia and its neighbors and find out the current trajctory and also comparison. 

# For reference the site we will use is: #https://data.worldbank.org/indicator/NY.GDP.MKTP.CD

## Loading up the packages 
# First thing is to load up the packages that we are going to use
# How does Armenia compare economically with the rest of its neighbors?

# packages
library("WDI")
library("dplyr")
library("sjlabelled")

# In this case the Data Pulling is gdp per capita but that can be applied to any case

### using API to pull data
WDI_GDP_per_capita <- WDI(indicator="NY.GDP.PCAP.KD", country= "all", start=1994, end=2021)
# renaming variables appropriately
names(WDI_GDP_per_capita) <- c("iso2c", "country", "GDP_per_capita_2010_USD", "year")

# identifying Armenia's neighbors
neighbors <- c("Armenia", "Russia", "Turkey", "Syria", "Georgia", "Azerbaijan", "Iran", #"Iraq", 
               "Russian Federation", #"Syrian Arab Republic", 
               "Iran, Islamic Rep.")
# filtering for only Armenia's neighbors
subsetted_countries <- WDI_GDP_per_capita %>% filter(country %in% neighbors)

#Cleaning up some of the names
subsetted_countries[subsetted_countries=="Iran, Islamic Rep."]<-"Iran"
subsetted_countries[subsetted_countries=="Russian Federation"]<-"Russia"

# Start Visualizaing 
library(ggplot2)

ggplot(data=subsetted_countries %>% filter(country == "Armenia"), aes(x=year, y=GDP_per_capita_2010_USD, color=country)) +
  geom_line()+
  geom_point() + 
  ggtitle("GDP per capita of Armenia and its Neighbors in 2010 USD dollars") + 
  scale_y_continuous(labels = scales::dollar_format()) + 
  labs(y="GDP Per capita in 2010 USD")

# Looking at a plot of Armenia by itself, 
# you think it looks good, you can see the growth rate that has happened. 
# An acceleration from 2000-2008, then crash of 08 hit it hard

# After a year of loss, the economy started to recover, you can see a some stagnation in 2015-2016
# but then strong growth until covid year

#So looking at the plots are fun, but its usually better to have something to compare it to. 
#How does Armenia compare against its neighbors. 

ggplot(data=subsetted_countries, aes(x=year, y=GDP_per_capita_2010_USD, color=country)) +
  geom_line()+
  geom_point() + 
  ggtitle("GDP per capita of Armenia and its Neighbors in 2010 USD dollars") + 
  scale_y_continuous(labels = scales::dollar_format()) + 
  labs(y="GDP Per capita in 2010 USD")



# Well comparitively to its neighbors it seems pretty outclassed
# The closest economically it resembles Georgia in total value

#ToDo add other comparison countries here

# Well what about in growth? how Does that look like?

library(dplyr)
subsetted_countries <- subsetted_countries %>% arrange(year, country)
# https://stackoverflow.com/questions/1296646/how-to-sort-a-dataframe-by-multiple-columns

pct <- function(x) {x/lag(x) - 1}
# https://stackoverflow.com/questions/7145826/how-to-format-a-number-as-percentage-in-r
library('scales')

subsetted_countries2 <- subsetted_countries %>% group_by(country) %>% mutate_each(funs(pct), c(GDP_per_capita_2010_USD))
names(subsetted_countries2)[3] <- "GDP_per_capita_pct_change"
#https://stackoverflow.com/questions/31352685/how-can-i-calculate-the-percentage-change-within-a-group-for-multiple-columns-in

ggplot(data=subsetted_countries2 %>% filter(country == "Armenia"), aes(x=year, y=GDP_per_capita_pct_change, color=country)) +
  geom_line()+
  geom_point() + scale_y_continuous(labels = function(x) paste0(x*100, "%")) + 
  ggtitle("% change for GDP per capita of Armenia and its Neighbors") + 
  labs(y="% change in GDP Per capita")

### so interestingly this does a good job of highlighting the growth rate. 
### you can see that the fastest growth rate economically happened during the 2000's. 
### It was achieving growth rate of 10-15%
### now it seems between 0-7.5% steady growth rate. 


### how does it compare with its neighbors?
### try to look for the top most line?
### you can see Armenia being on the higher side. 


ggplot(data=subsetted_countries2, aes(x=year, y=GDP_per_capita_pct_change, color=country)) +
  geom_line()+
  geom_point() + scale_y_continuous(labels = function(x) paste0(x*100, "%")) + 
  ggtitle("% change for GDP per capita of Armenia and its Neighbors") + 
  labs(y="% change in GDP Per capita")

### Armenia is having quite a fast growth even compared to its neighbors

#ToDo add other comparison countries here



library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(viridis)


# Boxplot
p <- subsetted_countries2%>%
  ggplot( aes(x=country, y=GDP_per_capita_pct_change, colour=country, fill=country)) +
  geom_boxplot() + 
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +  # This switch X and Y axis and allows to get the horizontal version
  coord_flip() 

p
# Maybe a 5 number summary statistic would be helpful
tapply(subsetted_countries2$GDP_per_capita_pct_change, subsetted_countries2$country, summary)





new_data <- xtabs(GDP_per_capita_pct_change ~ year + country, data = subsetted_countries2)
new_data2 <- as.data.frame.matrix(new_data)


library(reshape)
cast(dat1, name ~ numbers)



#### Regression Against Other Neighboring Economies
library(PerformanceAnalytics)
chart.Correlation(new_data)

library(GGally)
my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    #geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", se = FALSE)
  p
}
library(corrplot)

corrplot.mixed(cor(new_data2), lower = 'shade', upper = 'pie')
ggpairs(new_data2)
ggpairs(new_data2, lower = list(continuous = my_fn))

#ggpairs(subsetted_countries2 %>% select(GDP_per_capita_pct_change), ggplot2::aes(colour=country))

# Armenia + Georgia
# Armenia + Azerbajan
# Armenia + Russia



eq <- function(x,y) {
  m <- lm(y ~ x)
  as.character(
    as.expression(
      substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                 list(a = format(coef(m)[1], digits = 4),
                      b = format(coef(m)[2], digits = 4),
                      r2 = format(summary(m)$r.squared, digits = 3)))
    )
  )
}

# Ticker for growth is NY.GDP.MKTP.KD.ZG

ggplot(new_data2,aes(y = Armenia, x = Georgia)) + 
  geom_point() + 
  geom_smooth(method = "lm", se=FALSE) +
  geom_text(x = -.10, y = .15, label = eq(new_data2$Armenia,new_data2$Georgia), parse = TRUE)

ggplot(new_data2,aes(y = Armenia, x = Azerbaijan)) + 
  geom_point() + 
  geom_smooth(method = "lm", se=FALSE) +
  geom_text(x = -.10, y = .15, label = eq(new_data2$Azerbaijan,new_data2$Armenia), parse = TRUE)



ggplot(new_data2,aes(y = Armenia, x = Russia, col = 1995:2020)) + 
  geom_point() + 
  geom_smooth(method = "lm", se=FALSE) +
  geom_text(x = -.10, y = .10, label = eq(new_data2$Russia, new_data2$Armenia), parse = TRUE)


### Insert Raincloud

ggplot(new_data2, aes(x=Armenia)) + geom_histogram(color="darkblue", fill="lightblue")
# Let's see if we can make it more attractive

ggplot(subsetted_countries2,aes(x = country, y = GDP_per_capita_pct_change, fill = country)) +
  ggdist::stat_halfeye(
    adjust = .4,
    justification = -.2,
    .width = 0,
    point_colour = NA
  ) + 
  geom_boxplot(
    width = .12,
    outlier.color = NA, 
    alpha = .5
  ) +
  ggdist::stat_dots(
    side = "left",
    justitication = 1.1,
    binwidth = .005
  )  + coord_flip() 




### Now find the nearest neighbors

GDP_5_year_avg <- WDI_GDP_per_capita %>% filter(year > 2016) %>% group_by(country) %>%
  dplyr::summarize(Mean_Raw_GDP = mean(GDP_per_capita_2010_USD, na.rm=TRUE))

Armenia_Result <- GDP_5_year_avg %>% filter(country == "Armenia") %>% select(Mean_Raw_GDP) %>% as.numeric()
View(GDP_5_year_avg %>%
  group_by(country) %>%
  arrange(
    abs(Mean_Raw_GDP - Armenia_Result)) %>% mutate(Diff = Mean_Raw_GDP - Armenia_Result))



# Growth Rate
GDP_growth_all <- WDI_GDP_per_capita %>% arrange(year, country) %>% 
  group_by(country) %>% mutate_each(funs(pct), c(GDP_per_capita_2010_USD)) %>%
   filter(!is.na(GDP_per_capita_2010_USD))

GDP_growth_5_year_avg <- GDP_growth_all %>% filter(year > 2016) %>% group_by(country) %>%
  dplyr::summarize(Mean_GDP_growth_rate = mean(GDP_per_capita_2010_USD, na.rm=TRUE))


Armenia_Result_gdp_growth <- GDP_growth_5_year_avg %>% filter(country == "Armenia") %>% select(Mean_GDP_growth_rate) %>% as.numeric()
View(GDP_growth_5_year_avg %>%
       group_by(country) %>%
       arrange(
         abs(Mean_GDP_growth_rate - Armenia_Result_gdp_growth)) %>% mutate("Diff" = Mean_GDP_growth_rate - Armenia_Result_gdp_growth))






#### Inflation 
# FP.CPI.TOTL.ZG

#View(WDI_GDP_per_capita)

library(tidysynth)
library(tidyverse)


### using API to pull data
WDI_GDP_per_capita <- WDI(indicator="NY.GDP.PCAP.KD", country= "all", start=1994, end=2022)
# renaming variables appropriately
names(WDI_GDP_per_capita) <- c("iso2c", "country", "GDP_per_capita_2010_USD", "year")

check_list <- WDI_GDP_per_capita %>% filter(year > 2000) %>% drop_na() %>% group_by(country) %>% count() 
check_list2 <- check_list %>% filter(n == 20) %>% select(country)
top50 <- c("Armenia", "Burundi", #"China", 
           "Myanmar", "Ethiopia", "Congo, Dem. Rep.", 
           "Sierra Leone", "Mozambique", "Malawi", "Rwanda", #"Macao SAR, China", 
           "Niger", "Nepal", "Central African Republic", "Cambodia", "Madagascar", 
           "Tajikistan", "Liberia", "Bangladesh",#, "Burkina Faso", 
           "Togo", "Guinea-Bissau", "Lao PDR", "Tanzania", 
           "Uganda", "Mongolia", 
           "Chad", "Timor-Leste", 
           "Kyrgyz Republic", "Guinea", "Mali", "Uzbekistan", "Vietnam", 
           "India", "Sudan", 
           "Ghana",  "Kenya", "Zambia", "Lesotho", 
           "Bhutan", "Pakistan", "Haiti", "Sri Lanka"
           )

top5 <- c("Armenia", "Mongolia", "Vietnam", "Bhutan", "Sri Lanka", "Ghana", "Uzbekistan")

ggplot(data= WDI_GDP_per_capita%>% filter(country %in% c(neighbors)), aes(x=year, y=NY.GDP.PCAP.KD, color=country)) +
  geom_line()+
  geom_point() + 
  ggtitle("GDP per capita of Armenia Neighbors") + 
  scale_y_continuous(labels = scales::dollar_format()) + 
  labs(y="GDP Per capita in 2010 USD")


ggplot(data= WDI_GDP_per_capita%>% filter(country %in% c(top5)), aes(x=year, y=NY.GDP.PCAP.KD, color=country)) +
  geom_line()+
  geom_point() + 
  ggtitle("GDP per capita of Armenia Comparison Countries") + 
  scale_y_continuous(labels = scales::dollar_format()) + 
  labs(y="GDP Per capita in 2010 USD")



ggplot(new_data2, aes(x=Armenia)) + geom_histogram(color="darkblue", fill="lightblue")
# Let's see if we can make it more attractive

ggplot(WDI_GDP_per_capita%>% filter(country %in% c(neighbors)),aes(x = country, y = NY.GDP.PCAP.KD, fill = country)) +
  ggdist::stat_halfeye(
    adjust = .4,
    justification = -.2,
    .width = 0,
    point_colour = NA
  ) + 
  geom_boxplot(
    width = .12,
    outlier.color = NA, 
    alpha = .5
  ) +
  ggdist::stat_dots(
    side = "left",
    justitication = 1.1,
    binwidth = .005
  )  + coord_flip() 








inputted_data <-  WDI_GDP_per_capita %>% filter(year >= 2010) %>% drop_na() %>% filter(country %in% unlist(check_list2$country)) %>%
  filter(country %in% top5) %>% 
  arrange(country, year) 
x <- reshape(inputted_data %>% select(country, year, GDP_per_capita_2010_USD), idvar = "country", timevar = "year", direction = "wide")
#write.csv(x, "GDP_per_capita2.csv")



start <- ""
for (i in 5:21){
  start <- paste0(start, "$A$",i,"*C",i, "+")
}
start





smoking_out <-
  inputted_data %>%  
  # initial the synthetic control object
  synthetic_control(outcome = GDP_per_capita_2010_USD, # outcome
                    unit = country, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "Armenia", # unit where the intervention occurred
                    i_time = 2020, # time period when the intervention occurred
                    generate_placebos=T # generate placebo synthetic controls (for inference)
  ) %>%
  
  generate_predictor(time_window = 2012:2020, 
  mean_GDP_per_capita_2010_USD = mean(GDP_per_capita_2010_USD, na.rm = TRUE)) %>% 
    #generate_predictor(time_window = 2013,
    #                 per_capita_2013 = GDP_per_capita_2010_USD) %>%
    #generate_predictor(time_window = 2014,
    #                   per_capita_2014 = GDP_per_capita_2010_USD) %>%
    generate_predictor(time_window = 2015,
                       per_capita_2015 = GDP_per_capita_2010_USD) %>%
    generate_predictor(time_window = 2016,
                       per_capita_2016 = GDP_per_capita_2010_USD) %>%
    generate_predictor(time_window = 2017,
                       per_capita_2017 = GDP_per_capita_2010_USD) %>%
    generate_predictor(time_window = 2018,
                       per_capita_2018 = GDP_per_capita_2010_USD) %>%
    generate_predictor(time_window = 2019,
                       per_capita_2019 = GDP_per_capita_2010_USD) %>% 
    generate_predictor(time_window = 2020,
                      per_capita_2020 = GDP_per_capita_2010_USD) %>% 

  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = 2012:2020, margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 ) %>%
  
  # Generate the synthetic control
  generate_control()


smoking_out %>% plot_trends()
smoking_out %>% plot_differences()
smoking_out %>% plot_weights()




smoking_out2 <-
  inputted_data %>% 
  # initial the synthetic control object
  synthetic_control(outcome = GDP_per_capita_2010_USD, # outcome
                    unit = country, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "Armenia", # unit where the intervention occurred
                    i_time = 2020, # time period when the intervention occurred
                    generate_placebos=T # generate placebo synthetic controls (for inference)
  ) %>%
  
  generate_predictor(time_window = 2010:2019, 
                     mean_GDP_per_capita_2010_USD = mean(GDP_per_capita_2010_USD, na.rm = TRUE)) %>% 
  
  generate_predictor(time_window = 2015,
                     cigsale_1980 = GDP_per_capita_2010_USD) %>%
  generate_predictor(time_window = 2019,
                     cigsale_1988 = GDP_per_capita_2010_USD) %>% 
  
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = 2010:2020, margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 ) %>%
  
  # Generate the synthetic control
  generate_control()


smoking_out2 %>% plot_trends()
smoking_out2 %>% plot_differences()
smoking_out2 %>% plot_weights()


# It's actually doing fine, but for whatever reason, its just reading the observed in backwards.


#### Inflation ######

Inflation_data <- WDI(indicator="FP.CPI.TOTL.ZG", country= "all", start=1994, end=2021)
# renaming variables appropriately
names(Inflation_data) <- c("iso2c", "country", "Inflation", "year")

ggplot(data=Inflation_data %>% filter(country %in% c(neighbors) & year > 2010), aes(x=year, y=Inflation/100, color=country)) +
  geom_line()+
  geom_point() + 
  ggtitle("Inflation Rate of Armenia + Neighbors") + 
  scale_y_continuous(labels = scales::percent_format()) + 
  labs(y="Inflation Rate")


ggplot(data=Inflation_data %>% filter(country %in% c(top5) & year > 2010), aes(x=year, y=Inflation/100, color=country)) +
  geom_line()+
  geom_point() + 
  ggtitle("Inflation Rate of Armenia and Synthetic Countries") + 
  scale_y_continuous(labels = scales::percent_format()) + 
  labs(y="Inflation Rate")


check_list <- Inflation_data %>% filter(year > 2000) %>% drop_na() %>% group_by(country) %>% count() 
check_list2 <- check_list %>% filter(n == 20) %>% select(country)


inputted_data <-  Inflation_data %>% filter(year >= 2000) %>% drop_na() %>% filter(country %in% unlist(check_list2$country)) %>%
  filter(country %in% top5) %>% 
  arrange(country, year) 

smoking_out3 <-
  inputted_data %>% 
  # initial the synthetic control object
  synthetic_control(outcome = Inflation, # outcome
                    unit = country, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "Armenia", # unit where the intervention occurred
                    i_time = 2020, # time period when the intervention occurred
                    generate_placebos=T # generate placebo synthetic controls (for inference)
  ) %>%
  
  generate_predictor(time_window = 2000:2020, 
                     mean_GDP_per_capita_2010_USD = mean(Inflation, na.rm = TRUE)) %>% 
  
  generate_predictor(time_window = 2015,
                     inflation_2015 = Inflation) %>%
  generate_predictor(time_window = 2016,
                     inflation_2016 = Inflation) %>%
  generate_predictor(time_window = 2017,
                     inflation_2017 = Inflation) %>%
  generate_predictor(time_window = 2018,
                     inflation_2018 = Inflation) %>%
  generate_predictor(time_window = 2019,
                     inflation_2019 = Inflation) %>% 
  generate_predictor(time_window = 2020,
                     inflation_2020 = Inflation) %>% 
  
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = 2012:2020, margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 ) %>%
  
  # Generate the synthetic control
  generate_control()


smoking_out3 %>% plot_trends()
smoking_out3 %>% plot_differences()
smoking_out3 %>% plot_weights()


inflation_top5 <- c("Armenia", "Togo", "Kyrgz Republic", "Mali", "Mongolia", "Vietnam")

inputted_data <-  Inflation_data %>% filter(year >= 2013) %>% drop_na() %>% filter(country %in% unlist(check_list2$country)) %>%
  filter(country %in% inflation_top5) %>% 
  arrange(country, year) 
smoking_out4 <-
  inputted_data %>% 
  # initial the synthetic control object
  synthetic_control(outcome = Inflation, # outcome
                    unit = country, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "Armenia", # unit where the intervention occurred
                    i_time = 2020, # time period when the intervention occurred
                    generate_placebos=T # generate placebo synthetic controls (for inference)
  ) %>%
  
  generate_predictor(time_window = 2013:2020, 
                     mean_Inflation = mean(Inflation, na.rm = TRUE)) %>% 
  generate_predictor(time_window = 2014,
                     Inflation2014 = Inflation) %>%
  generate_predictor(time_window = 2015,
                     Inflation2015 = Inflation) %>%
  generate_predictor(time_window = 2016,
                     Inflation2016 = Inflation) %>%
  generate_predictor(time_window = 2017,
                     Inflation2017 = Inflation) %>%
  generate_predictor(time_window = 2018,
                     Inflation2018 = Inflation) %>%
  generate_predictor(time_window = 2019,
                     Inflation2019 = Inflation) %>% 
  generate_predictor(time_window = 2020,
                     Inflation2020 = Inflation) %>% 
  
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = 2015:2020, margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 ) %>%
  
  # Generate the synthetic control
  generate_control()


smoking_out4 %>% plot_trends()
smoking_out4 %>% plot_differences()
smoking_out4 %>% plot_weights()




















library(tidyverse)
library(tidysynth)

data("smoking")
smoking_out2 <-
  
  smoking %>%
  
  # initial the synthetic control object
  synthetic_control(outcome = cigsale, # outcome
                    unit = state, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "California", # unit where the intervention occurred
                    i_time = 1988, # time period when the intervention occurred
                    generate_placebos=T # generate placebo synthetic controls (for inference)
  ) %>%
  
  # Generate the aggregate predictors used to fit the weights
  
  # average log income, retail price of cigarettes, and proportion of the
  # population between 15 and 24 years of age from 1980 - 1988
  generate_predictor(time_window = 1980:1988,
                     ln_income = mean(lnincome, na.rm = T),
                     ret_price = mean(retprice, na.rm = T),
                     youth = mean(age15to24, na.rm = T)) %>%
  
  # average beer consumption in the donor pool from 1984 - 1988
  generate_predictor(time_window = 1984:1988,
                     beer_sales = mean(beer, na.rm = T)) %>%
  
  # Lagged cigarette sales 
  generate_predictor(time_window = 1975,
                     cigsale_1975 = cigsale) %>%
  generate_predictor(time_window = 1980,
                     cigsale_1980 = cigsale) %>%
  generate_predictor(time_window = 1988,
                     cigsale_1988 = cigsale) %>%
  
  
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = 1970:1988, # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
  ) %>%
  
  # Generate the synthetic control
  generate_control()
smoking_out2 %>% plot_trends()





test <- WDI(indicator="SL.UEM.TOTL.ZS", country= "all", start=1994, end=2021)

View(test)


# library(ggpubr)
# ggscatter(new_data, x = "Armenia", y = "Georgia", add = "reg.line") +
#   stat_cor(label.x = 3, label.y = 34) +
#   stat_regline_equation(label.x = 3, label.y = 32)
# 
# 
# #https://rpkgs.datanovia.com/ggpubr/reference/stat_regline_equation.html
# library(ggplot2)
# library(ggpmisc)
# 
# 
# 
# 
# 
# lm_eqn <- function(df, y, x){
#   m <- lm(y ~ x, df);
#   eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
#                    list(a = format(unname(coef(m)[1]), digits = 2),
#                         b = format(unname(coef(m)[2]), digits = 2),
#                         r2 = format(summary(m)$r.squared, digits = 3)))
#   as.character(as.expression(eq));
# }
# 
# p1 <- p + geom_text(x = 25, y = 300, label = lm_eqn(df), parse = TRUE)
# 
# 
# 
# my.formula <- "Armenia" ~ "Georgia"
# 
# p <- ggplot(data = data.frame(new_data2), aes(x = x, y = y)) +
#   geom_smooth(method = "lm", se=FALSE, color="black", formula = my.formula) +
#   stat_poly_eq(formula = my.formula, 
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#                parse = TRUE) +         
#   geom_point()
# p
# 
# 
# library(corrplot)
# library(RColorBrewer)
# M <-cor(new_data[,-1])
# corrplot(M, type="upper", order="hclust",
#          col=brewer.pal(n=8, name="RdYlBu"))
# library(igraph)
# # Make an Igraph object from this matrix:
# network <- graph_from_adjacency_matrix(M, weighted=T, mode="undirected", diag=F)
# 
# # Basic chart
# plot(network)
