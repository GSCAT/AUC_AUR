install.packages("dplyr")
library(dplyr)
library(magrittr)
library(readxl)
library(readr)
library(tidyr)


# PCF_Forecast <- read_csv("Mockup for Jessica.csv")
PCF_Forecast <- read_excel("AUR AUC 2016 - Jan Fcst - for KB.xlsx", sheet = "Corp CP Essbase Pull KB" )
PCF_Budget <- read_excel("AUR AUC 2016 - Jan Fcst - for KB.xlsx", sheet = "Corp CP Essbase Pull B KB" )
product_key <- read_excel("Area Product Key.xlsx", sheet = 1)
quarter_mapping <- read_csv("quarter_mapping.csv")
BMC_table <- read_csv("BMC.csv")

# Function for converting columns to factors ----
# Only works when on first n columns. Pass a sequence (i.e. 1:3 for first 3 columns)
conv_fact <- function(x, my_table){
  for(i in seq_along(x))
 my_table[[i]] <- as.factor(my_table[[i]])
  return(my_table)
}
# Change first number of columns (vec_1) to Factor ----

PCF_Forecast <- conv_fact(1:4, PCF_Forecast)
PCF_Budget <- conv_fact(1:4, PCF_Budget)
product_key <- conv_fact(1:3, product_key)
BMC_table <- conv_fact(1:8, BMC_table)

# Left join to prod_key table for "Business Unit" field and arrange
PCF_Forecast_post_proc <-  left_join(PCF_Forecast, product_key, by= c('Area', 'Product')) %>% 
  select(`Years`, `Accounts`, `Business Unit`, `February`, `March`, `April`, 
         `May`, `June`, `July`, `August`, `September`, `October`, `November`, `December`) %>% 
  gather("Month", "Value", 4:14) %>% 
  spread(Accounts, Value) %>% 
  left_join(quarter_mapping, by = c('Month'= 'Fiscal Month')) %>% 
  left_join(BMC_table, by = c())

PCF_Budget_post_proc <-  left_join(PCF_Budget, product_key, by= c('Area', 'Product')) %>% 
  select(`Years`, `Accounts`, `Business Unit`, `February`, `March`, `April`, 
         `May`, `June`, `July`, `August`, `September`, `October`, `November`, `December`) %>% 
  gather("Month", "Value", 4:14)%>% 
  spread(Accounts, Value) %>% 
  left_join(quarter_mapping, by = c('Month'= 'Fiscal Month'))

# Output PCF ----
Ouput_PCF_Forecast <- PCF_Forecast_post_proc %>% 
  group_by(Years, `Business Unit`, `Fiscal Quarter`) %>% 
 # summarise("Forecast TY AUR of Sales" = sum(as.integer(`Retail$`))/ sum(as.integer(`Unit Sales`)))

# Depricated code ----
# PCF_Forecast[[1]] <- as.factor(PCF_Forecast[[1]])
# PCF_Forecast[[2]] <- as.factor(PCF_Forecast[[2]])
# PCF_Forecast[[3]] <- as.factor(PCF_Forecast[[3]])
# PCF_Forecast[[4]] <- as.factor(PCF_Forecast[[4]])

# Experimantal function ----

conv_fact2 <- function(x, my_table){
  for(i in seq_along(x))
    my_table[[i]] <- as.factor(my_table[[i]])
  return(my_table)
}

vec_1 <- c(1, 3, 4 )
PCF_Forecast2 <- conv_fact2(vec_1, PCF_Forecast)