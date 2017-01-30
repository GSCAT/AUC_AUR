install.packages("dplyr")
library(dplyr)
library(magrittr)
library(readxl)
library(readr)
library(tidyr)

# Read in Data ----
# PCF_Forecast <- read_csv("Mockup for Jessica.csv")
PCF_Forecast <- read_excel("AUR AUC 2016 - Jan Fcst - for KB.xlsx", sheet = "Corp CP Essbase Pull KB" )
PCF_Budget <- read_excel("AUR AUC 2016 - Jan Fcst - for KB.xlsx", sheet = "Corp CP Essbase Pull B KB" )
product_key <- read_excel("Area Product Key.xlsx", sheet = 1)
quarter_mapping <- read_csv("quarter_mapping.csv")
BMC_table <- read_csv("BMC.csv")

last_col <- length(PCF_Forecast) 

# Add Source column to Forecast----
PCF_Forecast[last_col+1] <- as.factor("Forecast")
names(PCF_Forecast)[length(PCF_Forecast)] <- "Source"

# Add Source column to Budget ----
PCF_Budget[last_col+1] <- as.factor("Budget")
names(PCF_Budget)[length(PCF_Budget)] <- "Source"


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
quarter_mapping <-  conv_fact(1:3, quarter_mapping)

rbind_PCF <- rbind(PCF_Forecast, PCF_Budget)

# Left join to prod_key table for "Business Unit" field and arrange
PCF_post_proc <-  left_join(rbind_PCF, product_key, by= c('Area', 'Product')) %>% 
  select(`Years`, `Accounts`, `Business Unit`,`Source`, `February`, `March`, `April`, 
         `May`, `June`, `July`, `August`, `September`, `October`, `November`, `December`, `January`) %>% 
  gather("Month", "Value", 5:16) %>% 
  spread(Accounts, Value) %>% 
  left_join(quarter_mapping, by = c('Month'= 'Fiscal Month')) %>% 
  left_join(BMC_table, by = c('Business Unit' = 'BMC'))

# PCF_Budget_post_proc <-  left_join(PCF_Budget, product_key, by= c('Area', 'Product')) %>% 
#   select(`Years`, `Accounts`, `Business Unit`, `Source`, `February`, `March`, `April`, 
#          `May`, `June`, `July`, `August`, `September`, `October`, `November`, `December`) %>% 
#   gather("Month", "Value", 5:15) %>% 
#   spread(Accounts, Value) %>% 
#   left_join(quarter_mapping, by = c('Month'= 'Fiscal Month'))

# PCF_Forecast_post_proc[,5:12] <- lapply(PCF_Forecast_post_proc[, 5:12], function (x) as.numeric(x))

PCF_post_proc[,5:12] <- lapply(PCF_post_proc[, 5:12], function (x) as.numeric(x))

# Output PCF ----
Output_PCF <- PCF_post_proc %>% 
  group_by(Years, `BMC_short_desc`, `Fiscal Quarter`) %>% 
 summarise("Forecast TY AUR of Sales" = sum(subset(`Retail$`,    Source == "Forecast"))/sum(subset(`Unit Sales`, Source == "Forecast")),
                 "TY AUC of Receipts" = sum(subset(`Cost Rcpts`, Source == "Forecast"))/sum(subset(`Unit Rcpts`, Source == "Forecast")),
                 "Budget AUR of Sales"= sum(subset(`Retail$`,    Source == "Budget"))/sum(subset(`Unit Sales`, Source == "Budget")),
             "Budget AUC of Receipts" = sum(subset(`Cost Rcpts`, Source == "Budget"))/sum(subset(`Unit Rcpts`, Source == "Budget")),
                       "AUR % Change" = as.numeric((`Forecast TY AUR of Sales`-`Budget AUR of Sales`)/`Forecast TY AUR of Sales`)*100,
                       "AUC % Change" = as.numeric((`TY AUC of Receipts`-`Budget AUC of Receipts`)/`TY AUC of Receipts`)*100,
                          "GM Budget" = sum((subset(`Retail$`, Source == "Budget") - subset(`Cost$`, Source == "Budget"))/ sum(subset(`Retail$`, Source == "Budget")))*100,
                 "GM Forecast/Actual" = sum((subset(`Retail$`, Source == "Forecast") - subset(`Cost$`, Source == "Forecast"))/ sum(subset(`Retail$`, Source == "Forecast")))*100,
                "GM Budget (dollars)" = sum((subset(`Retail$`, Source == "Budget") - subset(`Cost$`, Source == "Budget"))),
       "GM Forecast/Actual (dollars)" = sum((subset(`Retail$`, Source == "Forecast") - subset(`Cost$`, Source == "Forecast"))))

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