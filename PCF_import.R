install.packages("dtplyr")
library(dtplyr)
library(readxl)


# PCF_Forecast <- read_csv("Mockup for Jessica.csv")
PCF_Forecast <- read_excel("AUR AUC 2016 - Jan Fcst - for KB.xlsx", sheet = "Corp CP Essbase Pull KB" )
PCF_Budget <- read_excel("AUR AUC 2016 - Jan Fcst - for KB.xlsx", sheet = "Corp CP Essbase Pull B KB" )

conv_fact <- function(x, my_table){
  for(i in seq_along(x))
 my_table[[i]] <- as.factor(my_table[[i]])
  return(my_table)
}
# Change first number of columns (vec_1) to Factor ----
vec_1 <- c(1:4)
PCF_Forecast <- conv_fact(vec_1, PCF_Forecast)
PCF_Budget <- conv_fact(vec_1, PCF_Budget)

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