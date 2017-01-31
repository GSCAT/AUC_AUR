library(dplyr)
library(readr)
library(RODBC)
library(rChoiceDialogs)

# Read in passwords ----
my_uid <- read_lines("C:\\Users\\Ke2l8b1\\Documents\\my_uid.txt")
my_pwd <- read_lines("C:\\Users\\Ke2l8b1\\Documents\\my_pwd.txt")

# Function for opening file chooser ----
choose_file_directory <- function()
{
  v <- jchoose.dir()
  return(v)
}

# Function for converting columns to factors ----
# Only works when on first n columns. Pass a sequence (i.e. 1:3 for first 3 columns)
conv_fact <- function(x, my_table){
  for(i in seq_along(x))
    my_table[[i]] <- as.factor(my_table[[i]])
  return(my_table)
}

my_directory <- choose_file_directory()

# Create RODBC connection---- 
my_connect <- odbcConnect(dsn= "IP EDWP", uid= my_uid, pwd= my_pwd)
# sqlTables(my_connect, catalog = "EDWP", tableName  = "tables")
sqlQuery(my_connect, query = "SELECT  * from dbc.dbcinfo;")

NOVA_data <- sqlQuery(my_connect, query = "SELECT * FROM SRAA_SAND.NOVA_ACTL_YTD")

save(NOVA_data, file = paste(my_directory, "NOVA_DATA2.rda", sep = .Platform$file.sep ))
# load(file = paste(my_directory, "NOVA_DATA.rda", sep = .Platform$file.sep ))

# Separate EDW Quarter Description ----
NOVA_data <- NOVA_data %>% separate(FIS_QTR_DESC, into= c('FIS_QTR_DESC', 'FIS_QTR_DATE_RANGE'), sep = ":")

NOVA_data <- conv_fact(1:13, NOVA_data)

levels(NOVA_data$FIS_YR_NBR_MO) <- c("Yr2", "LY", "TY")

Output_NOVA <- NOVA_data %>% group_by(FIS_QTR_DESC, FIS_YR_NBR, BRD_NM) %>% 
  summarise("TY AUR of Sales (Historic)" = sum(subset(`Rev Sales Amt`, FIS_YR_NBR_MO == "TY"), na.rm = TRUE)/sum(subset(`Rev Sales Units`, FIS_YR_NBR_MO == "TY"), na.rm = TRUE),
            "LY AUR of Sales (Historic)" = sum(subset(`Rev Sales Amt`, FIS_YR_NBR_MO == "LY"), na.rm = TRUE)/sum(subset(`Rev Sales Units`, FIS_YR_NBR_MO == "LY"), na.rm = TRUE),
            "Yr2 AUR of Sales (Historic)" = sum(subset(`Rev Sales Amt`, FIS_YR_NBR_MO == "Yr2"), na.rm = TRUE)/sum(subset(`Rev Sales Units`, FIS_YR_NBR_MO == "Yr2"), na.rm = TRUE),
            "TY AUC of Receipts (Historic)" = sum(subset(`Cost Vendor Rects` , FIS_YR_NBR_MO == "TY"), na.rm = TRUE)/sum(subset(`Unit Vendor Rects`, FIS_YR_NBR_MO == "TY"), na.rm = TRUE), 
            "LY AUC of Receipts (Historic)" = sum(subset(`Cost Vendor Rects`, FIS_YR_NBR_MO == "LY"), na.rm = TRUE)/sum(subset(`Unit Vendor Rects`, FIS_YR_NBR_MO == "LY"), na.rm = TRUE) )


# Experimental

View(subset(NOVA_data, 
            NOVA_data$FIS_YR_NBR == levels(NOVA_data$FIS_YR_NBR)[length(levels(NOVA_data$FIS_YR_NBR))-1]))