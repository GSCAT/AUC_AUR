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

my_directory <- choose_file_directory()

# Create RODBC connection---- 
my_connect <- odbcConnect(dsn= "IP EDWP", uid= my_uid, pwd= my_pwd)
# sqlTables(my_connect, catalog = "EDWP", tableName  = "tables")
sqlQuery(my_connect, query = "SELECT  * from dbc.dbcinfo;")

NOVA_data <- sqlQuery(my_connect, query = "SELECT * FROM SRAA_SAND.NOVA_ACTL_YTD")

save(NOVA_data, file = paste(my_directory, "NOVA_DATA.rda", sep = .Platform$file.sep ))

