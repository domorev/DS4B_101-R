# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# IMPORTING DATA INTO R ----


# 1.0 Load libraries ----

# Contains readr
library(tidyverse)   

# Excel Connection
library(readxl)
library(writexl)

# Database Connection
library(odbc)
library(RSQLite)


# 2.0 readr ----

# 2.1 CSV ----

bike_orders_csv_tbl <- readr::read_csv("00_data/bike_sales/data_wrangled/bike_orderlines.csv")

readr::problems()

bike_orders_csv_tbl %>% 
    slice(7916)

## We can specify col_types if they are read in incorrectly and check results.

# readr::read_csv("00_data/bike_sales/data_wrangled/bike_orderlines.csv",
#                col_types = cols(
#                    order_id = col_double()
#                )) %>% 
#    slice(7916)

# 2.2 RDS ----

bike_orders_rds_tbl <- readr::read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

bike_orders_rds_tbl %>% 
    slice(7916)

# 3.0 Excel ----

readxl::read_excel("00_data/bike_sales/data_wrangled/bike_orderlines.xlsx")

readxl::excel_sheets("00_data/bike_sales/data_wrangled/bike_orderlines.xlsx") # returns a list of sheets

bike_orders_excel_tbl <- readxl::read_excel("00_data/bike_sales/data_wrangled/bike_orderlines.xlsx", sheet = "Sheet1")

bike_orders_excel_tbl

# 4.0 Databases  ----

## 4.1 Create Connection ----
con <- RSQLite::dbConnect(drv = SQLite(), dbname = "00_data/chinook/Chinook_Sqlite.sqlite")

## 4.2 Examine Tables ----
dbListTables(con)

## 4.3 Extract Data from a Table ----
album_tbl <- tbl(con, "Album") %>% collect()

artists_tbl <- tbl(con, "Artist") %>% collect()

## 4.4 Disconnect from the Database
con
dbDisconnect(con)
con
