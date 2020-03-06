# add packages ----------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)
library(sf)
library(arcgisbinding)
library(raster)

# load data ---------------------------------------------------------------

# data available from http://silvis.forest.wisc.edu/data/wui-change/
wui_raw <- read_sf(dsn = here::here("ca_wui_cp12"), layer = 'ca_wui_cp12') %>% 
  clean_names() %>% 
  mutate(id = row_number())

test <- sf::st_read(dsn = here::here("fveg15_1.gdb"), layer = "fveg15_1.gdb")

test <- arc.open("fveg15_1.gdb")

test_test <- sf::st_write(test, "fveg15_1.gdb", "test.shp" )
est_test <- arc.write(test)
extr

# sub-data-------------------------------------------------------------------

#unite the columns in 1 then filter for wui phrases
wui_unite <- wui_raw %>% 
  unite(wui_combined, c(wuiclass90, wuiclass00, wuiclass10), sep = "-", remove = F)

wui <- wui_unite %>% 
  filter(str_detect(wui_combined, pattern = c("Inermix|Interface"))) %>% 
  mutate(wuiclass90_simplified = case_when(
    wuiclass90 %in% c("Low_Dens_Intermix", "Med_Dens_Intermix", "High_Dens_Intermix") ~"Intermix",
    wuiclass90 %in% c("Low_Dens_Interface", "Med_Dens_Interface", "High_Dens_Interface") ~"Interface",
    wuiclass90 %in% c("Uninhabited_Veg", "Uninhabited_NoVeg", "Very_Low_Dens_Veg" , "Water", "Very_Low_Dens_NoVeg",
                      "High_Dens_NoVeg"  , "Med_Dens_NoVeg" , "Low_Dens_NoVeg") ~"Other"
      )) %>% 
  mutate(wuiclass90_simplified = case_when(
    wuiclass00 %in% c("Low_Dens_Intermix", "Med_Dens_Intermix", "High_Dens_Intermix") ~"Intermix",
    wuiclass00 %in% c("Low_Dens_Interface", "Med_Dens_Interface", "High_Dens_Interface") ~"Interface",
    wuiclass00 %in% c("Uninhabited_Veg", "Uninhabited_NoVeg", "Very_Low_Dens_Veg" , "Water", "Very_Low_Dens_NoVeg",
                  "High_Dens_NoVeg"  , "Med_Dens_NoVeg" , "Low_Dens_NoVeg") ~"Other"
    )) %>% 
  mutate(wuiclass90_simplified = case_when(
    wuiclass10 %in% c("Low_Dens_Intermix", "Med_Dens_Intermix", "High_Dens_Intermix") ~"Intermix",
    wuiclass10 %in% c("Low_Dens_Interface", "Med_Dens_Interface", "High_Dens_Interface") ~"Interface",
    wuiclass10 %in% c("Uninhabited_Veg", "Uninhabited_NoVeg", "Very_Low_Dens_Veg" , "Water", "Very_Low_Dens_NoVeg",
                  "High_Dens_NoVeg"  , "Med_Dens_NoVeg" , "Low_Dens_NoVeg") ~"Other"

  ))

#write.table(wui, "wui_tidy.csv", sep = ",") # get bad table with too many NAs

