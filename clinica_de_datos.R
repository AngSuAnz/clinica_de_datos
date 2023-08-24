#LIBRERIAS----------------------------------------------------------------------

library(installr)
library(rsconnect)
library(janitor)
library(shiny)
library(googlesheets4)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)
library(dplyr)
library(readxl)
library(stringr)
library(readxl)
library(tidyverse)
library(plotly)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(dplyr)
library(stringr)
library(scales)
library(knitr)
library(DT)
library(sf)
library(bslib)
library(shiny)
library(shinydashboard)


#DF-----------------------------------------------------------------------------

callejero <- st_read("00-data/callejero.shp",
                      stringsAsFactors = TRUE,
                      options = "encoding=latin1")

distritos_economicos <- st_read("00-data/distritos_economicos.shp",
                     stringsAsFactors = TRUE,
                     options = "encoding=latin1")


deptos_alquiler <- data.frame(read_excel(
  "00-data/MI_DAN_AX04_procesado.xlsx",
  #sheet="calif_ocup_sexo__annio__calif_ocup_limpio.xlsx",
  #range = "A1:D100",
  col_names = TRUE,
  col_types = NULL,
  na = "",
  trim_ws = TRUE,
  skip = 0) %>% 
    clean_names())

deptos_venta <- data.frame(read_excel(
  "00-data/MI_DVN_procesado.xlsx",
  #sheet="calif_ocup_sexo__annio__calif_ocup_limpio.xlsx",
  #range = "A1:D100",
  col_names = TRUE,
  col_types = NULL,
  na = "",
  trim_ws = TRUE,
  skip = 0) %>% 
    clean_names())

maestro <- data.frame(read_excel(
  "00-data/dataset_MEC.xlsx",
  #sheet="calif_ocup_sexo__annio__calif_ocup_limpio.xlsx",
  #range = "A1:D100",
  col_names = TRUE,
  col_types = NULL,
  na = "",
  trim_ws = TRUE,
  skip = 0) %>% 
    clean_names()) %>% 
  mutate(rubro=as.factor(rubro))


#CONSIGNAS----------------------------------------------------------------------

## 1.¿Cuántos rubros hay en la ciudad y cómo son sus porcentajes? --------------
#¿Cuáles son los rubros predominantes?
  

porcentajes_rubros <- data.frame(table(maestro$rubro) / nrow(maestro) * 100) %>% 
  print()



## 2.¿Cómo se distribuyen geográficamente los establecimientos comerciales en la ciudad? ---- 
#¿Se detectan patrones?
  

## 3.---------------------------------------------------------------------------
