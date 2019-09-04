library(tidyverse)
library(openxlsx)
library(lubridate)

rm(list = ls())

fecha_inicio <- as.numeric(as.POSIXct("2018-01-01", tz = "GMT"))
fecha_final <- as.numeric(as.POSIXct("2019-08-26", tz = "GMT")) - 300

fechas_rango <- as.data.frame(seq(fecha_inicio, 
                                  fecha_final, 
                                  300))
names(fechas_rango)[1] = "Hora"

source(file = "Caudal_SL.R")
source(file = "Caudal_Poc.R")
source(file = "Caudal_AguaGata.R")
source(file = "Caudal_Bijagua_Bij.R")
source(file = "Caudal_Bijagua_Zap.R")
#source(file = "Caudal_Canalete.R")
source(file = "Caudal_Esperanza.R")
source(file = "Caudal_Hidrozarcas.R")


#################################################
# Guardado de archivos

DS_list <- list("San Lorenzo" = Toma_SL_15m, 
                "Pocosol" = Toma_Poc_15m,
                "AguaGata" = Toma_AG_15m,
                "Bijagua" = Toma_Bij_15m,
                "Zapote" = Toma_Zap_15m,
                "Esperanza" = Toma_Esp_15m,
                "HidroZarcas" = Toma_HZ_15m
#                "Canalete" = Toma_Can_15m
                )
write.xlsx(DS_list, file = "C:/Data Science/ArhivosGenerados/Nivel y Caudal Conelectricas.xlsx")
