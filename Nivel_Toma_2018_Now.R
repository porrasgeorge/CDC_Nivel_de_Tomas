library(tidyverse)
library(openxlsx)
library(lubridate)

rm(list = ls())

source(file = "Caudal_SL.R")
source(file = "Caudal_Poc.R")

#################################################
# Guardado de archivos

DS_list <- list("San Lorenzo" = NivelToma_SL_15m, "Pocosol_Gata" = NivelToma_PocGata_15m)
write.xlsx(DS_list, file = "C:/Data Science/ArhivosGenerados/Nivel y Caudal Conelectricas.xlsx")
