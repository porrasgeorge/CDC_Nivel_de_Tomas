
library(tidyverse)
library(openxlsx)
library(lubridate)



###############################################################################################################################
# San Lorenzo


SL_Toma <- read.csv("San Lorenzo Nivel Toma.txt", stringsAsFactors = F)
SL_Toma$TIME <- as.POSIXct(SL_Toma$TIME)

# los datos vienen de 3 diferentes columnas de la tabla en la Base de datos,
# debido al cambio de nombre en la variable de Survalent
SL_Toma01 <- SL_Toma %>% 
  filter(!is.na(Nivel01)) %>%
  mutate(Hora = TIME,
         Nivel = Nivel01,
         Flag = Flag01) %>%
  select(Hora, Nivel, Flag)

SL_Toma02 <- SL_Toma %>% 
  filter(!is.na(Nivel02)) %>%
  mutate(Hora = TIME,
         Nivel = Nivel02,
         Flag = Flag02) %>%
  select(Hora, Nivel, Flag)

SL_Toma03 <- SL_Toma %>% 
  filter(!is.na(Nivel03)) %>%
  mutate(Hora = TIME,
         Nivel = Nivel03,
         Flag = Flag03) %>%
  select(Hora, Nivel, Flag)

NivelToma_SL <- rbind(SL_Toma01, SL_Toma02, SL_Toma03)
rm(SL_Toma, SL_Toma01, SL_Toma02, SL_Toma03)


NivelToma_SL$NivelSobreCresta <- ifelse(NivelToma_SL$Nivel>0, NivelToma_SL$Nivel/100, 0)
NivelToma_SL$Caudal <- 94.047 * sqrt(NivelToma_SL$NivelSobreCresta ^ 3)
NivelToma_SL$VolumenVertido <- 300* NivelToma_SL$Caudal


NivelToma_SL <- NivelToma_SL %>% arrange(Hora) %>% 
  mutate(anho = year(Hora), 
         mes = month(Hora),
         dia = day(Hora),
         hora = hour(Hora)) %>%
  group_by(anho, mes, dia, hora) %>%
  summarise(CaudalAVG = mean(Caudal),
            VolumenVertidoT = sum(VolumenVertido))


###############################################################################################################################
# Pocosol


Poc_Toma <- read.csv2("Pocosol Nivel Tomas.txt", stringsAsFactors = F)
Poc_Toma$TIME <- as.POSIXct(Poc_Toma$TIME)


# los datos vienen de 3 diferentes columnas de la tabla en la Base de datos,
# debido al cambio de nombre en la variable de Survalent
AG_Toma01 <- Poc_Toma %>% 
  filter(!is.na(AG_Nivel01)) %>%
  mutate(Hora = TIME,
         AG_Nivel = AG_Nivel01,
         AG_Flag = AG_Flag01) %>%
  select(Hora, AG_Nivel, AG_Flag)

AG_Toma02 <- Poc_Toma %>% 
  filter(!is.na(AG_Nivel02)) %>%
  mutate(Hora = TIME,
         AG_Nivel = AG_Nivel02,
         AG_Flag = AG_Flag02) %>%
  select(Hora, AG_Nivel, AG_Flag)
NivelTomaAG <- rbind(AG_Toma01, AG_Toma02)

NivelTomaPoc <- Poc_Toma %>% 
  filter(!is.na(Poc_Nivel01)) %>%
  mutate(Hora = TIME,
         Poc_Nivel = Poc_Nivel01,
         Poc_Flag = Poc_Flag01) %>%
  select(Hora, Poc_Nivel, Poc_Flag)

NivelToma_PocGata <- NivelTomaPoc %>% right_join(NivelTomaAG, by = "Hora")
rm(NivelTomaPoc, AG_Toma01, AG_Toma02, NivelTomaAG)

#cantidad de NAs
sum(is.na(NivelToma_PocGata))

## faltan 15 min de datos que se encuentran juntos
# que hacer? ponerles el valor anterior
Poc_NAs <- which(is.na(NivelToma_PocGata$Poc_Nivel))
for (i in Poc_NAs){
  if (i == 1){
    NivelToma_PocGata$Poc_Nivel[i] = 0
    NivelToma_PocGata$Poc_Flag[i] = 0
  }
  else{
    NivelToma_PocGata$Poc_Nivel[i] = NivelToma_PocGata$Poc_Nivel[i-1]
    NivelToma_PocGata$Poc_Flag[i] = NivelToma_PocGata$Poc_Flag[i-1]
  }
}

rm(Poc_NAs, i)

NivelToma_PocGata$Poc_NivelSobreCresta <- ifelse(NivelToma_PocGata$Poc_Nivel > 457, 
                                                 NivelToma_PocGata$Poc_Nivel - 457, 
                                                 0)
NivelToma_PocGata$Poc_Caudal <- 93.555 * sqrt(NivelToma_PocGata$Poc_NivelSobreCresta ^ 3)
NivelToma_PocGata$Poc_VolumenVertido <- 300* NivelToma_PocGata$Poc_Caudal

NivelToma_PocGata$AG_NivelSobreCresta <- ifelse(NivelToma_PocGata$AG_Nivel > 531.5, 
                                                 NivelToma_PocGata$AG_Nivel - 531.5, 
                                                 0)
NivelToma_PocGata$AG_Caudal <- 18.75 * sqrt(NivelToma_PocGata$AG_NivelSobreCresta ^ 3)
NivelToma_PocGata$AG_VolumenVertido <- 300* NivelToma_PocGata$AG_Caudal



## Falta ver perdidas de comunicacion 

t1 = Sys.time()
NivelToma_PocGata_Hour <- NivelToma_PocGata

t2 = Sys.time()
t2 - t1
NivelToma_PocGata <- NivelToma_PocGata %>% 
  mutate(anho = year(Hora), 
         mes = month(Hora),
         dia = day(Hora),
         hora = hour(Hora),
         minuto = minute(Hora) %/% 15) 

t3 = Sys.time()
t3 - t2
NivelToma_PocGata <- NivelToma_PocGata %>% 
  group_by(anho, mes, dia, hora, minuto) %>%
  summarise(Fecha_Hora = Hora[1],
            Nivel = mean(Poc_Nivel),
            Poc_CaudalAVG = mean(Poc_Caudal),
            AG_CaudalAVG = mean(AG_Caudal))
t4 = Sys.time()
t4 - t3

hour(as.POSIXct(as.numeric(Sys.time())))


t2 - t1
t3 - t2
t4 - t3

NivelToma_PocGata_Hour <- NivelToma_PocGata_Hour %>% 
  ungroup() %>%
  select(Fecha_Hora, 
         Poc_CaudalAVG, 
         Poc_VolumenVertidoT,
         AG_CaudalAVG,
         AG_VolumenVertidoT)
  


##    ver otro SCRIPT TEST FECHA HORA

###################
#hasta aqui llegue


DS_list <- list("San Lorenzo" = NivelToma_SL, "Pocosol_Gata" = NivelToma_PocGata)
write.xlsx(DS_list, file = "C:/Data Science/ArhivosGenerados/Nivel y Caudal Conelectricas.xlsx")

