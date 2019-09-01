t1 = Sys.time()
Poc_Toma <- read.csv2("Pocosol Nivel Tomas.txt", stringsAsFactors = F)
Poc_Toma$TIME <- as.numeric(as.POSIXct(Poc_Toma$TIME))

t2 = Sys.time()

#################################################
# Union de columnas

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

t3 = Sys.time()
#################################################
# NAs suplantacion

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

t4 = Sys.time()
#################################################
# Calculo del nivel sobre cresta y Caudal

NivelToma_PocGata$Poc_NivelSobreCresta <- ifelse(NivelToma_PocGata$Poc_Nivel > 457, 
                                                 NivelToma_PocGata$Poc_Nivel - 457, 
                                                 0)
NivelToma_PocGata$Poc_Caudal <- 93.555 * sqrt(NivelToma_PocGata$Poc_NivelSobreCresta ^ 3)

NivelToma_PocGata$AG_NivelSobreCresta <- ifelse(NivelToma_PocGata$AG_Nivel > 531.5, 
                                                NivelToma_PocGata$AG_Nivel - 531.5, 
                                                0)
NivelToma_PocGata$AG_Caudal <- 18.75 * sqrt(NivelToma_PocGata$AG_NivelSobreCresta ^ 3)

t5 = Sys.time()
#################################################
# Unir en grupos de 15 minutos (900 segs)

NivelToma_PocGata <- NivelToma_PocGata %>% 
  mutate(Hora_15 = (Hora %/% 900)*900) %>% 
  group_by(Hora_15) %>%
  summarise(Fecha_Hora = min(Hora_15),
            Poc_Nivel = mean(Poc_Nivel),
            Poc_CaudalAVG = mean(Poc_Caudal),
            AG_Nivel = mean(AG_Nivel),
            AG_CaudalAVG = mean(AG_Caudal)) %>%
  select(Fecha_Hora, 
         Poc_Nivel, 
         Poc_CaudalAVG, 
         AG_Nivel, 
         AG_CaudalAVG)

t6 = Sys.time()
#################################################
# Volver al formato de fecha

NivelToma_PocGata$Fecha_Hora <- as.POSIXct(NivelToma_PocGata$Fecha_Hora, 
                                           origin = "1970-01-01")

t7 = Sys.time()

t2 - t1
t3 - t2
t4 - t3
t5 - t4
t6 - t5
t7 - t6

print("Total :")
t7 - t1
