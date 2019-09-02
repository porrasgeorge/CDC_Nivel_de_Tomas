

Poc_Toma <- read.csv2("Pocosol Nivel Tomas.txt", stringsAsFactors = F)
Poc_Toma$TIME <- as.numeric(as.POSIXct(Poc_Toma$TIME, tz = "GMT"))

#################################################
# Solo los datos de noche de 20:00 hasta 5:55

Poc_Toma <- Poc_Toma %>% filter((TIME %% 86400) %in% c(seq(0,21300,300), seq(72000, 86100, 300)))
# Poc_Toma$TIME <- as.POSIXct(Poc_Toma$TIME,
#                                           origin = "1970-01-01",
#                                           tz = "GMT")

# 3 datos por cada quince minutos
initial_Rows = length(Poc_Toma$TIME)/3

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

NivelToma_PocGata <- NivelTomaPoc %>% 
  right_join(NivelTomaAG, by = "Hora")

rm(NivelTomaPoc, AG_Toma01, AG_Toma02, NivelTomaAG)

#################################################
# NAs suplantacion

Poc_NAs <- which(is.na(NivelToma_PocGata$Poc_Nivel))
for (i in Poc_NAs) {
  if (i == 1) {
    NivelToma_PocGata$Poc_Nivel[i] = 0
    NivelToma_PocGata$Poc_Flag[i] = 0
  }
  else{
    NivelToma_PocGata$Poc_Nivel[i] = NivelToma_PocGata$Poc_Nivel[i - 1]
    NivelToma_PocGata$Poc_Flag[i] = NivelToma_PocGata$Poc_Flag[i - 1]
  }
}


#################################################
# Fallas de telemetria y valores manuales
NivelToma_Poc_TelemFailed <- NivelToma_PocGata %>% 
  filter(Poc_Flag != 1)

NivelToma_Poc_TelemFailed <- cbind(NivelToma_Poc_TelemFailed, 
                                   rep(0, nrow(NivelToma_Poc_TelemFailed)))

names(NivelToma_Poc_TelemFailed)[6] = "Rank"

n = 1
for (i in 1:(nrow(NivelToma_Poc_TelemFailed) - 1)) {
  NivelToma_Poc_TelemFailed$Rank[i] = n
  if ((NivelToma_Poc_TelemFailed$Hora[i] + 300) != NivelToma_Poc_TelemFailed$Hora[i + 1]) {
    n = n + 1
  }
}


#################################################
# si se desea eliminar los telemetry failed del punto anterior

Poc_TelemFailedRankingGroup <- NivelToma_Poc_TelemFailed %>%
  group_by(Rank) %>%
  summarise(Hora = min(Hora), minutos = 5 * n()) %>%
  filter(minutos > 30)

Poc_TelemFailedRankingGroup$Hora <- as.POSIXct(Poc_TelemFailedRankingGroup$Hora, 
                                               origin = "1970-01-01",
                                               tz = "GMT")

Poc_ABorrar <- NivelToma_Poc_TelemFailed %>% 
  filter(Rank %in% Poc_TelemFailedRankingGroup$Rank) %>% 
  select(Hora)

NivelToma_PocGata <- NivelToma_PocGata %>% 
  filter(!Hora %in% (Poc_ABorrar$Hora))

NivelToma_PocGata %>% filter(Poc_Flag != 1)


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

#################################################
# Unir en grupos de 15 minutos (900 segs)

Toma_PocGata_15m <- NivelToma_PocGata %>% 
  mutate(Hora = (Hora %/% 900)*900) %>% 
  group_by(Hora) %>%
  summarise(Fecha_Hora = min(Hora),
            Poc_Nivel = mean(Poc_Nivel),
            Poc_CaudalAVG = mean(Poc_Caudal),
            AG_Nivel = mean(AG_Nivel),
            AG_CaudalAVG = mean(AG_Caudal)) %>%
  select(Fecha_Hora, 
         Poc_Nivel, 
         Poc_CaudalAVG, 
         AG_Nivel, 
         AG_CaudalAVG)

#################################################
# Unir en grupos de 1 dia

# NivelToma_PocGata_1dia <- NivelToma_PocGata %>% 
#   mutate(Hora = (Hora %/% 86400)*86400) %>% 
#   group_by(Hora) %>%
#   summarise(Fecha_Hora = min(Hora),
#             Poc_Nivel = mean(Poc_Nivel),
#             Poc_CaudalAVG = mean(Poc_Caudal),
#             AG_Nivel = mean(AG_Nivel),
#             AG_CaudalAVG = mean(AG_Caudal)) %>%
#   select(Fecha_Hora, 
#          Poc_Nivel, 
#          Poc_CaudalAVG, 
#          AG_Nivel, 
#          AG_CaudalAVG)


#################################################
# Volver al formato de fecha (se requiere para agrupar por mes)


Toma_PocGata_15m$Fecha_Hora <- as.POSIXct(Toma_PocGata_15m$Fecha_Hora,
                                          origin = "1970-01-01",
                                          tz = "GMT")

# 
# NivelToma_PocGata_1dia$Fecha_Hora <- as.POSIXct(NivelToma_PocGata_1dia$Fecha_Hora,
#                                                 origin = "1970-01-01",
#                                                 tz = "GMT")

# NivelToma_PocGata$Hora <- as.POSIXct(NivelToma_PocGata$Hora, 
#                                            origin = "1970-01-01", 
#                                      tz = "GMT")
# 

#################################################
# Unir en grupos de 1 mes

# NivelToma_PocGata_1mes <- NivelToma_PocGata_1dia %>% 
#   group_by(anho = year(Fecha_Hora), mes = month(Fecha_Hora)) %>%
#   summarise(Fecha_Hora = min(Fecha_Hora),
#             Poc_Nivel = mean(Poc_Nivel),
#             Poc_CaudalAVG = mean(Poc_CaudalAVG),
#             AG_Nivel = mean(AG_Nivel),
#             AG_CaudalAVG = mean(AG_CaudalAVG)) %>%
#   ungroup() %>%
#   select(Fecha_Hora, 
#          Poc_Nivel, 
#          Poc_CaudalAVG, 
#          AG_Nivel, 
#          AG_CaudalAVG)


end_Rows = length(Toma_PocGata_15m$Fecha_Hora)
Poc_porc_eliminados <- round(100 * (initial_Rows - end_Rows) / initial_Rows, 2)

rm(Poc_TelemFailedRankingGroup, 
   i, 
   n, 
   NivelToma_Poc_TelemFailed, 
   Poc_ABorrar,
   NivelToma_PocGata,
   Poc_Toma,
   Poc_NAs, 
   initial_Rows,
   end_Rows)

