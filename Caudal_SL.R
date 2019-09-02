
SL_Toma <- read.csv("San Lorenzo Nivel Toma.txt", stringsAsFactors = F)
SL_Toma$TIME <- as.numeric(as.POSIXct(SL_Toma$TIME, tz = "GMT"))
SL_Toma <- fechas_rango %>% left_join(SL_Toma, by = "TIME")

# 3 datos por cada quince minutos
initial_Rows = length(SL_Toma$TIME)/3
#################################################
# Union de columnas

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
rm(SL_Toma01, SL_Toma02, SL_Toma03)


#################################################
# NAs suplantacion

SL_NAs <- which(is.na(NivelToma_SL$Nivel))
for (i in SL_NAs) {
  if (i == 1) {
    NivelToma_SL$Nivel[i] = 0
    NivelToma_SL$Flag[i] = 0
  }
  else{
    NivelToma_SL$Nivel[i] = NivelToma_SL$Nivel[i - 1]
    NivelToma_SL$Flag[i] = NivelToma_SL$Flag[i - 1]
  }
}

#################################################
# Fallas de telemetria y valores manuales
NivelToma_SL_TelemFailed <- NivelToma_SL %>% filter(Flag != 1)
NivelToma_SL_TelemFailed <- cbind(NivelToma_SL_TelemFailed, 
                                  rep(0, nrow(NivelToma_SL_TelemFailed)))
names(NivelToma_SL_TelemFailed)[4] = "Rank"

n = 1
for (i in 1:(nrow(NivelToma_SL_TelemFailed) - 1)) {
  NivelToma_SL_TelemFailed$Rank[i] = n
  if ((NivelToma_SL_TelemFailed$Hora[i] + 300) != NivelToma_SL_TelemFailed$Hora[i + 1]) {
    n = n + 1
  }
}

#################################################
# si se desea eliminar los telemetry failed del punto anterior

SL_TelemFailedRankingGroup <- NivelToma_SL_TelemFailed %>%
  group_by(Rank) %>%
  summarise(Hora = min(Hora), 
            minutos = 5 * n()) %>%
  filter(minutos > 30)

SL_TelemFailedRankingGroup$Hora <- as.POSIXct(SL_TelemFailedRankingGroup$Hora, 
                                              origin = "1970-01-01",
                                              tz = "GMT")

SL_ABorrar <- NivelToma_SL_TelemFailed %>%
  filter(Rank %in% SL_TelemFailedRankingGroup$Rank) %>%
  select(Hora)

NivelToma_SL <- NivelToma_SL %>% 
  filter(!Hora %in% (SL_ABorrar$Hora))

NivelToma_SL %>% filter(Flag != 1)

#################################################
# Calculo del nivel sobre cresta y Caudal

NivelToma_SL$NivelSobreCresta <- ifelse(NivelToma_SL$Nivel > 0, 
                                        NivelToma_SL$Nivel / 100, 
                                        0)
NivelToma_SL$Caudal <- 94.047 * sqrt(NivelToma_SL$NivelSobreCresta ^ 3)

#################################################
# Unir en grupos de 15 minutos (900 segs)

Toma_SL_15m <- NivelToma_SL %>% 
  mutate(Hora = (Hora %/% 900)*900) %>% 
  group_by(Hora) %>%
  summarise(Fecha_Hora = min(Hora),
            Nivel = mean(Nivel),
            CaudalAVG = mean(Caudal)) %>%
  select(Fecha_Hora, 
         Nivel, 
         CaudalAVG)

Toma_SL_15m$Fecha_Hora <- as.POSIXct(Toma_SL_15m$Fecha_Hora,
                                     origin = "1970-01-01",
                                     tz = "GMT")

end_Rows = length(Toma_SL_15m$Fecha_Hora)

SL_porc_eliminados <- round(100 * (initial_Rows - end_Rows) / initial_Rows, 2)


rm(NivelToma_SL_TelemFailed, 
   SL_TelemFailedRankingGroup, 
   SL_ABorrar, 
   SL_Toma, 
   NivelToma_SL,
   i,
   n,
   SL_NAs, 
   initial_Rows,
   end_Rows)
