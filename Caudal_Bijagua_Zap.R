
Zap_Toma <- read.csv2("Bijagua Nivel Tomas.txt", stringsAsFactors = F)
Zap_Toma$TIME <- as.numeric(as.POSIXct(Zap_Toma$TIME, tz = "GMT"))

Zap_Toma$BIJAGUA_SUB.NIVEL_DE_TOMA_BIJAGUA.VAL <- NULL
Zap_Toma$BIJAGUA_SUB.NIVEL_DE_TOMA_BIJAGUA.VAL.Q <- NULL
Zap_Toma$BIJAGUA_SUB.NIVEL_DE_TOMA_ZAPOTE.VAL <- NULL
Zap_Toma$BIJAGUA_SUB.NIVEL_DE_TOMA_ZAPOTE.VAL.Q <- NULL
Zap_Toma$BIJAGUA_SUB.NIVEL_RIO_BIJAGUA.VAL <- NULL
Zap_Toma$BIJAGUA_SUB.NIVEL_RIO_BIJAGUA.VAL.Q <- NULL

names(Zap_Toma)[1] <- "Hora"
names(Zap_Toma)[2] <- "Nivel"
names(Zap_Toma)[3] <- "Flag"

#################################################+
# Solo los datos de noche de 20:00 hasta 5:55

Zap_Toma <- Zap_Toma %>% filter((Hora %% 86400) %in% c(seq(0,21300,300), seq(72000, 86100, 300)))
# Zap_Toma$Hora <- as.POSIXct(Zap_Toma$Hora,
#                                           origin = "1970-01-01",
#                                           tz = "GMT")

# 3 datos por cada quince minutos
initial_Rows = length(Zap_Toma$Hora)/3
#################################################
# NAs suplantacion

SL_NAs <- which(is.na(Zap_Toma$Nivel))
for (i in SL_NAs) {
  if (i == 1) {
    Zap_Toma$Nivel[i] = 0
    Zap_Toma$Flag[i] = 0
  }
  else{
    Zap_Toma$Nivel[i] = Zap_Toma$Nivel[i - 1]
    Zap_Toma$Flag[i] = Zap_Toma$Flag[i - 1]
  }
}

#################################################
# Fallas de telemetria y valores manuales
Zap_Toma_TelemFailed <- Zap_Toma %>% filter(Flag != 1)
Zap_Toma_TelemFailed <- cbind(Zap_Toma_TelemFailed, 
                              rep(0, nrow(Zap_Toma_TelemFailed)))
names(Zap_Toma_TelemFailed)[4] = "Rank"

n = 1
for (i in 1:(nrow(Zap_Toma_TelemFailed) - 1)) {
  Zap_Toma_TelemFailed$Rank[i] = n
  if ((Zap_Toma_TelemFailed$Hora[i] + 300) != Zap_Toma_TelemFailed$Hora[i + 1]) {
    n = n + 1
  }
}

#################################################
# si se desea eliminar los telemetry failed del punto anterior

Zap_TelemFailedRankingGroup <- Zap_Toma_TelemFailed %>%
  group_by(Rank) %>%
  summarise(Hora = min(Hora), 
            minutos = 5 * n()) %>%
  filter(minutos > 30)

Zap_TelemFailedRankingGroup$Hora <- as.POSIXct(Zap_TelemFailedRankingGroup$Hora, 
                                               origin = "1970-01-01",
                                               tz = "GMT")

Bij_ABorrar <- Zap_Toma_TelemFailed %>%
  filter(Rank %in% Zap_TelemFailedRankingGroup$Rank) %>%
  select(Hora)

Zap_Toma <- Zap_Toma %>% 
  filter(!Hora %in% (Bij_ABorrar$Hora))

#################################################
# Calculo del nivel sobre cresta y Caudal
# se tiene una modificacion en esta lecturas 
# paso de msnm a nivel sobre cresta

# nivel minimo aceptado
Zap_Toma$NivelSobreCresta <- ifelse(Zap_Toma$Nivel > 413, 
                                    Zap_Toma$Nivel - 419.5, 
                                    Zap_Toma$Nivel)

Zap_Toma$NivelSobreCresta <- ifelse(Zap_Toma$NivelSobreCresta > 0, 
                                    Zap_Toma$NivelSobreCresta, 
                                    0)

# el sensor estubo malo y marcaba maxima lectura
# se eliminan lecturas no logicas que el sensor no mide
Zap_Toma <- Zap_Toma %>% filter(NivelSobreCresta < 3.0)

# Calculo de Caudal
Zap_Toma$Caudal <- 59.916 * sqrt(Zap_Toma$NivelSobreCresta ^ 3)

#################################################
# Unir en grupos de 15 minutos (900 segs)

Toma_Zap_15m <- Zap_Toma %>% 
  mutate(Hora = (Hora %/% 900)*900) %>% 
  group_by(Hora) %>%
  summarise(Fecha_Hora = min(Hora),
            Nivel = mean(Nivel),
            CaudalAVG = mean(Caudal)) %>%
  select(Fecha_Hora, 
         Nivel, 
         CaudalAVG)

Toma_Zap_15m$Fecha_Hora <- as.POSIXct(Toma_Zap_15m$Fecha_Hora,
                                      origin = "1970-01-01",
                                      tz = "GMT")

end_Rows = length(Toma_Zap_15m$Fecha_Hora)
Zapote_porc_eliminados <- round(100 * (initial_Rows - end_Rows) / initial_Rows, 2)

rm(Zap_Toma_TelemFailed, 
   Zap_TelemFailedRankingGroup, 
   Bij_ABorrar, 
   Zap_Toma, 
   i,
   n,
   SL_NAs, 
   initial_Rows,
   end_Rows)
