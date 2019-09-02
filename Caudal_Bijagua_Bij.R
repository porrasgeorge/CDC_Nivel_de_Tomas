
Bij_Toma <- read.csv2("Bijagua Nivel Tomas.txt", stringsAsFactors = F)
Bij_Toma$TIME <- as.numeric(as.POSIXct(Bij_Toma$TIME, tz = "GMT"))

Bij_Toma$BIJAGUA_SUB.NIVEL_DE_TOMA_BIJAGUA.VAL <- NULL
Bij_Toma$BIJAGUA_SUB.NIVEL_DE_TOMA_BIJAGUA.VAL.Q <- NULL
Bij_Toma$BIJAGUA_SUB.NIVEL_DE_TOMA_ZAPOTE.VAL <- NULL
Bij_Toma$BIJAGUA_SUB.NIVEL_DE_TOMA_ZAPOTE.VAL.Q <- NULL
Bij_Toma$BIJAGUA_SUB.NIVEL_RIO_ZAPOTE.VAL <- NULL
Bij_Toma$BIJAGUA_SUB.NIVEL_RIO_ZAPOTE.VAL.Q <- NULL

names(Bij_Toma)[1] <- "Hora"
names(Bij_Toma)[2] <- "Nivel"
names(Bij_Toma)[3] <- "Flag"

#################################################+
# Solo los datos de noche de 20:00 hasta 5:55

Bij_Toma <- Bij_Toma %>% filter((Hora %% 86400) %in% c(seq(0,21300,300), seq(72000, 86100, 300)))
# Bij_Toma$Hora <- as.POSIXct(Bij_Toma$Hora,
#                                           origin = "1970-01-01",
#                                           tz = "GMT")
# 3 datos por cada quince minutos
initial_Rows = length(Bij_Toma$Hora)/3
#################################################
# NAs suplantacion

SL_NAs <- which(is.na(Bij_Toma$Nivel))
for (i in SL_NAs) {
  if (i == 1) {
    Bij_Toma$Nivel[i] = 0
    Bij_Toma$Flag[i] = 0
  }
  else{
    Bij_Toma$Nivel[i] = Bij_Toma$Nivel[i - 1]
    Bij_Toma$Flag[i] = Bij_Toma$Flag[i - 1]
  }
}

#################################################
# Fallas de telemetria y valores manuales
Bij_Toma_TelemFailed <- Bij_Toma %>% filter(Flag != 1)
Bij_Toma_TelemFailed <- cbind(Bij_Toma_TelemFailed, 
                                  rep(0, nrow(Bij_Toma_TelemFailed)))
names(Bij_Toma_TelemFailed)[4] = "Rank"

n = 1
for (i in 1:(nrow(Bij_Toma_TelemFailed) - 1)) {
  Bij_Toma_TelemFailed$Rank[i] = n
  if ((Bij_Toma_TelemFailed$Hora[i] + 300) != Bij_Toma_TelemFailed$Hora[i + 1]) {
    n = n + 1
  }
}

#################################################
# si se desea eliminar los telemetry failed del punto anterior

Bij_TelemFailedRankingGroup <- Bij_Toma_TelemFailed %>%
  group_by(Rank) %>%
  summarise(Hora = min(Hora), 
            minutos = 5 * n()) %>%
  filter(minutos > 30)

Bij_TelemFailedRankingGroup$Hora <- as.POSIXct(Bij_TelemFailedRankingGroup$Hora, 
                                              origin = "1970-01-01",
                                              tz = "GMT")

Bij_ABorrar <- Bij_Toma_TelemFailed %>%
  filter(Rank %in% Bij_TelemFailedRankingGroup$Rank) %>%
  select(Hora)

Bij_Toma <- Bij_Toma %>% 
  filter(!Hora %in% (Bij_ABorrar$Hora))

#################################################
# Calculo del nivel sobre cresta y Caudal
# se tiene una modificacion en esta lecturas 
# paso de msnm a nivel sobre cresta

Bij_Toma$NivelSobreCresta <- ifelse(Bij_Toma$Nivel > 400, 
                                        Bij_Toma$Nivel - 409.8, 
                                        Bij_Toma$Nivel)

Bij_Toma$NivelSobreCresta <- ifelse(Bij_Toma$NivelSobreCresta > 0, 
                                    Bij_Toma$NivelSobreCresta, 
                                    0)

# el sensor estubo malo y marcaba maxima lectura
# se eliminan lecturas no logicas
Bij_Toma <- Bij_Toma %>% filter(NivelSobreCresta < 5)

# Calculo de Caudal
Bij_Toma$Caudal <- 69.06852 * sqrt(Bij_Toma$NivelSobreCresta ^ 3)


#################################################
# Unir en grupos de 15 minutos (900 segs)

Toma_Bij_15m <- Bij_Toma %>% 
  mutate(Hora = (Hora %/% 900)*900) %>% 
  group_by(Hora) %>%
  summarise(Fecha_Hora = min(Hora),
            Nivel = mean(Nivel),
            CaudalAVG = mean(Caudal)) %>%
  select(Fecha_Hora, 
         Nivel, 
         CaudalAVG)

Toma_Bij_15m$Fecha_Hora <- as.POSIXct(Toma_Bij_15m$Fecha_Hora,
                                          origin = "1970-01-01",
                                          tz = "GMT")
end_Rows = length(Toma_Bij_15m$Fecha_Hora)

Bijagua_porc_eliminados <- round(100 * (initial_Rows - end_Rows) / initial_Rows, 2)


rm(Bij_Toma_TelemFailed, 
   Bij_TelemFailedRankingGroup, 
   Bij_ABorrar, 
   Bij_Toma, 
   i,
   n,
   SL_NAs, 
   initial_Rows,
   end_Rows)
