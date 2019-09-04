
Toma <- read.csv2("Bijagua Nivel Tomas.txt", stringsAsFactors = F)
Toma$TIME <- as.numeric(as.POSIXct(Toma$TIME, tz = "GMT"))
Toma <- Toma %>% filter(TIME <= fecha_final & TIME >= fecha_inicio)

Toma$BIJAGUA_SUB.NIVEL_DE_TOMA_BIJAGUA.VAL <- NULL
Toma$BIJAGUA_SUB.NIVEL_DE_TOMA_BIJAGUA.VAL.Q <- NULL
Toma$BIJAGUA_SUB.NIVEL_DE_TOMA_ZAPOTE.VAL <- NULL
Toma$BIJAGUA_SUB.NIVEL_DE_TOMA_ZAPOTE.VAL.Q <- NULL
Toma$BIJAGUA_SUB.NIVEL_RIO_ZAPOTE.VAL <- NULL
Toma$BIJAGUA_SUB.NIVEL_RIO_ZAPOTE.VAL.Q <- NULL

names(Toma)[1] <- "Hora"
names(Toma)[2] <- "Nivel"
names(Toma)[3] <- "Flag"

# Rellenar espacios faltantes
Toma <- fechas_rango %>% left_join(Toma, by = "Hora")
Toma$Nivel <- ifelse(Toma$Nivel > 500, NA, Toma$Nivel)

# plot(Toma$Hora, Toma$NivelSobreCresta)
# boxplot(Toma$Nivel)
# hist(Toma$Nivel)


#################################################
# NAs suplantacion

NAs <- which(is.na(Toma$Nivel))
for (i in NAs) {
  if (i == 1) {
    Toma$Nivel[i] = 0
    Toma$Flag[i] = 0
  }
  else{
    Toma$Nivel[i] = Toma$Nivel[i - 1]
    Toma$Flag[i] = Toma$Flag[i - 1]
  }
}

#################################################
# Fallas de telemetria y valores manuales
Toma_TelemFailed <- Toma %>% filter(Flag != 1)
Toma_TelemFailed <- cbind(Toma_TelemFailed,
                          rep(0, nrow(Toma_TelemFailed)))
names(Toma_TelemFailed)[4] = "Rank"

n = 1
for (i in 1:(nrow(Toma_TelemFailed) - 1)) {
  Toma_TelemFailed$Rank[i] = n
  if ((Toma_TelemFailed$Hora[i] + 300) != Toma_TelemFailed$Hora[i + 1]) {
    n = n + 1
  }
}


#################################################
# si se desea AGRUPAR los telemetry failed del punto anterior con base en la duracion

TelemFailedRankingGroup <- Toma_TelemFailed %>%
  group_by(Rank) %>%
  summarise(Hora = min(Hora),
            minutos = 5 * n()) %>%
  filter(minutos > 60)

#################################################
# si se desea eliminar los telemetry failed del punto anterior
# se decide bajarlos a -1

ABorrar <- Toma_TelemFailed %>%
  filter(Rank %in% TelemFailedRankingGroup$Rank) %>%
  select(Hora)

Toma$Nivel <- ifelse(Toma$Hora %in% ABorrar$Hora	, -1, Toma$Nivel)

#################################################
# Calculo del nivel sobre cresta y Caudal
# se tiene una modificacion en esta lecturas 
# paso de msnm a nivel sobre cresta

Toma$NivelSobreCresta <- ifelse(Toma$Nivel > 400, 
                                        Toma$Nivel - 409.8, 
                                        Toma$Nivel)

Toma$NivelSobreCresta <- ifelse(Toma$NivelSobreCresta > 0, 
                                    Toma$NivelSobreCresta, 
                                    0)
# el sensor estubo malo y marcaba maxima lectura
Toma$NivelSobreCresta <- ifelse(Toma$NivelSobreCresta > 5, 
                                0, 
                                Toma$NivelSobreCresta)

# Calculo de Caudal
Toma$Caudal <- 69.06852 * sqrt(Toma$NivelSobreCresta ^ 3)


#################################################
# Unir en grupos de 15 minutos (900 segs)

Toma_Bij_15m <- Toma %>% 
  mutate(Hora = (Hora %/% 900)*900) %>% 
  group_by(Hora) %>%
  summarise(Fecha_Hora = min(Hora),
            Nivel = mean(Nivel),
            Caudal = mean(Caudal)) %>%
  select(Fecha_Hora, 
         Nivel, 
         Caudal)


Toma_Bij_1d <- Toma %>% 
  mutate(Hora = (Hora %/% 86400)*86400) %>% 
  group_by(Hora) %>%
  summarise(Fecha_Hora = min(Hora),
            Nivel = mean(Nivel),
            Caudal = mean(Caudal)) %>%
  select(Fecha_Hora, 
         Nivel, 
         Caudal)
# Toma$Hora <- as.POSIXct(Toma$Hora,
#                                       origin = "1970-01-01",
#                                       tz = "GMT")

Toma_Bij_15m$Fecha_Hora <- as.POSIXct(Toma_Bij_15m$Fecha_Hora,
                                      origin = "1970-01-01",
                                      tz = "GMT")

Toma_Bij_1d$Fecha_Hora <- as.POSIXct(Toma_Bij_1d$Fecha_Hora,
                                     origin = "1970-01-01",
                                     tz = "GMT")

Toma_Bij_1m <- Toma_Bij_1d %>%
  group_by(anho = year(Fecha_Hora), 
           mes = month(Fecha_Hora)) %>%
  summarise(Fecha_Hora = min(Fecha_Hora),
            Nivel = mean(Nivel),
            Caudal = mean(Caudal)) %>%
  ungroup() %>%
  select(Fecha_Hora,
         Nivel,
         Caudal)


# plot(Toma_Bij_1d$Fecha_Hora, Toma_Bij_1d$Nivel, type = "o",)
# plot(Toma_Bij_1d$Fecha_Hora, Toma_Bij_1d$Caudal, type = "o",)

rm(Toma_TelemFailed, 
   TelemFailedRankingGroup, 
   Toma,
   i,
   n,
   NAs)
