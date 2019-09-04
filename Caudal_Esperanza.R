
Toma <- read.csv2("Esperanza Nivel Toma.txt", stringsAsFactors = F)
Toma$TIME <- as.numeric(as.POSIXct(Toma$TIME, tz = "GMT"))
Toma <- Toma %>% filter(TIME <= fecha_final & TIME >= fecha_inicio)

names(Toma)[1] <- "Hora"
names(Toma)[2] <- "Nivel"
names(Toma)[3] <- "Flag"

# Rellenar espacios faltantes
Toma <- fechas_rango %>% left_join(Toma, by = "Hora")

# plot(Toma$Hora, Toma$Nivel)
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

Toma$NivelSobreCresta <- ifelse(Toma$Nivel > 2.43, 
                                Toma$Nivel - 2.43, 
                                0)

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
# Calculo de Caudal
Toma$Caudal <- 37.191 * sqrt(Toma$NivelSobreCresta ^ 3)


#################################################
# Unir en grupos de 15 minutos (900 segs)

Toma_Esp_15m <- Toma %>% 
  mutate(Hora = (Hora %/% 900)*900) %>% 
  group_by(Hora) %>%
  summarise(Fecha_Hora = min(Hora),
            Nivel = mean(Nivel),
            Caudal = mean(Caudal)) %>%
  select(Fecha_Hora, 
         Nivel, 
         Caudal)


Toma_Esp_1d <- Toma %>% 
  mutate(Hora = (Hora %/% 86400)*86400) %>% 
  group_by(Hora) %>%
  summarise(Fecha_Hora = min(Hora),
            Nivel = mean(Nivel),
            Caudal = mean(Caudal)) %>%
  select(Fecha_Hora, 
         Nivel, 
         Caudal)


Toma_Esp_15m$Fecha_Hora <- as.POSIXct(Toma_Esp_15m$Fecha_Hora,
                                      origin = "1970-01-01",
                                      tz = "GMT")

Toma_Esp_1d$Fecha_Hora <- as.POSIXct(Toma_Esp_1d$Fecha_Hora,
                                     origin = "1970-01-01",
                                     tz = "GMT")

Toma_Esp_1m <- Toma_Esp_1d %>%
  group_by(anho = year(Fecha_Hora), 
           mes = month(Fecha_Hora)) %>%
  summarise(Fecha_Hora = min(Fecha_Hora),
            Nivel = mean(Nivel),
            Caudal = mean(Caudal)) %>%
  ungroup() %>%
  select(Fecha_Hora,
         Nivel,
         Caudal)

rm(Toma,
   i,
   NAs,
   ABorrar)
