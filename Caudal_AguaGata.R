
Toma <- read.csv2("Pocosol Nivel Tomas.txt", stringsAsFactors = F)
Toma$TIME <- as.numeric(as.POSIXct(Toma$TIME, tz = "GMT"))
Toma <- Toma %>% filter(TIME <= fecha_final & TIME >= fecha_inicio)

Toma$Poc_Nivel01 <- NULL
Toma$Poc_Flag01 <- NULL
names(Toma)[1] <- "Hora"
names(Toma)[2] <- "Nivel01"
names(Toma)[3] <- "Flag01"
names(Toma)[4] <- "Nivel02"
names(Toma)[5] <- "Flag02"

Toma01 <- Toma %>% 
  filter(!is.na(Nivel01)) %>%
  mutate(Hora = Hora,
         Nivel = Nivel01,
         Flag = Flag01) %>%
  select(Hora, Nivel, Flag)

Toma02 <- Toma %>% 
  filter(!is.na(Nivel02)) %>%
  mutate(Hora = Hora,
         Nivel = Nivel02,
         Flag = Flag02) %>%
  select(Hora, Nivel, Flag)


Pot01 <- Toma %>% 
  filter(!is.na(AG_Pot01)) %>%
  mutate(Hora = Hora,
         Pot = AG_Pot01) %>%
  select(Hora, Pot)

Pot02 <- Toma %>% 
  filter(!is.na(AG_Pot02)) %>%
  mutate(Hora = Hora,
         Pot = AG_Pot02) %>%
  select(Hora, Pot)

Pot03 <- Toma %>% 
  filter(!is.na(AG_Pot03)) %>%
  mutate(Hora = Hora,
         Pot = AG_Pot03) %>%
  select(Hora, Pot)

Toma <- rbind(Toma01, Toma02)
Pot <- rbind(Pot01, Pot02, Pot03)

Toma <- fechas_rango %>% left_join(Toma, by = "Hora") %>% 
  left_join(Pot, by = "Hora")

rm(Toma01, Toma02)
rm(Pot01, Pot02, Pot03, Pot)

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

# potencia
NAs <- which(is.na(Toma$Pot))
for (i in NAs) {
  if (i == 1) {
    Toma$Pot[i] = 0
  }
  else{
    Toma$Pot[i] = Toma$Pot[i - 1]
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
  filter(minutos > 30)

# TelemFailedRankingGroup$Hora <- as.POSIXct(TelemFailedRankingGroup$Hora, 
#                                               origin = "1970-01-01",
#                                               tz = "GMT")

#################################################
# si se desea eliminar los telemetry failed del punto anterior

# ABorrar <- Toma_TelemFailed %>%
#   filter(Rank %in% TelemFailedRankingGroup$Rank) %>%
#   select(Hora)
# 
# Toma <- Toma %>%
#   filter(!Hora %in% (ABorrar$Hora))
# 


#################################################
# Calculo del nivel sobre cresta y Caudal

Toma$NivelSobreCresta <- ifelse(Toma$Nivel > 531.5,
                                Toma$Nivel - 531.5,
                                0)
Toma$Caudal <- 18.75 * sqrt(Toma$NivelSobreCresta ^ 3)


#################################################
# Unir en grupos de 15 minutos (900 segs)

Toma_AG_15m <- Toma %>% 
  mutate(Hora = (Hora %/% 900)*900) %>% 
  group_by(Hora) %>%
  summarise(Fecha_Hora = min(Hora),
            Nivel = mean(Nivel),
            Caudal = mean(Caudal),
            Pot = mean(Pot)) %>%
  select(Fecha_Hora, 
         Nivel, 
         Caudal,
         Pot)


Toma_AG_1d <- Toma %>% 
  mutate(Hora = (Hora %/% 86400)*86400) %>% 
  group_by(Hora) %>%
  summarise(Fecha_Hora = min(Hora),
            Nivel = mean(Nivel),
            Caudal = mean(Caudal),
            Pot = mean(Pot)) %>%
  select(Fecha_Hora, 
         Nivel, 
         Caudal,
         Pot)


Toma_AG_15m$Fecha_Hora <- as.POSIXct(Toma_AG_15m$Fecha_Hora,
                                      origin = "1970-01-01",
                                      tz = "GMT")

Toma_AG_1d$Fecha_Hora <- as.POSIXct(Toma_AG_1d$Fecha_Hora,
                                     origin = "1970-01-01",
                                     tz = "GMT")

Toma_AG_1m <- Toma_AG_1d %>%
  group_by(anho = year(Fecha_Hora), 
           mes = month(Fecha_Hora)) %>%
  summarise(Fecha_Hora = min(Fecha_Hora),
            Nivel = mean(Nivel),
            Caudal = mean(Caudal)) %>%
  ungroup() %>%
  select(Fecha_Hora,
         Nivel,
         Caudal)


# plot(Toma_AG_1d$Fecha_Hora, Toma_AG_1d$Nivel, type = "o",)
# plot(Toma_AG_1d$Fecha_Hora, Toma_AG_1d$Caudal, type = "o",)

rm(Toma_TelemFailed, 
   TelemFailedRankingGroup, 
   Toma,
   i,
   n,
   NAs)
