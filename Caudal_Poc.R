
Toma <- read.csv2("Pocosol Nivel Tomas.txt", stringsAsFactors = F)
Toma$TIME <- as.numeric(as.POSIXct(Toma$TIME, tz = "GMT"))
Toma <- Toma %>% filter(TIME <= fecha_final & TIME >= fecha_inicio)

Toma$AG_Nivel01 <- NULL
Toma$AG_Nivel02 <- NULL
Toma$AG_Flag01 <- NULL
Toma$AG_Flag02 <- NULL

names(Toma)[1] <- "Hora"
names(Toma)[2] <- "Nivel"
names(Toma)[3] <- "Flag"

Toma <- fechas_rango %>% left_join(Toma, by = "Hora")

Toma$Nivel <- ifelse(Toma$Nivel < 453.5, 454, Toma$Nivel)

# plot(Toma$Hora, Toma$Nivel)
boxplot(Toma$Nivel)

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

Toma$NivelSobreCresta <- ifelse(Toma$Nivel > 457,
                                Toma$Nivel - 457,
                                0)
Toma$Caudal <- 93.555 * sqrt(Toma$NivelSobreCresta ^ 3)

#################################################
# Unir en grupos de 15 minutos (900 segs)

Toma_Poc_15m <- Toma %>% 
  mutate(Hora = (Hora %/% 900)*900) %>% 
  group_by(Hora) %>%
  summarise(Fecha_Hora = min(Hora),
            Nivel = mean(Nivel),
            Caudal = mean(Caudal)) %>%
  select(Fecha_Hora, 
         Nivel, 
         Caudal)


Toma_Poc_1d <- Toma %>% 
  mutate(Hora = (Hora %/% 86400)*86400) %>% 
  group_by(Hora) %>%
  summarise(Fecha_Hora = min(Hora),
            Nivel = mean(Nivel),
            Caudal = mean(Caudal)) %>%
  select(Fecha_Hora, 
         Nivel, 
         Caudal)


Toma_Poc_15m$Fecha_Hora <- as.POSIXct(Toma_Poc_15m$Fecha_Hora,
                                     origin = "1970-01-01",
                                     tz = "GMT")

Toma_Poc_1d$Fecha_Hora <- as.POSIXct(Toma_Poc_1d$Fecha_Hora,
                                    origin = "1970-01-01",
                                    tz = "GMT")

Toma_Poc_1m <- Toma_Poc_1d %>%
  group_by(anho = year(Fecha_Hora), 
           mes = month(Fecha_Hora)) %>%
  summarise(Fecha_Hora = min(Fecha_Hora),
            Nivel = mean(Nivel),
            Caudal = mean(Caudal)) %>%
  ungroup() %>%
  select(Fecha_Hora,
         Nivel,
         Caudal)


plot(Toma_Poc_1d$Fecha_Hora, Toma_Poc_1d$Nivel, type = "o",)

plot(Toma_Poc_1d$Fecha_Hora, Toma_Poc_1d$Caudal, type = "o",)



rm(Toma_TelemFailed, 
   TelemFailedRankingGroup, 
   Toma,
   i,
   n,
   NAs)
