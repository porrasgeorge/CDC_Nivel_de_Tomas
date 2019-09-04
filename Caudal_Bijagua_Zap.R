
Toma <- read.csv2("Bijagua Nivel Tomas.txt", stringsAsFactors = F)
Toma$TIME <- as.numeric(as.POSIXct(Toma$TIME, tz = "GMT"))

Toma$BIJAGUA_SUB.NIVEL_DE_TOMA_BIJAGUA.VAL <- NULL
Toma$BIJAGUA_SUB.NIVEL_DE_TOMA_BIJAGUA.VAL.Q <- NULL
Toma$BIJAGUA_SUB.NIVEL_DE_TOMA_ZAPOTE.VAL <- NULL
Toma$BIJAGUA_SUB.NIVEL_DE_TOMA_ZAPOTE.VAL.Q <- NULL
Toma$BIJAGUA_SUB.NIVEL_RIO_BIJAGUA.VAL <- NULL
Toma$BIJAGUA_SUB.NIVEL_RIO_BIJAGUA.VAL.Q <- NULL

names(Toma)[1] <- "Hora"
names(Toma)[2] <- "Nivel"
names(Toma)[3] <- "Flag"

# Rellenar espacios faltantes
Toma <- fechas_rango %>% left_join(Toma, by = "Hora")
# Antes de esta fecha no funcionaba el sensor
Toma$Nivel <- ifelse(Toma$Hora <= 1533754500, NA, Toma$Nivel)
# se cambia de msnm a m sobre cresta
Toma$Nivel <- ifelse(Toma$Nivel > 100, Toma$Nivel - 419.5, Toma$Nivel)
# se eliminan los datos de maxima medicion, falla de sensor
Toma$Nivel <- ifelse(Toma$Nivel > 3.5, NA, Toma$Nivel)
# Calculo de Caudal
Toma$NivelSbreCresta <- ifelse(Toma$Nivel > 0, Toma$Nivel, 0)
# Calculo de Caudal

Toma$Caudal <- 59.916 * sqrt(Toma$NivelSbreCresta ^ 3)


# plot(Toma$Hora, Toma$Nivel)
# boxplot(Toma$Nivel)
# hist(Toma$Nivel)


#################################################
# NAs suplantacion
# 

# NAs <- which(is.na(Toma$Nivel))
# for (i in NAs) {
#   if (i == 1) {
#     Toma$Nivel[i] = 0
#     Toma$Flag[i] = 0
#   }
#   else{
#     Toma$Nivel[i] = Toma$Nivel[i - 1]
#     Toma$Flag[i] = Toma$Flag[i - 1]
#   }
# }

#################################################
# Unir en grupos de 15 minutos (900 segs)

Toma_Zap_15m <- Toma %>% 
  mutate(Hora = (Hora %/% 900)*900) %>% 
  group_by(Hora) %>%
  summarise(Fecha_Hora = min(Hora),
            Nivel = mean(Nivel),
            Caudal = mean(Caudal)) %>%
  select(Fecha_Hora, 
         Nivel, 
         Caudal)


Toma_Zap_1d <- Toma %>% 
  mutate(Hora = (Hora %/% 86400)*86400) %>% 
  group_by(Hora) %>%
  summarise(Fecha_Hora = min(Hora),
            Nivel = mean(Nivel),
            Caudal = mean(Caudal)) %>%
  select(Fecha_Hora, 
         Nivel, 
         Caudal)


Toma_Zap_15m$Fecha_Hora <- as.POSIXct(Toma_Zap_15m$Fecha_Hora,
                                      origin = "1970-01-01",
                                      tz = "GMT")

Toma_Zap_1d$Fecha_Hora <- as.POSIXct(Toma_Zap_1d$Fecha_Hora,
                                     origin = "1970-01-01",
                                     tz = "GMT")

Toma_Zap_1m <- Toma_Zap_1d %>%
  group_by(anho = year(Fecha_Hora), 
           mes = month(Fecha_Hora)) %>%
  summarise(Fecha_Hora = min(Fecha_Hora),
            Nivel = mean(Nivel),
            Caudal = mean(Caudal)) %>%
  ungroup() %>%
  select(Fecha_Hora,
         Nivel,
         Caudal)


rm(Toma)

