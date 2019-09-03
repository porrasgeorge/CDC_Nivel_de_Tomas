
Toma <- read.csv2("HidroZarcas Nivel Tomas.txt", stringsAsFactors = F)
Toma$TIME <- as.numeric(as.POSIXct(Toma$TIME, tz = "GMT"))
Toma <- Toma %>% filter(TIME <= fecha_final & TIME >= fecha_inicio)

Toma$HIDROZARCAS_U1.NIVEL_TOMA.VAL <- NULL
Toma$HIDROZARCAS_U1.NIVEL_TOMA.VAL.Q <- NULL

names(Toma)[1] <- "Hora"
names(Toma)[2] <- "Nivel"
names(Toma)[3] <- "Flag"

# Rellenar espacios faltantes
Toma <- fechas_rango %>% left_join(Toma, by = "Hora")

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
# Calculo del nivel sobre cresta y Caudal
# se tiene una modificacion en esta lecturas 
# paso de msnm a nivel sobre cresta

Toma$NivelSobreCresta <- ifelse(Toma$Nivel > 0, 
                                Toma$Nivel, 
                                0)

# Calculo de Caudal
Toma$Caudal <- 36.729 * sqrt(Toma$NivelSobreCresta ^ 3)


#################################################
# Unir en grupos de 15 minutos (900 segs)

Toma_HZ_15m <- Toma %>% 
  mutate(Hora = (Hora %/% 900)*900) %>% 
  group_by(Hora) %>%
  summarise(Fecha_Hora = min(Hora),
            Nivel = mean(Nivel),
            Caudal = mean(Caudal)) %>%
  select(Fecha_Hora, 
         Nivel, 
         Caudal)



Toma_HZ_1h <- Toma %>% 
  mutate(Hora = (Hora %/% 3600)*3600) %>% 
  group_by(Hora) %>%
  summarise(Fecha_Hora = min(Hora),
            Nivel = mean(Nivel),
            Caudal = mean(Caudal)) %>%
  select(Fecha_Hora, 
         Nivel, 
         Caudal)

Toma_HZ_1d <- Toma %>% 
  mutate(Hora = (Hora %/% 86400)*86400) %>% 
  group_by(Hora) %>%
  summarise(Fecha_Hora = min(Hora),
            Nivel = mean(Nivel),
            Caudal = mean(Caudal)) %>%
  select(Fecha_Hora, 
         Nivel, 
         Caudal)


Toma_HZ_15m$Fecha_Hora <- as.POSIXct(Toma_HZ_15m$Fecha_Hora,
                                      origin = "1970-01-01",
                                      tz = "GMT")

Toma_HZ_1d$Fecha_Hora <- as.POSIXct(Toma_HZ_1d$Fecha_Hora,
                                     origin = "1970-01-01",
                                     tz = "GMT")

Toma_HZ_1m <- Toma_HZ_1d %>%
  group_by(anho = year(Fecha_Hora), 
           mes = month(Fecha_Hora)) %>%
  summarise(Fecha_Hora = min(Fecha_Hora),
            Nivel = mean(Nivel),
            Caudal = mean(Caudal)) %>%
  ungroup() %>%
  select(Fecha_Hora,
         Nivel,
         Caudal)


# plot(Toma_HZ_1d$Fecha_Hora, Toma_HZ_1d$Nivel, type = "o",)
# plot(Toma_HZ_1d$Fecha_Hora, Toma_HZ_1d$Caudal, type = "o",)

rm(Toma,
   i,
   NAs)
