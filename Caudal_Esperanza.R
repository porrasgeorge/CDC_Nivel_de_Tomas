
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
   NAs)
