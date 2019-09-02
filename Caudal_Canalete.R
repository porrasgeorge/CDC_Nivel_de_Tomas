
Can_Toma <- read.csv2("Canalete Nivel Toma.txt", stringsAsFactors = F)
Can_Toma$TIME <- as.numeric(as.POSIXct(Can_Toma$TIME, tz = "GMT"))

names(Can_Toma)[1] <- "Hora"
names(Can_Toma)[2] <- "Nivel"
names(Can_Toma)[3] <- "Flag"

#################################################
# Solo los datos de noche de 20:00 hasta 5:55

Can_Toma <- Can_Toma %>% filter((Hora %% 86400) %in% c(seq(0,21300,300), seq(72000, 86100, 300)))
# Can_Toma$Hora <- as.POSIXct(Can_Toma$Hora,
#                                           origin = "1970-01-01",
#                                           tz = "GMT")

# 3 datos por cada quince minutos
initial_Rows = length(Can_Toma$Hora)/3

# hay muchos problemas con esta medida
# hay valores negativos sin sentido
Can_Toma <- Can_Toma %>% filter(Nivel > 255 & Nivel < 264)

# hay valores pegados en 257.75 sin sentido
Can_Toma <- Can_Toma %>% filter(round(Nivel, 2) != 257.75)

# hay valores pegados en 261.15 sin sentido
Can_Toma <- Can_Toma %>% filter(round(Nivel,2) != 261.15)

hist(Can_Toma$Nivel)

# Can_Toma$Hora <- as.POSIXct(Can_Toma$Hora,
#                                       origin = "1970-01-01",
#                                       tz = "GMT")
sum(Can_Toma$Nivel)


#################################################
# NAs suplantacion

Can_NAs <- which(is.na(Can_Toma$Nivel))
for (i in Can_NAs) {
  if (i == 1) {
    Can_Toma$Nivel[i] = 0
    Can_Toma$Flag[i] = 0
  }
  else{
    Can_Toma$Nivel[i] = Can_Toma$Nivel[i - 1]
    Can_Toma$Flag[i] = Can_Toma$Flag[i - 1]
  }
}

#################################################
# Fallas de telemetria y valores manuales
Can_Toma_TelemFailed <- Can_Toma %>% filter(Flag != 1)
Can_Toma_TelemFailed <- cbind(Can_Toma_TelemFailed, 
                              rep(0, nrow(Can_Toma_TelemFailed)))
names(Can_Toma_TelemFailed)[4] = "Rank"

n = 1
for (i in 1:(nrow(Can_Toma_TelemFailed) - 1)) {
  Can_Toma_TelemFailed$Rank[i] = n
  if ((Can_Toma_TelemFailed$Hora[i] + 300) != Can_Toma_TelemFailed$Hora[i + 1]) {
    n = n + 1
  }
}

#################################################
# si se desea eliminar los telemetry failed del punto anterior

Can_TelemFailedRankingGroup <- Can_Toma_TelemFailed %>%
  group_by(Rank) %>%
  summarise(Hora = min(Hora), 
            minutos = 5 * n()) %>%
  filter(minutos > 30)

Can_TelemFailedRankingGroup$Hora <- as.POSIXct(Can_TelemFailedRankingGroup$Hora, 
                                               origin = "1970-01-01",
                                               tz = "GMT")

Can_ABorrar <- Can_Toma_TelemFailed %>%
  filter(Rank %in% Can_TelemFailedRankingGroup$Rank) %>%
  select(Hora)

Can_Toma <- Can_Toma %>% 
  filter(!Hora %in% (Can_ABorrar$Hora))

#################################################
# Calculo del nivel sobre cresta y Caudal
# se tiene una modificacion en esta lecturas 
# paso de msnm a nivel sobre cresta

# nivel minimo aceptado
Can_Toma$NivelSobreCresta <- ifelse(Can_Toma$Nivel > 260, 
                                    Can_Toma$Nivel - 260, 
                                    0)

# Calculo de Caudal
Can_Toma$Caudal <- 59.916 * sqrt(Can_Toma$NivelSobreCresta ^ 3)

#################################################
# Unir en grupos de 15 minutos (900 segs)

Toma_Can_15m <- Can_Toma %>% 
  mutate(Hora = (Hora %/% 900)*900) %>% 
  group_by(Hora) %>%
  summarise(Fecha_Hora = min(Hora),
            Nivel = mean(Nivel),
            CaudalAVG = mean(Caudal)) %>%
  select(Fecha_Hora, 
         Nivel, 
         CaudalAVG)

Toma_Can_15m$Fecha_Hora <- as.POSIXct(Toma_Can_15m$Fecha_Hora,
                                      origin = "1970-01-01",
                                      tz = "GMT")
end_Rows = length(Toma_Can_15m$Fecha_Hora)
Can_porc_eliminados <- round(100 * (initial_Rows - end_Rows) / initial_Rows, 2)

rm(Can_Toma_TelemFailed, 
   Can_TelemFailedRankingGroup, 
   Can_ABorrar, 
   Can_Toma, 
   i,
   n,
   Can_NAs, 
   initial_Rows,
   end_Rows)
