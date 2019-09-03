
Toma <- read.csv2("Cubujuqui Nivel Toma.txt", stringsAsFactors = F)
Toma$TIME <- as.numeric(as.POSIXct(Toma$TIME, tz = "GMT"))
names(Toma)[1] <- "Hora"
names(Toma)[2] <- "Nivel"
names(Toma)[3] <- "Flag"

# Rellenar espacios faltantes
Toma <- fechas_rango %>% left_join(Toma, by = "Hora")
# se cambia de msnm a m sobre cresta
Toma$Nivel <- ifelse(Toma$Nivel < 250, NA, Toma$Nivel)
Toma$NivelVertido <- ifelse(Toma$Nivel > 283.4,
                            Toma$Nivel - 283.4,
                            0)

Toma$Caudal <- 59.916 * sqrt(Toma$NivelVertido ^ 3)


# Los datos no son utilizables pues no marcan valores ligicos
# pues el nivel de vertido es 283.4
plot(Toma$Hora, Toma$Nivel)
boxplot(Toma$Nivel)
hist(Toma$Nivel)

rm(Toma)

