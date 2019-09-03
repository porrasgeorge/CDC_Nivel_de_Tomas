
library(tidyverse)
library(openxlsx)
library(lubridate)

inicio <- as.numeric(as.POSIXct("2018-01-01", tz = "GMT"))
fin <- as.numeric(as.POSIXct("2019-08-26", tz = "GMT")) - 300

Toma <- read.csv("San Lorenzo Nivel Toma.txt", stringsAsFactors = F)
Toma$TIME <- as.numeric(as.POSIXct(Toma$TIME, tz = "GMT"))

Toma <- Toma %>% filter(TIME <= fin)
Toma <- Toma[-c(2, 3, 4, 5, 6),]

horas <- as.data.frame(seq(inicio, fin, 900))
names(horas)[1] = "TIME"

asdfger <- horas %>% left_join(Toma, by = "TIME")


asdfger$Hora <- as.POSIXct(asdfger$TIME,
                        origin = "1970-01-01",
                        tz = "GMT")

Toma %>% filter(is.na(Nivel))
Toma %>% filter(is.na(Pot))

max(Toma$Nivel, na.rm = T)



Pot %>% filter(is.na(Pot))

Toma$TIME <- as.POSIXct(Toma$Hora,
                        origin = "1970-01-01",
                        tz = "GMT")


plot(Toma$Hora, 
     Toma$Nivel, 
     pch = 19,
     cex = 0.5,
     col =  "#0073C2FF")
abline(a = 2.43, 
       b = 0, 
       col =  "yellow")

plot(Toma_HZ_1h$Fecha_Hora, 
     Toma_HZ_1h$Nivel, 
     pch = 19,
     cex = 0.5,
     col =  "#0073C2FF")
abline(a = 2.43, 
       b = 0, 
       col =  "yellow")

plot(Toma_Esp_1d$Fecha_Hora, 
     Toma_Esp_1d$Nivel, 
     pch = 19,
     cex = 0.5,
     col =  "#0073C2FF")
abline(a = 2.43, 
       b = 0, 
       col =  "yellow")


hist(Toma_SL_15m$Nivel)
boxplot(Toma_SL_15m$Nivel)

a <- Toma_Zap_15m %>% filter(Nivel > 0, Nivel < 0.2)
plot(a$Fecha_Hora, a$Nivel)

abline(v=c(15,20), col=c("blue", "red"), lty=c(1,2), lwd=c(1, 3))

