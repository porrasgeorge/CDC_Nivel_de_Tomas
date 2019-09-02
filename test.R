
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
