library(tidyverse)
library(openxlsx)
library(lubridate)


SL_Toma <- read.csv("San Lorenzo Nivel Toma.txt", stringsAsFactors = F)
SL_Toma$TIME <- as.POSIXct(SL_Toma$TIME)

# los datos vienen de 3 diferentes columnas de la tabla en la Base de datos,
# debido al cambio de nombre en la variable de Survalent
SL_Toma01 <- SL_Toma %>% 
  filter(!is.na(Nivel01)) %>%
  mutate(Hora = TIME,
         Nivel = Nivel01,
         Flag = Flag01) %>%
  select(Hora, Nivel, Flag)

SL_Toma02 <- SL_Toma %>% 
  filter(!is.na(Nivel02)) %>%
  mutate(Hora = TIME,
         Nivel = Nivel02,
         Flag = Flag02) %>%
  select(Hora, Nivel, Flag)

SL_Toma03 <- SL_Toma %>% 
  filter(!is.na(Nivel03)) %>%
  mutate(Hora = TIME,
         Nivel = Nivel03,
         Flag = Flag03) %>%
  select(Hora, Nivel, Flag)

NivelToma <- rbind(SL_Toma01, SL_Toma02, SL_Toma03)
rm(SL_Toma, SL_Toma01, SL_Toma02, SL_Toma03)

# si se desea eliminar los telemetry failed del punto anterior
# 
# NivelTomaTelemFailed <- NivelToma %>% filter(Flag != 1)
# NivelTomaTelemFailed <- cbind(NivelTomaTelemFailed, rep(0, nrow(NivelTomaTelemFailed)))
# names(NivelTomaTelemFailed)[4] = "Rank"
# 
# 
# n = 1
# for (i in 1:(nrow(NivelTomaTelemFailed)- 1)) {
#   NivelTomaTelemFailed$Rank[i] = n
#   if ((NivelTomaTelemFailed$Hora[i] + 300) != NivelTomaTelemFailed$Hora[i+1]){
#     n = n+1
#   }
# }
# 
# TelemFailedRankingGroup <- NivelTomaTelemFailed %>% 
#   group_by(Rank) %>% 
#   summarise(Hora = min(Hora), minutos = 5*n()) %>% 
#   filter(minutos > 30)
# 
# ABorrar <- NivelTomaTelemFailed %>% filter(Rank %in% TelemFailedRankingGroup$Rank)%>% select(Hora)
# NivelToma <- NivelToma %>% filter(!Hora %in% (ABorrar$Hora))
# NivelToma %>% filter(Flag != 1)
# 

NivelToma$NivelSobreCresta <- ifelse(NivelToma$Nivel>0, NivelToma$Nivel/100, 0)
NivelToma$Caudal <- 94.047 * sqrt(NivelToma$NivelSobreCresta ^ 3)
NivelToma$VolumenVertido <- 300* NivelToma$Caudal


NivelToma <- NivelToma %>% arrange(Hora) %>% 
  mutate(anho = year(Hora), 
         mes = month(Hora),
         dia = day(Hora),
         hora = hour(Hora)) %>%
  group_by(anho, mes, dia, hora) %>%
  summarise(CaudalAVG = mean(Caudal),
            VolumenVertidoT = sum(VolumenVertido))




DS_list <- list("Nivel" = NivelToma)
write.xlsx(DS_list, file = "C:/Data Science/ArhivosGenerados/Nivel y Caudal San Lorenzo.xlsx")
