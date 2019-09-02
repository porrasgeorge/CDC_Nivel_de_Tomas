#################################################
# Solo los datos de noche de 20:00 hasta 5:55

# SL_Toma <- SL_Toma %>% filter((TIME %% 86400) %in% c(seq(0,21300,300), seq(72000, 86100, 300)))
# SL_Toma$TIME <- as.POSIXct(SL_Toma$TIME,
#                                           origin = "1970-01-01",
#                                           tz = "GMT")




