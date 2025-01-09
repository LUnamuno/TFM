library(haven)
library(tidygeocoder)
library(sp)
library(readxl)
library(dplyr)
library(lubridate)

dades_aula <- read_dta("cpnl_aules.dta")

addresses <- data.frame(address=paste0(dades_aula$carrer_num, ", ", dades_aula$població_aula, ", ", dades_aula$aulacp, ", Spain"))
geolocal <- addresses %>% geocode(address, method = 'osm', lat = latitude , long = longitude)
dades_aula$lat_aula <- geolocal$latitude
dades_aula$lon_aula <- geolocal$longitude

load("Cleaning_lat_lon_LEIRE/cpnl_v3.Rdata") #cpnl
cpnl$lat_aula <- as.numeric(cpnl$lat_aula)
cpnl$lon_aula <- as.numeric(cpnl$lon_aula)

### Adreça de id_local 1029 no està bé, canvi manual:
cpnl$carrer_num[!is.na(cpnl$id_local) & cpnl$id_local==1029] <- "C. Riera de la Salut,7"
addresses2 <- data.frame(address=paste0(cpnl$carrer_num[!is.na(cpnl$id_local) & cpnl$id_local==1029], ", ", 
                                        cpnl$població_aula.y[!is.na(cpnl$id_local) & cpnl$id_local==1029], ", ", cpnl$aulacp[!is.na(cpnl$id_local) & cpnl$id_local==1029], ", Spain"))
geolocal2 <- addresses2 %>% geocode(address, method = 'osm', lat = latitude , long = longitude)
cpnl$lat_aula[!is.na(cpnl$id_local) & cpnl$id_local==1029] <- geolocal2$latitude
cpnl$lon_aula[!is.na(cpnl$id_local) & cpnl$id_local==1029] <- geolocal2$longitude

### Merge localized aules
pr <- merge(dades_aula, cpnl, by="id_local", all.x=T)
for (i in 1:dim(pr)[1])
{
  if (is.na(pr$lat_aula.x[i]))
  {
    pr$lat_aula.x[i] <- pr$lat_aula.y[i]
    pr$lon_aula.x[i] <- pr$lon_aula.y[i]
  }
}
pr2 <- pr
#write.csv(pr[is.na(pr$lat_aula.x), ], "/home/dmorina/Baixades/cpnl_nogeo_20240130.csv", row.names = F)

load("Cleaning_lat_lon_LEIRE/Cleaning_lat_lon_v5.RData")

pr3 <- merge(pr2, dades, by="id_local", all.x=T)
for (i in 1:dim(pr3)[1])
{
  if (is.na(pr3$lat_aula.x[i]))
  {
    pr3$lat_aula.x[i] <- pr3$lat_aula[i]
    pr3$lon_aula.x[i] <- pr3$lon_aula[i]
  }
}

dades_aula <- pr3[, c(1, 8, 9)] 
colnames(dades_aula) <- c("id_local", "lat_aula", "lon_aula")

dades_indiv <- read_dta("cpnl_modif.dta")
dades_indiv <- dades_indiv[dades_indiv$agrup_modalitat=="1 Presencial", ]

### Afegeixo les coordenades a nivell individual
dades <- merge(dades_indiv, dades_aula, by="id_local") 

### Les coordenades del id_local 1252 i 1253 no estan bé, les poso manualment
dades$lat_aula[dades$id_local==1252] <- 41.93221
dades$lon_aula[dades$id_local==1252] <- 2.254539
dades$lat_aula[dades$id_local==1253] <- 41.93221
dades$lon_aula[dades$id_local==1253] <- 2.254539

### Les coordenades del id_local 1270 no estan bé, les poso manualment
dades$lat_aula[dades$id_local==1270] <- 41.33863
dades$lon_aula[dades$id_local==1270] <- 1.69925


### Afegir el nivell següent a cada fila
dades$seg_nivell1[dades$nivell == "Ep 1"]                               <- "Bàsic 1"
dades$seg_nivell1[dades$nivell == "Ep 2"]                               <- "Bàsic 1"
dades$seg_nivell1[dades$nivell == "Ep 3"]                              <- "Bàsic 1"
dades$seg_nivell1[dades$nivell == "Inicial"]                            <- "Bàsic 1"
dades$seg_nivell1[dades$nivell == "Inicial 2"]                          <- "Inicial 3"
dades$seg_nivell1[dades$nivell == "Inicial 3"]                          <- "Bàsic 1"
dades$seg_nivell1[dades$nivell == "Bàsic 1"]                           <- "Bàsic 2"
dades$seg_nivell1[dades$nivell == "Bàsic 1-2 (CB)"]                     <- "Bàsic 3"
dades$seg_nivell1[dades$nivell == "Bàsic 2"]                            <- "Bàsic 3"
dades$seg_nivell1[dades$nivell == "Bàsic 2-3"]                          <- "Elemental 1"
dades$seg_nivell1[dades$nivell == "Bàsic 1-2-3"]                        <- "Elemental 1"
dades$seg_nivell1[dades$nivell == "Bàsic 3"]                            <- "Elemental 1"
dades$seg_nivell1[dades$nivell == "Bàsic 3-Elemental 1 (BE)"]           <- "Elemental 2"
dades$seg_nivell1[dades$nivell == "Elemental 1"]                        <- "Elemental 2"
dades$seg_nivell1[dades$nivell == "Elemental 1-2"]                      <- "Elemental 3"
dades$seg_nivell1[dades$nivell == "Elemental 2"]                        <- "Elemental 3"
dades$seg_nivell1[dades$nivell == "Elemental 2-3 (CE)"]                 <- "Intermedi 1"
dades$seg_nivell1[dades$nivell == "Elemental 1-2-3"]                    <- "Intermedi 1"
dades$seg_nivell1[dades$nivell == "Elemental 3"]                        <- "Intermedi 1"
dades$seg_nivell1[dades$nivell == "Intermedi 1"]                        <- "Intermedi 2"
dades$seg_nivell1[dades$nivell == "Intermedi 1-2 (CI)"]                 <- "Intermedi 3"
dades$seg_nivell1[dades$nivell == "Intermedi 2"]                        <- "Intermedi 3"
dades$seg_nivell1[dades$nivell == "Intermedi 2-3"]                      <- "Suficiència 1"
dades$seg_nivell1[dades$nivell == "Intermedi 1-2-3"]                    <- "Suficiència 1"
dades$seg_nivell1[dades$nivell == "Intermedi 3"]                        <- "Suficiència 1"
dades$seg_nivell1[dades$nivell == "Suficiència 1"]                      <- "Suficiència 2"
dades$seg_nivell1[dades$nivell == "Suficiència 1-2"]                    <- "Suficiència 3"
dades$seg_nivell1[dades$nivell == "Suficiència 1-2-3"]                  <- "Superior"
dades$seg_nivell1[dades$nivell == "Suficiència 2"]                      <- "Suficiència 3"
dades$seg_nivell1[dades$nivell == "Suficiència 2-3 (CS)"]               <- "Superior"
dades$seg_nivell1[dades$nivell == "Suficiència 3"]                      <- "Superior"
dades$seg_nivell1[dades$nivell == "Superior"]                           <- NA

dades$seg_nivell2[dades$nivell == "Ep 1"]                               <- "Bàsic 1-2 (CB)"
dades$seg_nivell2[dades$nivell == "Ep 2"]                               <- "Bàsic 1-2 (CB)"
dades$seg_nivell2[dades$nivell == "Ep 3"]                              <- "Bàsic 1-2 (CB)"
dades$seg_nivell2[dades$nivell == "Inicial"]                            <- "Bàsic 1-2 (CB)"
dades$seg_nivell2[dades$nivell == "Inicial 2"]                          <- NA
dades$seg_nivell2[dades$nivell == "Inicial 3"]                          <- "Bàsic 1-2 (CB)"
dades$seg_nivell2[dades$nivell == "Bàsic 1"]                           <- "Bàsic 2-3"
dades$seg_nivell2[dades$nivell == "Bàsic 1-2 (CB)"]                     <- "Bàsic 3-Elemental 1 (BE)"
dades$seg_nivell2[dades$nivell == "Bàsic 2"]                            <- "Bàsic 3-Elemental 1 (BE)"
dades$seg_nivell2[dades$nivell == "Bàsic 2-3"]                          <- "Elemental 1-2"
dades$seg_nivell2[dades$nivell == "Bàsic 1-2-3"]                        <- "Elemental 1-2"
dades$seg_nivell2[dades$nivell == "Bàsic 3"]                            <- "Elemental 1-2"
dades$seg_nivell2[dades$nivell == "Bàsic 3-Elemental 1 (BE)"]           <- "Elemental 2-3 (CE)"
dades$seg_nivell2[dades$nivell == "Elemental 1"]                        <- "Elemental 2-3 (CE)"
dades$seg_nivell2[dades$nivell == "Elemental 1-2"]                      <- NA
dades$seg_nivell2[dades$nivell == "Elemental 2"]                        <- NA
dades$seg_nivell2[dades$nivell == "Elemental 2-3 (CE)"]                 <- "Intermedi 1-2 (CI)"
dades$seg_nivell2[dades$nivell == "Elemental 1-2-3"]                    <- "Intermedi 1-2 (CI)"
dades$seg_nivell2[dades$nivell == "Elemental 3"]                        <- "Intermedi 1-2 (CI)"
dades$seg_nivell2[dades$nivell == "Intermedi 1"]                        <- "Intermedi 2-3"
dades$seg_nivell2[dades$nivell == "Intermedi 1-2 (CI)"]                 <- NA
dades$seg_nivell2[dades$nivell == "Intermedi 2"]                        <- NA
dades$seg_nivell2[dades$nivell == "Intermedi 2-3"]                      <- "Suficiència 1-2"
dades$seg_nivell2[dades$nivell == "Intermedi 1-2-3"]                    <- "Suficiència 1-2"
dades$seg_nivell2[dades$nivell == "Intermedi 3"]                        <- "Suficiència 1-2"
dades$seg_nivell2[dades$nivell == "Suficiència 1"]                      <- "Suficiència 2-3 (CS)"
dades$seg_nivell2[dades$nivell == "Suficiència 1-2"]                    <- NA
dades$seg_nivell2[dades$nivell == "Suficiència 1-2-3"]                  <- NA
dades$seg_nivell2[dades$nivell == "Suficiència 2"]                      <- NA
dades$seg_nivell2[dades$nivell == "Suficiència 2-3 (CS)"]               <- NA
dades$seg_nivell2[dades$nivell == "Suficiència 3"]                      <- NA
dades$seg_nivell2[dades$nivell == "Superior"]                           <- NA

dades$seg_nivell3[dades$nivell == "Ep 1"]                               <- "Bàsic 1-2-3"
dades$seg_nivell3[dades$nivell == "Ep 2"]                               <- "Bàsic 1-2-3"
dades$seg_nivell3[dades$nivell == "Ep 3"]                              <-  "Bàsic 1-2-3"
dades$seg_nivell3[dades$nivell == "Inicial"]                            <- "Bàsic 1-2-3"
dades$seg_nivell3[dades$nivell == "Inicial 2"]                          <- NA
dades$seg_nivell3[dades$nivell == "Inicial 3"]                          <- "Bàsic 1-2-3"
dades$seg_nivell3[dades$nivell == "Bàsic 1"]                           <- NA
dades$seg_nivell3[dades$nivell == "Bàsic 1-2 (CB)"]                     <-NA
dades$seg_nivell3[dades$nivell == "Bàsic 2"]                            <-NA
dades$seg_nivell3[dades$nivell == "Bàsic 2-3"]                          <-"Elemental 1-2-3"
dades$seg_nivell3[dades$nivell == "Bàsic 1-2-3"]                        <-"Elemental 1-2-3"
dades$seg_nivell3[dades$nivell == "Bàsic 3"]                            <-"Elemental 1-2-3"
dades$seg_nivell3[dades$nivell == "Bàsic 3-Elemental 1 (BE)"]           <- NA
dades$seg_nivell3[dades$nivell == "Elemental 1"]                        <- NA
dades$seg_nivell3[dades$nivell == "Elemental 1-2"]                      <- NA
dades$seg_nivell3[dades$nivell == "Elemental 2"]                        <- NA
dades$seg_nivell3[dades$nivell == "Elemental 2-3 (CE)"]                 <-"Intermedi 1-2-3"
dades$seg_nivell3[dades$nivell == "Elemental 1-2-3"]                    <-"Intermedi 1-2-3"
dades$seg_nivell3[dades$nivell == "Elemental 3"]                        <-"Intermedi 1-2-3"
dades$seg_nivell3[dades$nivell == "Intermedi 1"]                        <- NA
dades$seg_nivell3[dades$nivell == "Intermedi 1-2 (CI)"]                 <- NA
dades$seg_nivell3[dades$nivell == "Intermedi 2"]                        <- NA
dades$seg_nivell3[dades$nivell == "Intermedi 2-3"]                      <-"Suficiència 1-2-3"
dades$seg_nivell3[dades$nivell == "Intermedi 1-2-3"]                    <-"Suficiència 1-2-3"
dades$seg_nivell3[dades$nivell == "Intermedi 3"]                        <-"Suficiència 1-2-3"
dades$seg_nivell3[dades$nivell == "Suficiència 1"]                      <- NA
dades$seg_nivell3[dades$nivell == "Suficiència 1-2"]                    <- NA
dades$seg_nivell3[dades$nivell == "Suficiència 1-2-3"]                  <- NA
dades$seg_nivell3[dades$nivell == "Suficiència 2"]                      <- NA
dades$seg_nivell3[dades$nivell == "Suficiència 2-3 (CS)"]               <- NA
dades$seg_nivell3[dades$nivell == "Suficiència 3"]                      <- NA
dades$seg_nivell3[dades$nivell == "Superior"]                           <- NA

### Afegir horari matí/tarda
dades$matitarda <- "Tarda"
dades$matitarda[dades$horainici == " 8:00"] <- "Matí"
dades$matitarda[dades$horainici == " 8:15"] <- "Matí"
dades$matitarda[dades$horainici == " 8:30"] <- "Matí"
dades$matitarda[dades$horainici == " 8:45"] <- "Matí"
dades$matitarda[dades$horainici == " 9:00"] <- "Matí"
dades$matitarda[dades$horainici == " 9:15"] <- "Matí"
dades$matitarda[dades$horainici == " 9:30"] <- "Matí"
dades$matitarda[dades$horainici == " 9:45"] <- "Matí"
dades$matitarda[dades$horainici == "10:00"] <- "Matí"
dades$matitarda[dades$horainici == "10:15"] <- "Matí"
dades$matitarda[dades$horainici == "10:30"] <- "Matí"
dades$matitarda[dades$horainici == "10:45"] <- "Matí"
dades$matitarda[dades$horainici == "11:00"] <- "Matí"
dades$matitarda[dades$horainici == "11:15"] <- "Matí"
dades$matitarda[dades$horainici == "11:30"] <- "Matí"
dades$matitarda[dades$horainici == "11:45"] <- "Matí"
dades$matitarda[dades$horainici == "12:00"] <- "Matí"
dades$matitarda[dades$horainici == "12:15"] <- "Matí"
dades$matitarda[dades$horainici == "12:30"] <- "Matí"
dades$matitarda[dades$horainici == "13:00"] <- "Matí"
dades$matitarda[dades$horainici == "13:30"] <- "Matí"
dades$matitarda[dades$horainici == "13:45"] <- "Matí"

### Remove useless data
rm(list=c("cpnl", "dades_aula", "dades_indiv", "i", "addresses", "addresses2", "geolocal", "geolocal2", "pr", "pr2", "pr3"))

### Calcular distància mínima al centre que fa el mateix nivell
load("dades_ind_geo_20240408.RData")


calc_min_distance <- function(i)
{
  if (is.na(dades$lon_aula[i])) return(NA)
  centre_coords <- c(dades$lon_aula[i], dades$lat_aula[i])
  datainici_dur <- ifelse(dades$intensivitat[i]==5, 6, ifelse(dades$intensivitat[i]==6, 12, dades$intensivitat[i]))
  #### SELECCIONAR SUBMOSTRA DE CENTRES AMB EL MATEIX NIVELL (NOMÉS CAL CALCULAR DISTÀNCIA ENTRE AQUESTS)
  submostra <- dades[(dades$nivell == dades$seg_nivell1[i] | dades$nivell == dades$seg_nivell2[i] | dades$nivell == dades$seg_nivell3[i]) & 
                       dades$matitarda == dades$matitarda[i] &
                       dades$datainicicurs <= dades$datainicicurs[i] %m+% years(1) %m+% months(datainici_dur) &
                       dades$datainicicurs > dades$datainicicurs[i] %m+% months(datainici_dur), ]
  submostra <- submostra[!duplicated(submostra$id_local), ]
  if (dim(submostra)[1] == 0 ) return(NA)
  #if (dim(submostra)[1] == 0 | is.na(submostra$id_local)) return(NA)
  dist_min <- 10000
  for (j in 1:dim(submostra)[1])
  {
    if (is.na(submostra$lon_aula[j])){
      dist_min <- 10000
    }else{
      dist <- spDists(matrix(c(centre_coords[1], submostra$lon_aula[j], centre_coords[2], submostra$lat_aula[j]), ncol=2), longlat = T)[2, 1]
      if (dist == 0) return(0)
      
      if (dist < dist_min) dist_min <- dist
    }
  }
  if (dist_min == 10000) dist_min <- NA
  return(dist_min)
}


system.time(dades$dist_min_aula <- vapply(1:nrow(dades), calc_min_distance, numeric(1)))

tail(dades %>% na.omit() %>% select(lon_aula, lat_aula, dist_min_aula))
save(dades, file= "cpnl_dist10042024.RData")
