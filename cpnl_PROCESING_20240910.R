
#  CPNL: PROCESAMIENTO FINAL 
  # FECHA INICIO 07/06/20204

library(haven)
library(dplyr)
library(ggplot2)
library(reshape2)
library(readxl)
library(tidyr)
library(Hmisc)

load("cpnl_dist_20240607.Rdades") #dim = 691578 x 83
names(dades) 

# REVISION INICIAL --------------------------------------------------------

# solo cursos entre 2010-2019 --->filtrado de any_curs>=2020
dades <- subset(dades, dades$any_curs<2020) #669114
#edad mayor que 16 años
dades <- subset(dades, ! dades$edat<16) # BIEN 

#Solo cursos presenciales
table(dades$agrup_modalitat)# BIEN

#eliminar matriculas repetidas de individuo:
table(duplicated(dades[,c("codicurs","codialumne")]))  #BIEN, NO HAY DUPLICADOS

# alumno_ant2010 ----------------------------------------------------------

alumnes_ant_2010 <- read_excel("AlumnesAmbinscripAnteriors_1_1_2010.xlsx")
dades$alumno_ant2010<- 0
dades[dades$codialumne%in%alumnes_ant_2010$codialumne,"alumno_ant2010"]<- 1 #LISTO
#colnames(dades)[colnames(dades) == 'oldName'] <- 'newName'

# DireccionesErroneas.txt-------------------------------------------------
#habia unos typos que encontramos a la hora de calcular la distancia y las corrijo usando el codicurs
for(row in which(dades$codicurs=="01009E1PRJ00138")){
  # "aulacarrer" y "aulanum" NO SE DEBERIAN DE CAMBIAR TAMBIEN?? si supongo 
  dades[row,"aulacarrer"] <-"C. de Pujadas Truch"
  dades[row,"aulanum"] <-"1, A"
  dades[row,"població_aula"] <-"El Masnou"
  dades[row,"aulacp"] <-08320
}
for(row in which(dades$codicurs=="01009I3PRJ00072")){
  dades[row,"aulacarrer"] <-"C. de Pujadas Truch, "
  dades[row,"aulanum"] <-"1, A"
  dades[row,"població_aula"] <-"El Masnou"
  dades[row,"aulacp"] <-08320
}
for(row in which(dades$codicurs=="01001C2PRA00727")){
  dades[row,"aulacarrer"] <-"Muralla dels Genovesos"
  dades[row,"aulanum"] <-"12"
  dades[row,"població_aula"] <-"Mataró"
  dades[row,"aulacp"] <-08301
}

for(row in which(dades$codicurs=="01504B2PRJ00350")){
  dades[row,"aulacarrer"] <-"Carrer de Can Flequer"
  dades[row,"aulanum"] <-"25"
  dades[row,"població_aula"] <-"Mollet del Vallès"
  dades[row,"aulacp"] <-08100
}
for(row in which(dades$codicurs=="11929B1PRJ00010")){
  dades[row,"aulacarrer"] <-"Plaça Canalejas"
  dades[row,"aulanum"] <-""
  dades[row,"població_aula"] <-"Torregrossa"
  dades[row,"aulacp"] <-25141
}

# TERR ----------------------------------------------------------------------
#CALCULAR AMBITOS TERRITORIALES
ambits <- read_dta("ambits_territorials.dta") 
indices <- which(dades$'població_aula' %in% ambits$'població_aula')
cat("CP no encontrados:",length(dades$'població_aula')-length(indices))
cat("% CP no encontrados:",(length(dades$'població_aula')-length(indices))/(length(dades$'població_aula'))*100)
dades$terr <- NA 
for (i in 1:length(dades$'població_aula')) {
  ind_ambits <- which(ambits$'població_aula'==dades$'població_aula'[i])
  dades$terr[i] <- ambits$ambit[ind_ambits]
}
table(dades$terr)


# dist_min_aula_cat -----------------------------------------------------------------------
quant_prop <- c(-10,0,1.5,5,10,20,Inf)
dades$dist_min_aula_cat <-  cut(dades$dist_min_aula,quant_prop,labels = NULL,include.lowest = FALSE,right = TRUE,dig.lab = 3,ordered_result = TRUE,) 
levels(dades$dist_min_aula_cat) <- c("0","(0,1.5]","(1.5,5]","(5,10]","(10,20]",">20")
table(dades$dist_min_aula_cat)

# edat_cat-------------------------------------------------------------------------
hist(dades$edat)
summary(dades$edat) # tiene sentido hacerlo con los quantiles porque se asemejan a los intervalos de edad que creariamos 
quant_prop <- c(16,28,35,45,109)
dades$edat_cat <-  cut(dades$edat,quant_prop,labels = NULL,include.lowest = FALSE,right = TRUE,dig.lab = 3,ordered_result = TRUE,) 
# Vector de entrada (numérico)
# Número o vector con los cortes
# Etiquetas para cada grupo
# Si se incluye el valor más pequeño o no en el primer intervalo
# Si el intervalo derecho está cerrado (y el izquierdo abierto) o viceversa
# Número de digitos de los grupos si labels = NULL
# Si se deberia ordenar el resultado (TRUE) del factor o no (FALSE)

round(table(dades$edat_cat)/length(dades$edat)*100,2)

# VAR INSCRIPCIONES, NIVELES CAT-----------------------------------------------------------------------
# nivel_partida,nivel_final, edat_media, edat_media_cat

# Nivel de partida, nivel final, edad media:
calcular_nivel_partida <- function(dades) {
  # Agrupamos los datos por el identificador del alumno (codialumne)
  dades <- dades %>% 
    group_by(codialumne) %>% 
    mutate(nivel_partida = descragrupniv[which.min(any_curs)], 
           nivel_final = descragrupniv[which.max(any_curs)],
           edat_media = mean(edat, na.rm = TRUE))  
  return(dades)
}
dades <- calcular_nivel_partida(dades)

quant_prop <- c(16,28,35,45,109)
dades$edat_media_cat <-  cut(dades$edat_media,quant_prop,labels = NULL,include.lowest = FALSE,right = TRUE,dig.lab = 3,ordered_result = TRUE,) 
round(table(dades$edat_media_cat)/length(dades$edat_media_cat)*100,2)
# nivel maximo (assolit):
dades <- dades %>% 
  # Agrupamos por el identificador del alumno (codialumne) y seleccionamos el nivel máximo dentro de los Apte-s
  group_by(codialumne) %>% 
  mutate(nivel_maximo = if_else(any(assoliment_grup == "Apte"), max(descragrupniv[assoliment_grup == "Apte"]), "Res assolit"))
#sale un warning pero lo calcula bien:

#View(dades[,c("codialumne","any_curs","descragrupniv","assoliment_grup","nivel_maximo")])#es para comprobar si se calcula bien 

# GENERE -----------------------------------------------------------------------
#revision y correccion de valores 
# Agrupar por 'codialumne' y contar los valores unicos'
conteo_genero <- dades %>%
  group_by(codialumne) %>%
  summarise(n_distinct_sexos = n_distinct(genere))
alumnos_con_genero_diferente <- conteo_genero %>%
  filter(n_distinct_sexos > 1)
# 811
alumnos_con_genero_diferente <- alumnos_con_genero_diferente$codialumne
# Iterar sobre los alumnos con genero no unificado
for (alumno in alumnos_con_genero_diferente) {
  generos_alumno <- dades$genere[dades$codialumne == alumno]
  valor_distinto_na <-  generos_alumno[generos_alumno != ""]
  dades$genere[dades$codialumne == alumno & dades$genere == ""] <- valor_distinto_na
}
conteo_genero <- dades %>%
  group_by(codialumne) %>%
  summarise(n_distinct_sexos = n_distinct(genere))
alumnos_con_genero_diferente <- conteo_genero %>%
  filter(n_distinct_sexos > 1)
# 68
#TABLA DE GENEROS CONFUSOS:
subtabla <- dades[dades$codialumne %in% alumnos_con_genero_diferente$codialumne,c("codialumne","genere","codicurs") ]
save(subtabla,file = "alumnos_genero_confuso.Rdades")
write.csv(subtabla, file="Alumnos_genero_confuso.csv")
#SOLUCION: escoger la categoria mayoritaria o la ULTIMA en cuanto al tiempo:
#View(dades[dades$codialumne %in%alumnos_con_genero_diferente$codialumne,c("codialumne","genere","codicurs","dadesinicicurs")])
# Identificar la categor?a de g?nero mayoritaria para cada alumno
# Calculo un indicador de empate para luego decidir entre el valor mayoritario o el mas reciente 
genero_mayoritario <- dades[dades$codialumne %in% alumnos_con_genero_diferente$codialumne, ] %>%
  group_by(codialumne, genere) %>%
  summarise(count = n()) %>%
  arrange(codialumne, desc(count)) %>%
  mutate(empate = ifelse(n_distinct(count) > 1,1,0), # Variable que indica si hay empate (0)
         row_number = row_number()) %>%
  filter(row_number == 1) %>%
  select(codialumne, genere, empate) 
# Identificar la categor?a de g?nero m?s reciente en caso de empate
genero_mas_reciente <- dades[dades$codialumne %in% alumnos_con_genero_diferente$codialumne, ] %>%
  group_by(codialumne) %>%
  arrange(codialumne, desc(dadesinicicurs)) %>%
  slice(1) %>%
  select(codialumne, genere)
# Fusionar la informaci?n de g?nero mayoritario con la m?s reciente
genero_final <- merge(genero_mayoritario, genero_mas_reciente, by = "codialumne", all = TRUE)
# Seleccionar la categor?a de g?nero final
genero_final$genere <- ifelse(genero_final$empate==0 , genero_final$genere.y, genero_final$genere.x)
genero_final <- genero_final[, -which(names(genero_final) %in% c("genere.x", "genere.y","empate"))]
# Fusionar la informacion de genero mayoritario con 'datos'
dades <- merge(dades, genero_final, by = "codialumne", all.x = TRUE)
# Reemplazar las categorias de genero por la mayoritaria
dades$genere <- ifelse(is.na(dades$genere.y), dades$genere.x, dades$genere.y)
dades <- dades[, -which(names(dades) == "genere.y")]
names(dades)
colnames(dades)[colnames(dades) == 'genere'] <- 'genere_corr' 
colnames(dades)[colnames(dades) == 'genere.x'] <- 'genere_orig' 
names(dades)

# PAIS DE ORIGEN -----------------------------------------------------------------------
#revision y correccion de valores 
conteo_agrpais <- dades %>%
  group_by(codialumne) %>%
  summarise(n_distinct_agrpais = n_distinct(agrpais))
alumnos_con_agrpais_diferente <- conteo_agrpais %>%
  filter(n_distinct_agrpais > 1)
# 576
#View(dades[dades$codialumne %in%alumnos_con_agrpais_diferente$codialumne,c("codialumne","agrpais","dadesinicicurs")])
dades$agrpais[dades$agrpais==""] <- NA
dades$agrpais[dades$agrpais==" "] <- NA
alumnos_con_agrpais_diferente <- alumnos_con_agrpais_diferente$codialumne
for (alumno in alumnos_con_agrpais_diferente) {
  agrpais_alumno <- dades$agrpais[dades$codialumne == alumno]
  valor_distinto_na <-  agrpais_alumno[!is.na(agrpais_alumno)]
  dades$agrpais[dades$codialumne == alumno & is.na(dades$agrpais)] <- valor_distinto_na
}
conteo_agrpais <- dades %>%
  group_by(codialumne) %>%
  summarise(n_distinct_agrpais = n_distinct(agrpais))
alumnos_con_agrpais_diferente <- conteo_agrpais %>%
  filter(n_distinct_agrpais > 1)
# 490
#View(dades[dades$codialumne %in%alumnos_con_agrpais_diferente$codialumne,c("codialumne","agrpais","dadesinicicurs")])
alumnos_con_agrpais_diferente <- alumnos_con_agrpais_diferente$codialumne
for (alumno in alumnos_con_agrpais_diferente) {
  agrpais_alumno <- dades$agrpais[dades$codialumne == alumno]
  valor_distinto_na <-  unique(agrpais_alumno[agrpais_alumno != "Sense dades"])
  dades$agrpais[dades$codialumne == alumno & dades$agrpais == "Sense dades"] <- valor_distinto_na
}
conteo_agrpais <- dades %>%
  group_by(codialumne) %>%
  summarise(n_distinct_agrpais = n_distinct(agrpais))
alumnos_con_agrpais_diferente <- conteo_agrpais %>%
  filter(n_distinct_agrpais > 1)
# 109
#View(dades[dades$codialumne %in%alumnos_con_agrpais_diferente$codialumne,c("codialumne","agrpais","dadesinicicurs")])
subtabla <- dades[dades$codialumne %in% alumnos_con_agrpais_diferente$codialumne,c("codialumne","agrpais","codicurs") ]
save(subtabla,file = "Agrpais_confuso.Rdades")
write.csv(subtabla, file="Agrpais_confuso.csv")
#NOTA: para aquellos alumnos con distintos valores de agrpais cogeremos la mas frecuente:
#SOLUCION: escoger la categoria mayoritaria o la ULTIMA  en cuanto al tiempo:
#View(dades[dades$codialumne %in%alumnos_con_agrpais_diferente$codialumne,c("codialumne","agrpais","dadesinicicurs")])
# Identificar la categor?a de AGRPAIS mayoritaria para cada alumno
# Calculo un indicador de empate para luego decidir entre el valor mayoritario o el mas reciente 
agrpais_mayoritario <- dades[dades$codialumne %in% alumnos_con_agrpais_diferente$codialumne, ] %>%
  group_by(codialumne, agrpais) %>%
  summarise(count = n()) %>%
  arrange(codialumne, desc(count)) %>%
  mutate(empate = ifelse(n_distinct(count) > 1,1,0), # Variable que indica si hay empate (0)
         row_number = row_number()) %>%
  filter(row_number == 1) %>%
  select(codialumne, agrpais, empate) 
agrpais_mas_reciente <- dades[dades$codialumne %in% alumnos_con_agrpais_diferente$codialumne, ] %>%
  group_by(codialumne) %>%
  arrange(codialumne, desc(dadesinicicurs)) %>%
  slice(1) %>%
  select(codialumne, agrpais)
agrpais_final <- merge(agrpais_mayoritario, agrpais_mas_reciente, by = "codialumne", all = TRUE)
agrpais_final$agrpais <- ifelse(agrpais_final$empate==0 ,agrpais_final$agrpais.y, agrpais_final$agrpais.x)
#View(agrpais_final)
agrpais_final <- agrpais_final[, -which(names(agrpais_final) %in% c("agrpais.x", "agrpais.y","empate"))]
dades <- merge(dades, agrpais_final, by = "codialumne", all.x = TRUE)
dades$agrpais <- ifelse(is.na(dades$agrpais.y), dades$agrpais.x, dades$agrpais.y)
names(dades)
dades$agrpais.y<- NULL
colnames(dades)[colnames(dades) == 'agrpais.x'] <- 'agrpais_orig'
colnames(dades)[colnames(dades) == 'agrpais'] <- 'agrpais_corr'
names(dades)
# agrpais_corr_mod -----------------------------------------------------------------------
#load("Agrpais_confuso.Rdades") #109 casos a arreglar o modificar de la variable ya corregida
dades$agrpais <- dades$agrpais_orig
dades$agrpais_corr_mod <- dades$agrpais
conteo_agrpais <- dades %>%
  group_by(codialumne) %>%
  summarise(n_distinct_agrpais = n_distinct(agrpais_corr_mod))
alumnos_con_agrpais_diferente <- conteo_agrpais %>%
  filter(n_distinct_agrpais > 1)
#109 

#SOLUCION: corregimos con el valor modal y, en caso de empate, la mas reciente 

# Paso 1: Calcular la moda de estudis
agrpais_mayoritaria <- dades %>%
  filter(codialumne %in% alumnos_con_agrpais_diferente$codialumne) %>%
  group_by(codialumne, agrpais) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(codialumne, desc(count)) %>%
  group_by(codialumne) %>%
  mutate(rank = row_number(), 
         max_count = max(count)) %>%
  filter(count == max_count) %>%
  mutate(empate = ifelse(n() > 1, 1, 0)) %>%
  filter(rank == 1) %>%
  select(codialumne, agrpais, empate)

# Paso 2: Obtener los estudis más recientes por `codialumne`
agrpais_mas_reciente <- dades %>%
  filter(codialumne %in% alumnos_con_agrpais_diferente$codialumne) %>%
  group_by(codialumne) %>%
  arrange(desc(cursacademic)) %>%
  slice(1) %>%
  select(codialumne, agrpais) %>%
  rename(agrpais_reciente = agrpais)

# Paso 3: Combinar los resultados
agrpais_final <- merge(agrpais_mayoritaria, agrpais_mas_reciente, by = "codialumne", all.x = TRUE)

# Seleccionar la moda si no hay empate, si hay empate seleccionar los estudis más recientes
agrpais_final <- agrpais_final %>%
  mutate(agrpais_final = ifelse(empate == 1, agrpais_reciente, agrpais)) %>%
  select(codialumne, agrpais_final)

# Verificar los resultados
#View(estudis_final)
dades <- merge(dades, agrpais_final, by = "codialumne", all.x = TRUE)
dades$agrpais_corr_mod <- ifelse(is.na(dades$agrpais_final), dades$agrpais_corr_mod, dades$agrpais_final)
View(dades[,c("codialumne","cursacademic","agrpais_orig","agrpais_corr_mod","agrpais_final")])
dades$agrpais_final<- NULL
dades$agrpais <- NULL
names(dades)
 
# ORDEN AGRPAIS -----------------------------------------------------------------------

#DECIDIDO POR ANTONIO
dades$agrpais_corr <- as.factor(dades$agrpais_corr)
ordenado <- c("Catalunya","Resta de l'Estat Espanyol","Unió Europea","Europa Extracomunitària","Amèrica Llatina",
              "EUA i Canadà","Nord d'Àfrica","Resta d'Àfrica","Àsia","Oceania","Sense dades")
dades$agrpais_corr <-factor(dades$agrpais_corr, levels = ordenado)

# ESTUDIS CORREGIDO: -----------------------------------------------------------------------
dades$estudis_orig <- dades$estudis
#  estudis_corr y estudis_orig
dades$estudis_corr <- dades$estudis
conteo_estudis <- dades %>%
  group_by(codialumne) %>%
  summarise(n_distinct_estudis = n_distinct(estudis_corr))
alumnos_con_estudis_diferente <- conteo_estudis %>%
  filter(n_distinct_estudis > 1)
#3259
#View(dades[dades$codialumne %in%alumnos_con_estudis_diferente$codialumne,c("codialumne","estudis","dadesinicicurs")])
#primero quitamos la categoria sense dades si existe alguna otra informacion del alumno
alumnos_con_estudis_diferente <- alumnos_con_estudis_diferente$codialumne
for (alumno in alumnos_con_estudis_diferente) {
  estudis_alumno <- dades$estudis_corr[dades$codialumne == alumno]
  valor_distinto_na <-  unique(estudis_alumno[estudis_alumno != "Sense dades"])
  dades$estudis_corr[dades$codialumne == alumno & dades$estudis_corr == "Sense dades"] <- valor_distinto_na
}
conteo_estudis <- dades %>%
  group_by(codialumne) %>%
  summarise(n_distinct_estudis = n_distinct(estudis_corr))
alumnos_con_estudis_diferente <- conteo_estudis %>%
  filter(n_distinct_estudis > 1)

#1861
subtabla <- dades[dades$codialumne %in% alumnos_con_estudis_diferente$codialumne,c("codialumne","estudis","codicurs") ]
save(subtabla,file = "Estudis_confuso.Rdades")
write.csv(subtabla, file="Estudis_confuso.csv")

#SOLUCION: ahora nos quedamos con la info mas reciente de los estudios
estudis_mas_reciente <- dades[dades$codialumne %in% alumnos_con_estudis_diferente$codialumne, ] %>%
  group_by(codialumne) %>%
  arrange(codialumne, desc(dadesinicicurs)) %>%
  slice(1) %>%
  select(codialumne, estudis_corr)
alumnos_con_estudis_diferente <- alumnos_con_estudis_diferente$codialumne

for (alumno in alumnos_con_estudis_diferente) {
  dades$estudis_corr[dades$codialumne == alumno ] <- estudis_mas_reciente$estudis_corr[estudis_mas_reciente$codialumne == alumno]
}
conteo_estudis <- dades %>%
  group_by(codialumne) %>%
  summarise(n_distinct_estudis = n_distinct(estudis_corr))

alumnos_con_estudis_diferente <- conteo_estudis %>%
  filter(n_distinct_estudis > 1)
#0 :)))
names(dades)
colnames(dades)[colnames(dades) == 'estudis'] <- 'estudis_orig'

# estudis_corr_mod -----------------------------------------------------------------------
dades$estudis <- dades$estudis_orig
dades$estudis_corr_mod <- dades$estudis
conteo_estudis <- dades %>%
  group_by(codialumne) %>%
  summarise(n_distinct_estudis = n_distinct(estudis_corr_mod))
alumnos_con_estudis_diferente <- conteo_estudis %>%
  filter(n_distinct_estudis > 1)
#3259
#View(dades[dades$codialumne %in%alumnos_con_estudis_diferente$codialumne,c("codialumne","estudis","dadesinicicurs")])
#primero quitamos la categoria sense dades si existe alguna otra informacion del alumno
alumnos_con_estudis_diferente <- alumnos_con_estudis_diferente$codialumne
for (alumno in alumnos_con_estudis_diferente) {
  estudis_alumno <- dades$estudis_corr_mod[dades$codialumne == alumno]
  valor_distinto_na <-  unique(estudis_alumno[estudis_alumno != "Sense dades"])
  dades$estudis_corr_mod[dades$codialumne == alumno & dades$estudis_corr_mod == "Sense dades"] <- valor_distinto_na
}
conteo_estudis <- dades %>%
  group_by(codialumne) %>%
  summarise(n_distinct_estudis = n_distinct(estudis_corr_mod))
alumnos_con_estudis_diferente <- conteo_estudis %>%
  filter(n_distinct_estudis > 1)

#1861
#SOLUCION: corregimos con el valor modal y, en caso de empate, la mas reciente 


# Paso 1: Calcular la moda de estudis
estudis_mayoritaria <- dades %>%
  filter(codialumne %in% alumnos_con_estudis_diferente$codialumne) %>%
  group_by(codialumne, estudis) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(codialumne, desc(count)) %>%
  group_by(codialumne) %>%
  mutate(rank = row_number(), 
         max_count = max(count)) %>%
  filter(count == max_count) %>%
  mutate(empate = ifelse(n() > 1, 1, 0)) %>%
  filter(rank == 1) %>%
  select(codialumne, estudis, empate)

# Paso 2: Obtener los estudis más recientes por `codialumne`
estudis_mas_reciente <- dades %>%
  filter(codialumne %in% alumnos_con_estudis_diferente$codialumne) %>%
  group_by(codialumne) %>%
  arrange(desc(cursacademic)) %>%
  slice(1) %>%
  select(codialumne, estudis) %>%
  rename(estudis_reciente = estudis)

# Paso 3: Combinar los resultados
estudis_final <- merge(estudis_mayoritaria, estudis_mas_reciente, by = "codialumne", all.x = TRUE)

# Seleccionar la moda si no hay empate, si hay empate seleccionar los estudis más recientes
estudis_final <- estudis_final %>%
  mutate(estudis_final = ifelse(empate == 1, estudis_reciente, estudis)) %>%
  select(codialumne, estudis_final)

# Verificar los resultados
#View(estudis_final)
dades <- merge(dades, estudis_final, by = "codialumne", all.x = TRUE)
#View(dades[,c("codialumne","cursacademic","estudis","estudis_corr_mod","estudis_final")])
dades$estudis_corr_mod <- ifelse(is.na(dades$estudis_final), dades$estudis_corr_mod, dades$estudis_final)
#View(dades[,c("codialumne","cursacademic","estudis","estudis_corr_mod","estudis_final")])
dades$estudis_final<- NULL
dades$estudis <- NULL
names(dades)
# ESTUDIS AGRUPADO -----------------------------------------------------------------------

# sense estudis	/educacio obligatoria	/secundària post-obligatòria/	estudis universitàris	/sense dades
dades <- dades %>%
  mutate(agrestudis = recode(estudis_corr,
                             "No sap llegir o escriure" = "Sense estudis",
                             "Sense estudis o primària incompleta"  = "Sense estudis",
                             "Ed. primària (EGB completa, batxillerat elem.)" = "Educació obligatoria",
                             "ESO (ESO, F.prof. grau mitjà, FP 1r grau)" = "Educació obligatoria",
                             "Secundària (batx., f. prof.grau sup., FP 2n grau)" = "Secundària (batx., f. prof.grau sup., FP 2n grau)",
                             "Universitaris (grau, màster, dip., llic., doct.)" = "Universitaris (grau, màster, dip., llic., doct.)",
                             "Sense dades" = "Sense dades"
  ))
dades <- dades %>%
  mutate(agrestudis_mod = recode(estudis_corr_mod,
                             "No sap llegir o escriure" = "Sense estudis",
                             "Sense estudis o primària incompleta"  = "Sense estudis",
                             "Ed. primària (EGB completa, batxillerat elem.)" = "Educació obligatoria",
                             "ESO (ESO, F.prof. grau mitjà, FP 1r grau)" = "Educació obligatoria",
                             "Secundària (batx., f. prof.grau sup., FP 2n grau)" = "Secundària (batx., f. prof.grau sup., FP 2n grau)",
                             "Universitaris (grau, màster, dip., llic., doct.)" = "Universitaris (grau, màster, dip., llic., doct.)",
                             "Sense dades" = "Sense dades"
  ))

# PAIS DE ORIGEN : revision y correccion de valores   -----------------------------------------------------------------------
conteo_pais <- dades %>%
  group_by(codialumne) %>%
  summarise(n_distinct_pais = n_distinct(pais))
alumnos_con_pais_diferente <- conteo_pais %>%
  filter(n_distinct_pais > 1)
# 653
dades$pais[dades$pais==""] <- NA
dades$pais[dades$pais==" "] <- NA
alumnos_con_pais_diferente <- alumnos_con_pais_diferente$codialumne
for (alumno in alumnos_con_pais_diferente) {
  pais_alumno <- dades$pais[dades$codialumne == alumno]
  valor_distinto_na <-  pais_alumno[!is.na(pais_alumno)]
  dades$pais[dades$codialumne == alumno & is.na(dades$pais)] <- valor_distinto_na
}
conteo_pais <- dades %>%
  group_by(codialumne) %>%
  summarise(n_distinct_pais = n_distinct(pais))
alumnos_con_pais_diferente <- conteo_pais %>%
  filter(n_distinct_pais > 1)
# 567
alumnos_con_pais_diferente <- alumnos_con_pais_diferente$codialumne
for (alumno in alumnos_con_pais_diferente) {
  pais_alumno <- dades$pais[dades$codialumne == alumno]
  valor_distinto_na <-  unique(pais_alumno[pais_alumno != "Sense dades"])
  dades$pais[dades$codialumne == alumno & dades$pais == "Sense dades"] <- valor_distinto_na
}
conteo_pais <- dades %>%
  group_by(codialumne) %>%
  summarise(n_distinct_pais = n_distinct(pais))
alumnos_con_pais_diferente <- conteo_pais %>%
  filter(n_distinct_pais > 1)
#186
subtabla <- dades[dades$codialumne %in% alumnos_con_pais_diferente$codialumne,c("codialumne","pais","codicurs") ]
save(subtabla,file = "Pais_confuso.Rdades")
write.csv(subtabla, file="Pais_confuso.csv")
#NOTA: para aquellos alumnos con distintos valores de agrpais cogeremos la mas frecuente:

#SOLUCION: escoger la categoria mayoritaria o la ULTIMA  en cuanto al tiempo:
# Identificar la categoria de AGRPAIS mayoritaria para cada alumno
# Calculo un indicador de empate para luego decidir entre el valor mayoritario o el mas reciente 
pais_mayoritario <- dades[dades$codialumne %in% alumnos_con_pais_diferente$codialumne, ] %>%
  group_by(codialumne, pais) %>%
  summarise(count = n()) %>%
  arrange(codialumne, desc(count)) %>%
  mutate(empate = ifelse(n_distinct(count) > 1,1,0), # Variable que indica si hay empate (0)
         row_number = row_number()) %>%
  filter(row_number == 1) %>%
  select(codialumne, pais, empate) 
pais_mas_reciente <- dades[dades$codialumne %in% alumnos_con_pais_diferente$codialumne, ] %>%
  group_by(codialumne) %>%
  arrange(codialumne, desc(dadesinicicurs)) %>%
  slice(1) %>%
  select(codialumne, pais)
pais_final <- merge(pais_mayoritario, pais_mas_reciente, by = "codialumne", all = TRUE)
pais_final$pais <- ifelse(pais_final$empate==0 ,pais_final$pais.y, pais_final$pais.x)

pais_final <- pais_final[, -which(names(pais_final) %in% c("pais.x", "pais.y","empate"))]
dades <- merge(dades, pais_final, by = "codialumne", all.x = TRUE)
dades$pais <- ifelse(is.na(dades$pais.y), dades$pais.x, dades$pais.y)
names(dades)
dades$pais.y<- NULL
colnames(dades)[colnames(dades) == 'pais.x'] <- 'pais_orig'
colnames(dades)[colnames(dades) == 'pais'] <- 'pais_corr'
names(dades)
# pais_corr_mod -----------------------------------------------------------------------
#load("Pais_confuso.Rdades") #186 casos a arreglar o modificar de la variable ya corregida
dades$pais <- dades$pais_orig
dades$pais_corr_mod <- dades$pais
conteo_pais <- dades %>%
  group_by(codialumne) %>%
  summarise(n_distinct_pais = n_distinct(pais_corr_mod))
alumnos_con_pais_diferente <- conteo_pais %>%
  filter(n_distinct_pais > 1)
#186 

#SOLUCION: corregimos con el valor modal y, en caso de empate, la mas reciente 

# Paso 1: Calcular la moda de estudis
pais_mayoritaria <- dades %>%
  filter(codialumne %in% alumnos_con_pais_diferente$codialumne) %>%
  group_by(codialumne, pais) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(codialumne, desc(count)) %>%
  group_by(codialumne) %>%
  mutate(rank = row_number(), 
         max_count = max(count)) %>%
  filter(count == max_count) %>%
  mutate(empate = ifelse(n() > 1, 1, 0)) %>%
  filter(rank == 1) %>%
  select(codialumne, pais, empate)

# Paso 2: Obtener los estudis más recientes por `codialumne`
pais_mas_reciente <- dades %>%
  filter(codialumne %in% alumnos_con_pais_diferente$codialumne) %>%
  group_by(codialumne) %>%
  arrange(desc(cursacademic)) %>%
  slice(1) %>%
  select(codialumne, pais) %>%
  rename(pais_reciente = pais)

# Paso 3: Combinar los resultados
pais_final <- merge(pais_mayoritaria, pais_mas_reciente, by = "codialumne", all.x = TRUE)

# Seleccionar la moda si no hay empate, si hay empate seleccionar los estudis más recientes
pais_final <- pais_final %>%
  mutate(pais_final = ifelse(empate == 1, pais_reciente, pais)) %>%
  select(codialumne, pais_final)

dades <- merge(dades, pais_final, by = "codialumne", all.x = TRUE)
dades$pais_corr_mod <- ifelse(is.na(dades$pais_final), dades$pais_corr_mod, dades$pais_final)
View(dades[,c("codialumne","cursacademic","pais_orig","pais_corr_mod","pais_final")])
dades$pais_final<- NULL
dades$pais <- NULL
names(dades)

# CORRECCION DE VARIABLES STATA: -----------------------------------------------------------------------
#  pc_univ, pc_esp,esp,pc_esp_cat, esp_cat

# % personas con estudios universitarios
dades$univ <- ifelse(dades$educacio == 4, 1, 0)
dades$univ[is.na(dades$educacio)] <- NA
dades$pc_univ <- ave(dades$univ, dades$codicurs, FUN = mean, na.rm = TRUE)

# % personas procedentes del resto de España o de Cataluña
dades$esp_cat <- ifelse(dades$agrpais_corr == "Catalunya" | dades$agrpais_corr == "Resta de l'Estat Espanyol", 1, 0)
dades$esp_cat[dades$agrpais_corr == "Sense dades"] <- NA
dades$pc_esp_cat <- ave(dades$esp_cat, dades$codicurs, FUN = mean, na.rm = TRUE)

# % personas procedentes del resto de España
dades$esp <- ifelse(dades$agrpais_corr == "Resta de l'Estat Espanyol", 1, 0)
dades$esp[dades$agrpais_corr == "Sense dades"] <- NA
dades$pc_esp <- ave(dades$esp, dades$codicurs, FUN = mean, na.rm = TRUE)

# % personas hispanohablantes (incluye personas nacidas en Cataluña)
dades$castparl <- dades$esp_cat
dades$castparl[dades$agrpais_corr == "Amèrica Llatina" & dades$lleng_latam == ""] <- 1

# INDICEs DE FRACCIONALIZACION  -----------------------------------------------------------------------
#CON pais_corr
# Step 1: Create a count variable for each individual
dades$individual_count <- 1

# Step 2: Collapse the dades to get the count of each ethnicity in each district
collapsed_dades <- dades %>% 
  group_by(codicurs, pais_corr) %>%
  summarise(individual_count = sum(individual_count)) %>%
  ungroup()

# Step 3: Calculate the total population in each district
collapsed_dades <- collapsed_dades %>%
  group_by(codicurs) %>%
  mutate(total_population = sum(individual_count)) %>%
  ungroup()

# Step 4: Calculate the proportion of each ethnic group in each district
collapsed_dades <- collapsed_dades %>%
  mutate(prop_paiscorr = individual_count / total_population)

# Step 5: Calculate the Fractionalization Index at the district level
fractionalization_pais <- collapsed_dades %>%
  group_by(codicurs) %>%
  summarise(sum_prop2 = sum(prop_paiscorr^2)) %>%
  mutate(fractionalization_pais = 1 - sum_prop2) %>%
  select(codicurs, fractionalization_pais)
dades <- merge(dades, fractionalization_pais, by = "codicurs", all.x = TRUE)

dades$individual_count <- NULL
#CON PAIS_CORR_MOD

# Step 1: Create a count variable for each individual
dades$individual_count <- 1

# Step 2: Collapse the dades to get the count of each ethnicity in each district
collapsed_dades <- dades %>% 
  group_by(codicurs, pais_corr_mod) %>%
  summarise(individual_count = sum(individual_count)) %>%
  ungroup()

# Step 3: Calculate the total population in each district
collapsed_dades <- collapsed_dades %>%
  group_by(codicurs) %>%
  mutate(total_population = sum(individual_count)) %>%
  ungroup()

# Step 4: Calculate the proportion of each ethnic group in each district
collapsed_dades <- collapsed_dades %>%
  mutate(prop_paiscorr = individual_count / total_population)

# Step 5: Calculate the Fractionalization Index at the district level
fractionalization_pais_mod <- collapsed_dades %>%
  group_by(codicurs) %>%
  summarise(sum_prop2 = sum(prop_paiscorr^2)) %>%
  mutate(fractionalization_pais_mod = 1 - sum_prop2) %>%
  select(codicurs, fractionalization_pais_mod)
dades <- merge(dades, fractionalization_pais_mod, by = "codicurs", all.x = TRUE)
dades$individual_count <- NULL

# dadesNaixement  ----------------------------------------------------------

conteo_dades<- dades %>%
  group_by(codialumne) %>%
  summarise(n_distinct_dats = n_distinct(dadesNaixement))
alumnos_con_dades_diferente <- conteo_dades %>%
  filter(n_distinct_dats > 1)
# 734
#TABLA DE GENEROS CONFUSOS:
subtabla <- dades[dades$codialumne %in% alumnos_con_dades_diferente$codialumne,c("codialumne","dadesNaixement","codicurs") ]
save(subtabla,file = "alumnos_dadesNaixement_confuso.Rdades")
write.csv(subtabla, file="Alumnos_dadesNaixement_confuso.csv")

dades_mas_reciente <- dades[dades$codialumne %in% alumnos_con_dades_diferente$codialumne, ] %>%
  group_by(codialumne) %>%
  arrange(codialumne, desc(dadesinicicurs)) %>%
  slice(1) %>%
  select(codialumne, dadesNaixement)
dades_mas_antigua <- dades[dades$codialumne %in% alumnos_con_dades_diferente$codialumne, ] %>%
  group_by(codialumne) %>%
  arrange(codialumne, dadesinicicurs) %>%
  slice(1) %>%
  select(codialumne, dadesNaixement)


dades <- merge(dades, dades_mas_reciente, by = "codialumne", all.x = TRUE) #dadesNaixement_orig.y
dades <- merge(dades, dades_mas_antigua, by = "codialumne", all.x = TRUE) #dadesNaixement_orig
names(dades)
colnames(dades)[colnames(dades) == 'dadesNaixement'] <- 'dadesNaixement_corr_primer' 
colnames(dades)[colnames(dades) == 'dadesNaixement.x'] <- 'dadesNaixement_orig' 
colnames(dades)[colnames(dades) == 'dadesNaixement.y'] <- 'dadesNaixement_corr_ultim' 
names(dades) 

dades$dadesNaixement_corr_primer <- ifelse(is.na(dades$dadesNaixement_corr_primer), dades$dadesNaixement_orig, dades$dadesNaixement_corr_primer)
dades$dadesNaixement_corr_ultim <- ifelse(is.na(dades$dadesNaixement_corr_ultim), dades$dadesNaixement_orig, dades$dadesNaixement_corr_ultim)
dades$dadesNaixement_corr_primer <- as.Date(dades$dadesNaixement_corr_primer)
dades$dadesNaixement_corr_ultim <- as.Date(dades$dadesNaixement_corr_ultim)
View(dades[,c("codialumne","cursacademic","dadesNaixement_orig","dadesNaixement_corr_ultim","dadesNaixement_corr_primer")])
#verificado que ya hay un valor unico por cada alumno endadesNaixement_corr 

# n_matr_corr -----------------------------------------------------------------------
dades <- dades %>% # una vez hemos hecho el subset de any_curs <2020
  group_by(codialumne)%>% 
  mutate(
    n_matr_corr = n_distinct(codicurs) # contamos cuantas matriculas tiene cada alumno EN ESE PERIODO y no n_matr (que es el total de la bbdd)
  )
dades$inscr_cat <- NA
hist(dades$n_matr_corr) # 1, 2-3,  3+ 
dades$inscr_cat <- cut( dades$n_matr_corr, breaks= c(-Inf,1,3, Inf), labels= c("1","2-3","3+"))
round(prop.table(table(dades$inscr_cat))*100,2)  

# caso_ab -----------------------------------------------------------------------

dades <- dades %>%
  group_by(codialumne) %>%
  mutate(
    caso_ab = case_when(
      n_matr_corr == 1 & assoliment_grup != "Apte" ~ "1) Un curso, no aprobado",
      n_matr_corr == 1 & assoliment_grup == "Apte" ~ "2) Un curso, aprobado",
      n_matr_corr > 1 & all(assoliment_grup == "Apte") ~ "3) Varios cursos, todos aprobados",
      n_matr_corr > 1 & any(assoliment_grup != "Apte") ~ "4) Varios cursos, al menos uno  no aprobado",
      n_matr_corr > 1 & all(assoliment_grup != "Apte") ~ "5) Varios cursos, ninguno aprobado"
    )
  ) %>%
  ungroup()
#View(dades[which(dades$codialumne==100001),c("codialumne","codicurs","n_matr","assoliment_grup","caso_ab")])
# VAR CONTINUIDADES -----------------------------------------------------------------------

# dias_entre_cursos, contiuidad, discontinuidad_cat, tiempo_reenganche, assoliment_grup_cat
dades$dadesinicicurs <- as.Date(dades$dadesinicicurs)
dades <- dades %>% arrange(codialumne, dadesinicicurs)

dades <- dades %>%
  group_by(codialumne) %>%
  arrange(codialumne, dadesinicicurs)  %>% 
  mutate(
    dias_entre_cursos = lead(dadesinicicurs) - dadesinicicurs,
    continuidad_12m = ifelse(is.na(dias_entre_cursos) | dias_entre_cursos > 365, "Discontinuidad", "Continuidad"),
    continuidad_6m = ifelse(is.na(dias_entre_cursos) | dias_entre_cursos > 180, "Discontinuidad", "Continuidad"),
    continuidad_1m = ifelse(is.na(dias_entre_cursos) | dias_entre_cursos > 30, "Discontinuidad", "Continuidad")
  )

dades <- dades %>%
  group_by(codialumne) %>%
  mutate(
    discontinuidades_12m = sum(continuidad_12m == "Discontinuidad"),
    discontinuidades_6m = sum(continuidad_6m == "Discontinuidad"),
    discontinuidades_1m = sum(continuidad_1m == "Discontinuidad")
  ) %>%
  mutate(
    discontinuidades_cat_12m = case_when(n_matr_corr == 1 ~ "Un solo curso",
                                         n_matr_corr > 1 & discontinuidades_12m== 1 ~ "Mas de un curso consecutivo/continuo",
                                         n_matr_corr > 1 & discontinuidades_12m == 2 ~ "Una discontinuidad",
                                         n_matr_corr > 1 & discontinuidades_12m > 2 ~ ">1 discontinuidad",
                                         TRUE ~ "Otro caso"),
    discontinuidades_cat_6m = case_when(n_matr_corr == 1 ~ "Un solo curso",
                                        n_matr_corr > 1 & discontinuidades_6m== 1 ~ "Mas de un curso consecutivo/continuo",
                                        n_matr_corr > 1 & discontinuidades_6m == 2 ~ "Una discontinuidad",
                                        n_matr_corr > 1 & discontinuidades_6m > 2 ~ ">1 discontinuidad",
                                        TRUE ~ "Otro caso"),                                     
    discontinuidades_cat_1m = case_when(n_matr_corr == 1 ~ "Un solo curso",
                                        n_matr_corr > 1 & discontinuidades_1m== 1 ~ "Mas de un curso consecutivo/continuo",
                                        n_matr_corr > 1 & discontinuidades_1m == 2 ~ "Una discontinuidad",
                                        n_matr_corr > 1 & discontinuidades_1m > 2 ~ ">1 discontinuidad",
                                        TRUE ~ "Otro caso"),  
    
  )

dades <- dades %>%
  group_by(codialumne) %>%
  mutate(
    tiempo_reenganche_12m = ifelse((continuidad_12m == "Continuidad" & lag(continuidad_12m) == "Discontinuidad")|
                                     (continuidad_12m == "Discontinuidad" & lag(continuidad_12m) == "Discontinuidad"),
                                   dadesinicicurs - lag(dadesinicicurs),NA),
    tiempo_reenganche_6m = ifelse((continuidad_6m == "Continuidad" & lag(continuidad_6m) == "Discontinuidad")|
                                    (continuidad_6m == "Discontinuidad" & lag(continuidad_6m) == "Discontinuidad"),
                                  dadesinicicurs - lag(dadesinicicurs),NA),
    tiempo_reenganche_1m = ifelse((continuidad_1m == "Continuidad" & lag(continuidad_1m) == "Discontinuidad")|
                                    (continuidad_1m == "Discontinuidad" & lag(continuidad_1m) == "Discontinuidad"),
                                  dadesinicicurs - lag(dadesinicicurs),NA)
  )
dades <- mutate(dades,
                assoliment_grup_cat = case_when(
                  assoliment_grup %in% c('Apte', 'No apte') ~ 'Finaliza',
                  assoliment_grup %in% c('baixa_abandonament', 'No presentat/Sense avaluació') ~ 'No finaliza',
                  TRUE ~ 'Otros'
                )
)

#View(dades[,c("codialumne","codicurs","dadesinicicurs","dias_entre_cursos","continuidad","tiempo_reenganche")])
names(dades)


# LlenguaPetit_1 ----------------------------------------------------------

conteo_llenguaPetit1 <- dades %>%
  group_by(codialumne) %>%
  summarise(n_distinct = n_distinct(LlenguaPetit_1))
alumnos_con_llenguapetit_diferente <- conteo_llenguaPetit1 %>%
  filter(n_distinct > 1)
# 2885
#View(dades[dades$codialumne %in%alumnos_con_agrpais_diferente$codialumne,c("codialumne","agrpais","dadesinicicurs")])
dades$LlenguaPetit_1[dades$LlenguaPetit_1==""] <- NA
dades$LlenguaPetit_1[dades$LlenguaPetit_1==" "] <- NA
alumnos_con_llenguapetit_diferente <- alumnos_con_llenguapetit_diferente$codialumne
#primero unificar valores si existe alguna vacia 
for (alumno in alumnos_con_llenguapetit_diferente) {
  llengua_alumno <- dades$LlenguaPetit_1[dades$codialumne == alumno]
  valor_distinto_na <-  llengua_alumno[!is.na(llengua_alumno)]
  dades$LlenguaPetit_1[dades$codialumne == alumno & is.na(dades$LlenguaPetit_1)] <- valor_distinto_na
}
conteo_llenguaPetit1 <- dades %>%
  group_by(codialumne) %>%
  summarise(n_distinct = n_distinct(LlenguaPetit_1))
alumnos_con_llenguapetit_diferente <- conteo_llenguaPetit1 %>%
  filter(n_distinct > 1)
# 671
#View(dades[dades$codialumne %in%alumnos_con_llenguapetit_diferente$codialumne,c("codialumne","LlenguaPetit_1","dadesinicicurs")])
alumnos_con_llenguapetit_diferente <- alumnos_con_llenguapetit_diferente$codialumne
for (alumno in alumnos_con_llenguapetit_diferente) {
  llengua_alumno <- dades$LlenguaPetit_1[dades$codialumne == alumno]
  valor_distinto_na <-  unique(llengua_alumno[llengua_alumno != "Sense dades"])
  dades$LlenguaPetit_1[dades$codialumne == alumno & dades$LlenguaPetit_1 == "Sense dades"] <- valor_distinto_na
}
conteo_llenguaPetit1 <- dades %>%
  group_by(codialumne) %>%
  summarise(n_distinct = n_distinct(LlenguaPetit_1))
alumnos_con_llenguapetit_diferente <- conteo_llenguaPetit1 %>%
  filter(n_distinct > 1)
# 516
#View(dades[dades$codialumne %in%alumnos_con_llenguapetit_diferente$codialumne,c("codialumne","LlenguaPetit_1","LlenguaPetit_2","dadesinicicurs")])
subtabla <- dades[dades$codialumne %in% alumnos_con_llenguapetit_diferente$codialumne,c("codialumne","LlenguaPetit_1","LlenguaPetit_2","codicurs","dadesinicicurs") ]
save(subtabla,file = "LlenguaPetit1_confuso.Rdades")
write.csv(subtabla, file="LlenguaPetit1_confuso.csv")
#SOLUCION: escoger la categoria modal Y SI HAY EMPATE LA DEL ULTIMO CURSACADEMIC  
names(dades)
dades$LlenguaPetit_1_corr <- dades$LlenguaPetit_1

# Paso 1: Calcular la moda de LlenguaPetit_1_corr
llengua_mayoritaria <- dades %>%
  filter(codialumne %in% alumnos_con_llenguapetit_diferente$codialumne) %>%
  group_by(codialumne, LlenguaPetit_1_corr) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(codialumne, desc(count)) %>%
  group_by(codialumne) %>%
  mutate(rank = row_number(), 
         max_count = max(count)) %>%
  filter(count == max_count) %>%
  mutate(empate = ifelse(n() > 1, 1, 0)) %>%
  filter(rank == 1) %>%
  select(codialumne, LlenguaPetit_1_corr, empate)

# Paso 2: Obtener la LlenguaPetit_1_corr más reciente por `codialumne`
llengua_mas_reciente <- dades %>%
  filter(codialumne %in% alumnos_con_llenguapetit_diferente$codialumne) %>%
  group_by(codialumne) %>%
  arrange(desc(cursacademic)) %>%
  slice(1) %>%
  select(codialumne, LlenguaPetit_1_corr) %>%
  rename(LlenguaPetit_1_corr_reciente = LlenguaPetit_1_corr)

# Paso 3: Combinar los resultados
llengua_final <- merge(llengua_mayoritaria, llengua_mas_reciente, by = "codialumne", all.x = TRUE)

# Seleccionar la moda si no hay empate, si hay empate seleccionar la más reciente
llengua_final <- llengua_final %>%
  mutate(LlenguaPetit_1 = ifelse(empate == 1, LlenguaPetit_1_corr_reciente, LlenguaPetit_1_corr)) %>%
  select(codialumne, LlenguaPetit_1)%>%
  rename(LlenguaPetit_1_corr = LlenguaPetit_1)

# Verificar los resultados
#View(llengua_final)

dades <- merge(dades, llengua_final, by = "codialumne", all.x = TRUE)
names(dades)
#View(dades[,c("codialumne","cursacademic","LlenguaPetit_1","LlenguaPetit_1_corr.x","LlenguaPetit_1_corr.y")])
dades$LlenguaPetit_1_corr <- ifelse(is.na(dades$LlenguaPetit_1_corr.y), dades$LlenguaPetit_1_corr.x, dades$LlenguaPetit_1_corr.y)
names(dades)
#View(dades[,c("codialumne","cursacademic","LlenguaPetit_1","LlenguaPetit_1_corr","LlenguaPetit_1_corr.x","LlenguaPetit_1_corr.y")])
dades$LlenguaPetit_1_corr.x<- NULL
dades$LlenguaPetit_1_corr.y<- NULL
colnames(dades)[colnames(dades) == 'LlenguaPetit_1'] <- 'LlenguaPetit_1_orig'
names(dades)



# agrupar idiomes ---------------------------------------------------------
# Asignar valores a 'llengua_inicial1' y 'llengua_inicial2' basado en las condiciones
dades <- dades %>%
  mutate(llengua_inicial1 = case_when(
    LlenguaPetit_1_corr == "Català" ~ 1,
    LlenguaPetit_1_corr %in% c("Castellà", "Gallec", "Basc") ~ 2,
    LlenguaPetit_1_corr %in% c("Italià", "Francès", "Portuguès", "Romanès/moldau", "Sard") ~ 3,
    LlenguaPetit_1_corr %in% c("Àrab", "Àrab hassania") ~ 4,
    is.na(LlenguaPetit_1_corr) ~ 5,
    LlenguaPetit_1_corr %in% c("", "Sense dades") ~ 6,
    TRUE ~ 5  # Por defecto, si ninguna condición se cumple, asignar 5 (equivalente a . en Stata)
  )) %>%
  mutate(llengua_inicial2 = case_when(
    LlenguaPetit_2 == "Català" ~ 1,
    LlenguaPetit_2 %in% c("Castellà", "Gallec", "Basc") ~ 2,
    LlenguaPetit_2 %in% c("Italià", "Francès", "Portuguès", "Romanès/moldau", "Sard") ~ 3,
    LlenguaPetit_2 %in% c("Àrab", "Àrab hassania") ~ 4,
    is.na(LlenguaPetit_2) ~ 5,
    LlenguaPetit_2 %in% c("", "Sense dades") ~ 6,
    TRUE ~ 5  
  ))

# Definir etiquetas para los valores de 'llengua_inicial1' y 'llengua_inicial2'
llengini_labels <- c("Català", "Castellà o llengua co-oficial", "Llengua Romànica", "Àrab", "Altres", "Sense dades")
# Asignar etiquetas como factor
dades <- dades %>%
  mutate(llengua_inicial1 = factor(llengua_inicial1, labels = llengini_labels),
         llengua_inicial2 = factor(llengua_inicial2, labels = llengini_labels))
save(dades, file= "cpnl_final20240614.Rdades")
#load("cpnl_final20240614.Rdades")
names(dades)


# INDICE DE FRAC CON LlenguaPetit_1_corr ----------------------------------------------------------------------

# Step 1: Create a count variable for each individual
dades$individual_count <- 1

# Step 2: Collapse the dades to get the count of each ethnicity in each district
collapsed_dades <- dades %>% 
  group_by(codicurs, LlenguaPetit_1_corr) %>%
  summarise(individual_count = sum(individual_count)) %>%
  ungroup()

# Step 3: Calculate the total population in each district
collapsed_dades <- collapsed_dades %>%
  group_by(codicurs) %>%
  mutate(total_population = sum(individual_count)) %>%
  ungroup()

# Step 4: Calculate the proportion of each ethnic group in each district
collapsed_dades <- collapsed_dades %>%
  mutate(prop_paiscorr = individual_count / total_population)

# Step 5: Calculate the Fractionalization Index at the district level
fractionalization_llenguapetit1_corr <- collapsed_dades %>%
  group_by(codicurs) %>%
  summarise(sum_prop2 = sum(prop_paiscorr^2)) %>%
  mutate(fractionalization_llenguapetit1_corr = 1 - sum_prop2) %>%
  select(codicurs, fractionalization_llenguapetit1_corr)
dades <- merge(dades, fractionalization_llenguapetit1_corr, by = "codicurs", all.x = TRUE)
names(dades)
dades$individual_count <- NULL
# dummy 2015 RETOCADA ----------------------------------------------------------------------

dades <- dades %>%
  group_by(codialumne) %>%
  mutate(
    primer_año = min(any_curs)
  )
# VAMOR A UTILIZAR MEJOR PAIS_CORR DIRECTAMENTE 
UE_corr <- c("Alemanya", "Àustria", "Bèlgica", "Bulgària", "Chequia", "Xipre", "Croàcia", "Dinamarca", "Eslovàquia", "Eslovènia", "Estònia", "Finlàndia",
             "França", "Grècia", "Hongria", "Irlanda", "Itàlia", "Letònia", "Lituània", "Luxemburg", "Malta", "Països Baixos", "Polonia", "Portugal", 
             "Romania" ,"Suècia", "Liechtenstein", "Suïssa", "Andorra", "Noruega","Polònia","Islàndia","República txeca","San Marino","Vaticà")
España <- c("Andalusia","Aragó","Astúries","Canàries","Cantàbria","Castella-la Manxa","Castella-Lleó","Catalunya","Ceuta-Melilla","Extremadura","Galícia",
            "Illes Balears","Madrid","Múrcia","Navarra","País Basc","País Valencià","Rioja, la")
# document_aportat que es vulnerable o "faltan papeles"
no_papeles <-c("Altres","Document identificatiu altres països","Passaport")

# DUMMY para analizar si el cambio de la ley de 2015 tiene algo que ver
# dummy = 1 si el indiv no es europeo y su primer curso es 2015 
dades$afectat_llei2015 <- ifelse(dades$pais_corr %nin% c(UE_corr,España) & 
                                   dades$document_aportat %in% no_papeles & dades$primer_año >= 2015, 1, 0)

# TAMBIEN DEBEN DE CONTARSE COMO dummy=1 LOS ALUMNOS <2015 + "SIN  PAPELES"+ ASISTENCIA <75% 
# ya que tambien es poblacion sensible o afectada por esta ley 

condiciones <- dades %>%
  group_by(codialumne) %>%
  summarise(
    cumple_condiciones = all(cursacademic < 2015 & percentassistgrup != "75-100") & 
      all(pais_corr %nin% c(UE_corr,España)) & 
      all(document_aportat %in% no_papeles)
  )

# Unir el resumen con los datos originales y actualizar afectat_llei2015
dades <- dades %>%
  left_join(condiciones, by = "codialumne") %>%
  mutate(
    afectat_llei2015 = if_else(cumple_condiciones, 1, afectat_llei2015)
  ) %>%
  select(-cumple_condiciones)  # Eliminar la columna temporal

# pon un 0 en las matriculas <2015 para todos los individuos (porque en esas matriculas NO les afecta la ley porque no existia)
dades[which(dades$cursacademic <2015),"afectat_llei2015"] <- 0

table(factor(dades$afectat_llei2015))
#View(dades[which(dades$pais_corr %nin% UE),c("codialumne","cursacademic","percentassistgrup","document_aportat","afectat_llei2015")]) 

# n_estud -------------------------------------------------------------------
dades <- dades %>%
  group_by(codicurs) %>%
  mutate(n_estud = n_distinct(codialumne[assoliment_grup != "no_inicia"]))

#View(dades[which(dades$codicurs=="00201B1PRB00723"),c("codicurs","codialumne","assoliment_grup","n_estud","n_estud_complet")])

# fraccionalizacio del MODELO -------------------------------------------------------------------

dades <- dades %>%
  mutate(pais_aux = ifelse(assoliment_grup == "no_inicia", NA, pais_corr_mod),
         LlenguaPetit1_aux=ifelse(assoliment_grup == "no_inicia", NA, LlenguaPetit_1_corr))

# en pais_aux ponemos todas las ccaa menos ctalunya con el mismo valor 
ccaa <- c("Andalusia","Aragó","Astúries","Canàries","Cantàbria","Castella-la Manxa","Castella-Lleó","Ceuta-Melilla","Extremadura","Galícia",
          "Illes Balears","Madrid","Múrcia","Navarra","País Basc","País Valencià","Rioja, la")
#ccaa <- setdiff(España,"Catalunya")
dades$pais_aux[which(dades$pais_aux %in% ccaa)] <- "ccaa"


#CON PAIS_aux

dades$individual_count <- 1

collapsed_data <- dades %>% 
  group_by(codicurs, pais_aux) %>%
  summarise(individual_count = sum(individual_count)) %>%
  ungroup()

collapsed_data <- collapsed_data %>%
  group_by(codicurs) %>%
  mutate(total_population = sum(individual_count)) %>%
  ungroup()

collapsed_data <- collapsed_data %>%
  mutate(prop_paiscorr = individual_count / total_population)

fractionalization_pais_modelo <- collapsed_data %>%
  group_by(codicurs) %>%
  summarise(sum_prop2 = sum(prop_paiscorr^2)) %>%
  mutate(fractionalization_pais_modelo = 1 - sum_prop2) %>%
  select(codicurs, fractionalization_pais_modelo)
dades <- merge(dades, fractionalization_pais_modelo, by = "codicurs", all.x = TRUE)
names(dades)

# CON LLENGUA PETTIT1
collapsed_data <- dades %>% 
  group_by(codicurs, LlenguaPetit1_aux) %>%
  summarise(individual_count = sum(individual_count)) %>%
  ungroup()

collapsed_data <- collapsed_data %>%
  group_by(codicurs) %>%
  mutate(total_population = sum(individual_count)) %>%
  ungroup()

collapsed_data <- collapsed_data %>%
  mutate(prop_paiscorr = individual_count / total_population)

fractionalization_llenguapetit1_modelo <- collapsed_data %>%
  group_by(codicurs) %>%
  summarise(sum_prop2 = sum(prop_paiscorr^2)) %>%
  mutate(fractionalization_llenguapetit1_modelo = 1 - sum_prop2) %>%
  select(codicurs, fractionalization_llenguapetit1_modelo)
dades <- merge(dades, fractionalization_llenguapetit1_modelo, by = "codicurs", all.x = TRUE)
names(dades)
dades$individual_count <- NULL


#save(dades, file= "cpnl_final20240621.RData")
load("cpnl_final20240621.RData")

# CORRECCION DE VARIABLES PC_xx -----------------------------------------------------------------------
# estos porcentajes en el aula deben calcularse SIN ASSOLIMENT_GRUP= NO INICIA
# ya que esos indiv es como si no empezaran el curso por lo que no "afectan" al assoliment de los que si 
#  Originales de Stata: pc_esp, esp, pc_esp_cat, esp_cat

#ESTABAN MAL ASI QUE LOS BORRAMOS FIRST
dades$pc_al <- NULL
dades$pc_ccaa <- NULL
dades$pc_eo <- NULL
dades$pc_esp_cat <- NULL
dades$pc_cat <- NULL
dades$pc_op <- NULL
dades$pc_se <- NULL
dades$pc_sec <- NULL
dades$pc_ue <- NULL
dades$pc_univ <- NULL

# Calcular pc_XX para las filas donde assoliment_grup != "no_inicia"

pc_xx_calculation <- dades %>%
  filter(assoliment_grup != "no_inicia") %>%
  mutate(
    # Composicion de la clase por pais de origen
    esp_cat = ifelse(agrpais_corr_mod == "Catalunya" | agrpais_corr_mod == "Resta de l'Estat Espanyol", 1, 
                     ifelse(agrpais_corr_mod == "Sense dades", NA, 0)),
    esp = ifelse(agrpais_corr_mod == "Resta de l'Estat Espanyol", 1, ifelse(agrpais_corr_mod == "Sense dades", NA, 0)),
    cat = ifelse(agrpais_corr_mod == "Catalunya", 1, ifelse(agrpais_corr_mod == "Sense dades", NA, 0)),
    ue = ifelse(agrpais_corr_mod == "Unió Europea", 1, ifelse(agrpais_corr_mod == "Sense dades", NA, 0)),
    al = ifelse(agrpais_corr_mod == "Amèrica Llatina", 1, ifelse(agrpais_corr_mod == "Sense dades", NA, 0)),
    op = ifelse(agrpais_corr_mod %in% c("Àsia", "EUA i Canadà", "Nord d'Àfrica",
                                        "Oceania", "Europa Extracomunitària",
                                        "Resta d'Àfrica"), 1, 
                ifelse(agrpais_corr_mod == "Sense dades", NA, 0)),
    # Composicion de la clase por niveles de estudio 
    univ = ifelse(agrestudis_mod== "Universitaris (grau, màster, dip., llic., doct.)", 1, ifelse(agrestudis_mod== "Sense dades",NA,0)),
    sec = ifelse(agrestudis_mod== "Secundària (batx., f. prof.grau sup., FP 2n grau)", 1, ifelse(agrestudis_mod== "Sense dades",NA,0)),
    eo = ifelse(agrestudis_mod== "Educació obligatoria" , 1, ifelse(agrestudis_mod== "Sense dades",NA,0)),
    se = ifelse(agrestudis_mod== "Sense estudis", 1, ifelse(agrestudis_mod== "Sense dades",NA,0))
  ) %>%
  group_by(codicurs) %>%
  summarise(
    pc_esp_cat = mean(esp_cat, na.rm = TRUE),
    pc_ccaa = mean(esp, na.rm = TRUE),
    pc_cat = mean(cat, na.rm = TRUE),
    pc_ue = mean(ue, na.rm = TRUE),
    pc_al = mean(al, na.rm = TRUE),
    pc_op = mean(op, na.rm = TRUE),
    pc_univ = mean(univ, na.rm = TRUE),
    pc_sec = mean(sec, na.rm = TRUE),
    pc_eo = mean(eo, na.rm = TRUE),
    pc_se = mean(se, na.rm = TRUE)
  ) %>%
  ungroup()
# Unir la tabla auxiliar con la base de datos original
dades <- dades %>%
  left_join(pc_xx_calculation, by = "codicurs") %>%
  mutate(
    pc_esp_cat = ifelse(assoliment_grup == "no_inicia", NA, pc_esp_cat),
    pc_ccaa = ifelse(assoliment_grup == "no_inicia", NA, pc_ccaa),
    pc_cat = ifelse( assoliment_grup == "no_inicia", NA, pc_cat),
    pc_ue = ifelse (assoliment_grup == "no_inicia", NA, pc_ue),
    pc_al = ifelse(assoliment_grup == "no_inicia", NA, pc_al),
    pc_op = ifelse(assoliment_grup == "no_inicia", NA, pc_op),
    pc_univ = ifelse(assoliment_grup=="no_inicia", NA, pc_univ),
    pc_sec = ifelse(assoliment_grup == "no_inicia", NA, pc_sec),
    pc_eo = ifelse(assoliment_grup == "no_inicia", NA, pc_eo), 
    pc_se = ifelse(assoliment_grup =="no_inicia", NA, pc_se)
  )

dades <- dades %>%
  mutate(
    esp_cat = ifelse(agrpais_corr_mod == "Catalunya" | agrpais_corr_mod == "Resta de l'Estat Espanyol", 1, NA),
    esp_cat = ifelse(agrpais_corr_mod == "Sense dades", NA, esp_cat)
  )
# % personas hispanohablantes (incluye personas nacidas en Cataluña)
colnames(dades)[colnames(dades) == 'esp_cat'] <- 'castparl'
dades$castparl[dades$agrpais_corr_mod == "Amèrica Llatina" & dades$lleng_latam == ""] <- 1

#duplicated_columns <- names(dades)[duplicated(names(dades))]
#print(duplicated_columns)
# Eliminar columnas duplicadas
#dades <- dades %>% select(-one_of(duplicated_columns))


# agrpais2 -------------------------------------------------------------------

#estos todos fueron creados durante el modelo8 y se han trasladado aqui

#AGRUPAMOS UN POCO LA VAR PAIS SIN QUE SEA AGRPAIS (para evitar estancamiento del modelo)
# Crear tabla de frecuencias para pais_corr_mod
frecuencias <- table(dades$pais_corr_mod)
# Inicializar variable_nueva con NA
dades$variable_nueva <- NA
# Asignar valores a variable_nueva
dades$variable_nueva <- ifelse(frecuencias[dades$pais_corr_mod] < 100, dades$agrpais_corr, dades$pais_corr_mod)
colnames(dades)[colnames(dades) == 'variable_nueva'] <- 'agrpais2'

# agrnomcnl -------------------------------------------------------------------

# Agrupar nomcnl "serveis centrals" a "CNL de Barcelona" 
dades$nomcnl_agr <- recode(dades$nomcnl.x, "Serveis Centrals"= "CNL de Barcelona")

# VD -------------------------------------------------------------------
#Creo la Variable Dependiente 1-0 para simplificar tiempo computacional
#NOTA: aunque aqui los de no_inicia les he puesto un 2, aunque al hacer el subset se quitaran
dades$VD <- ifelse(dades$assoliment_grup=="baixa_abandonament",1,ifelse(dades$assoliment_grup=="no_inicia",2,0))

# VD Continuidad -------------------------------------------------------------------
dades$VDC_12m <- ifelse(dades$continuidad_12m=="Continuidad",1,0)
dades$VDC_6m <- ifelse(dades$continuidad_6m=="Continuidad",1,0)
dades$VDC_1m <- ifelse(dades$continuidad_1m=="Continuidad",1,0)

# nivell_desagrupat -------------------------------------------------------------------

dades$nivell_desagrupat <- NA

# Reemplazar los valores según las condiciones
dades$nivell_desagrupat[dades$descragrupniv == "Inicials"] <- 1
dades$nivell_desagrupat[dades$nivell == "Bàsic 1"] <- 2
dades$nivell_desagrupat[dades$nivell == "Bàsic 1-2 (CB)"] <- 2
dades$nivell_desagrupat[dades$nivell == "Bàsic 1-2-3"] <- 2

dades$nivell_desagrupat[dades$nivell == "Bàsic 2"] <- 3
dades$nivell_desagrupat[dades$nivell == "Bàsic 2-3"] <- 3

dades$nivell_desagrupat[dades$nivell == "Bàsic 3"] <- 4
dades$nivell_desagrupat[dades$nivell == "Bàsic 3-Elemental 1 (BE)"] <- 4

dades$nivell_desagrupat[dades$descragrupniv == "Elementals"] <- 5
dades$nivell_desagrupat[dades$descragrupniv == "Intermedis"] <- 6
dades$nivell_desagrupat[dades$descragrupniv == "Suficiència"] <- 7
dades$nivell_desagrupat[dades$descragrupniv == "Superior"] <- 8

# Asignar etiquetas a los valores de nivell_desagrupat
dades$nivell_desagrupat <- factor(dades$nivell_desagrupat, 
                                 levels = 1:8, 
                                 labels = c("Inicials", "Bàsic 1", "Bàsic 2", "Bàsic 3", 
                                            "Elementals", "Intermedis", "Suficiència", "Superior"))
# NA COMO CATEGORIA DE DIST_MIN_AULA_xx -------------------------------------------------------------------
library(forcats)

dades$dist_min_aula_cat_12m <- fct_explicit_na(dades$dist_min_aula_cat_12m,"NA")
dades$dist_min_aula_cat_6m <- fct_explicit_na(dades$dist_min_aula_cat_6m,"NA")
dades$dist_min_aula_cat_1m <- fct_explicit_na(dades$dist_min_aula_cat_1m,"NA")

dades$idprof <- as.factor(dades$idprof)
dades$nivell_desagrupat <- as.factor(dades$nivell_desagrupat)
dades$intensivitat <- as.factor(dades$intensivitat)

#table(dades$dist_min_aula_cat_6m) para ver el NA como categoria o levels(dades$dist_mi_aula_cat_12m)

save(dades, file= "cpnl_final20240910.RData")



# STATA -------------------------------------------------------------------
library(foreign)
library(haven)
# Convert dades frame to character type
dades <- as.data.frame(lapply(dades, as.character), stringsAsFactors = FALSE)
# Convert empty strings to NA
for (col in names(dades)) {
  dades[[col]] <- zap_empty(dades[[col]])
}
write.dta(dades, "cpnl_final20240910.dta")

