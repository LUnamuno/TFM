#install.packages("openxlsx")
#install.packages("reshape2")

library(haven)
library(dplyr)
library(ggplot2)
library(reshape2)
library("openxlsx")
library(gridExtra) # plot combinado
library(xtable)

load("cpnl_final20240910_DEF.RData")

colnames(dades)[colnames(dades) == 'codicnl.x'] <- 'codicnl'

# BLOQUE 1 ----------------------------------------------------------------

#install.packages("openxlsx")
#library("openxlsx")
#wb <- createWorkbook()

# Agregar cada tabla como una hoja en el libro de Excel:

#addWorksheet(wb, sheet = "tabla1")
#writeData(wb, sheet = sheet_name, x = "Main Title", startRow = 1, startCol = 1)
#writeData(wb, sheet = sheet_name, x = "Subtitle", startRow = 2, startCol = 1)
#writeData(wb, sheet = sheet_name, x = tabla1,startRow=3)

# Para agregar un grafico como una foto

##print(name_plot)
#insertPlot(wb, sheet_name, startCol = ncol(tabla1)+10, startRow = 3)

# Guardar el libro de Excel:

#saveWorkbook(wb, file = "Bloque1_tablas.xlsx", overwrite = TRUE)
wb <- createWorkbook()
addWorksheet(wb, "INDEX")
writeData(wb, sheet = "INDEX", x = "Name of the table" , startRow=1,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Description" , startRow=1,startCol = 2)
#-------------------------------------------------------------------------------
#1)	Evolucio temporal del nombre de centres (agrupada i per nivells). 
#2)	Evolucio temporal del nombre de matriculats i estudiants (agrupada i per nivells) 
#-------------------------------------------------------------------------------
addWorksheet(wb, "t_1_1")
tabla1 <- dades %>% 
  group_by(academic_course) %>%
  summarize(
    n_enrollments =n(), 
    n_students = n_distinct(codialumne),
    n_centres = n_distinct(codicnl)
  )

#print(tabla1)
writeData(wb, sheet = "t_1_1", x = "Time evolution of number of enrollments, students and centres", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_1_1", x = tabla1, startRow=3)
writeData(wb, sheet = "INDEX", x = "t_1_1" , startRow=2,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of number of enrollments, students and centres" , startRow=2,startCol = 2)


# by course_level
tabla1c <- dades %>% 
  group_by(academic_course,course_level) %>%
  summarize(
    n_enrollments =n(), 
    n_students = n_distinct(codialumne),
    n_centres = n_distinct(codicnl)
  )


n_matr_widec <-dcast(tabla1c[,1:3], academic_course ~ course_level) 
addWorksheet(wb, "t_5_1")
writeData(wb, sheet = "t_5_1", x = "Time evolution of number of enrollments by course levels", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_5_1", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_5_1", x = n_matr_widec, startRow=3)
writeData(wb, sheet = "INDEX", x = "t_5_1" , startRow=6,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of number of enrollments by course levels" , startRow=6,startCol = 2)

xtable(n_matr_widec, caption= "Time evolution of number of enrollments by course levels")

n_students_widec <-dcast(tabla1c[,c(1:2,4)], academic_course ~ course_level) 
addWorksheet(wb, "t_6_1")
writeData(wb,sheet = "t_6_1", x = "Time evolution of number of students by course levels", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_6_1", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_6_1", x = n_students_widec, startRow=3)
writeData(wb, sheet = "INDEX", x = "t_6_1" , startRow=7,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of number of students by course levels" , startRow=7,startCol = 2)

xtable(n_students_widec, caption= "Time evolution of number of students by course levels")

n_centres_widec <- dcast(tabla1c[,c(1:2,5)], academic_course ~ course_level) 
addWorksheet(wb, "t_7_1")
writeData(wb,sheet = "t_7_1", x = "Time evolution of number of centres by course levels", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_7_1", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_7_1", x = n_centres_widec, startRow=3)
writeData(wb, sheet = "INDEX", x = "t_7_1" , startRow=8,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of number of centres by course levels" , startRow=8,startCol = 2)


#-------------------------------------------------------------------------------
#3)	Evolucio temporal del nombre de cursos per tipologia de curs 
#-------------------------------------------------------------------------------
tabla <- dades %>% 
  group_by(academic_course,intensivity) %>%
  summarize(
    n_enrollments =n()
  )

addWorksheet(wb, "t_8_1")
n_matr_widec <-dcast(tabla, academic_course ~ intensivity) 
writeData(wb, sheet = "t_8_1", x = "Time evolution of number of enrollments by intensivity", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_8_1", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_8_1", x = n_matr_widec, startRow=3)
writeData(wb, sheet = "INDEX", x = "t_8_1" , startRow=9,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of number of enrollments by intensivity" , startRow=9,startCol = 2)


xtable(n_matr_widec, caption= "Time evolution of number of enrollments by intensivity")

addWorksheet(wb, "t_10_1")
tabla3 <- dades %>% 
  group_by(academic_course,intensivity,course_level) %>%
  summarize(
    n_enrollments =n()
  )
#print(tabla3)
writeData(wb, sheet = "t_10_1", x = "Time evolution of number of enrollments by intensivity and course levels", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_10_1", x = tabla3, startRow=3)
writeData(wb, sheet = "INDEX", x = "t_10_1" , startRow=11,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of number of enrollments by intensivity and course levels" , startRow=11,startCol = 2)

#-------------------------------------------------------------------------------
#4)	Evolucio temporal del nombre de cursos (agrupada i per nivells) POR descragrupniv Y NIVELL_MECR
#-------------------------------------------------------------------------------
addWorksheet(wb, "t_11_1")

tabla3 <- dades %>% 
  group_by(academic_course) %>%
  summarize(
    n_courses = n_distinct(codicurs)
  )
#print(tabla3)
writeData(wb, sheet = "t_11_1", x = "Time evolution of number of courses", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_11_1", x = tabla3, startRow=3)
writeData(wb, sheet = "INDEX", x = "t_11_1" , startRow=12,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of number of courses" , startRow=12,startCol = 2)

xtable(tabla3, caption= "Time evolution of number of courses")

tabla3b <- dades %>% 
  group_by(academic_course,course_level) %>%
  summarize(
    n_courses = n_distinct(codicurs)
  )

n_cursos_wide <-dcast(tabla3b[,1:3], academic_course ~ course_level) 
addWorksheet(wb, "t_12_1")
writeData(wb, sheet = "t_12_1", x = "Evolució temporal del nombre de cursos per nivells", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_12_1", x = n_cursos_wide, startRow=3)
writeData(wb, sheet = "INDEX", x = "t_12_1" , startRow=13,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Evolució temporal del nombre de cursos per nivells" , startRow=13,startCol = 2)

xtable(n_cursos_wide, caption = "Evolució temporal del nombre de cursos per nivells")

#-------------------------------------------------------------------------------
#5)	Evolucio temporal de la mitjana d'estudiants per curs (agrupada i per nivells) 
#-------------------------------------------------------------------------------
# por niveles_merc
tabla4b <- dades %>% 
  group_by(academic_course, course_level) %>%
  summarize(
    mean_stud_centres = round(n_distinct(codialumne)/n_distinct(codicnl)*100,2), 
    mean_stud_aula = round(mean(n_stud),2) 
  )
#print(tabla4b)
addWorksheet(wb, "t_15_1")
n_mitj_wide <-dcast(tabla4b[,1:3], academic_course ~ course_level) 
writeData(wb, sheet = "t_15_1", x = "Time evolution of mean of students by  centre", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_15_1", x = "By course levels", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_15_1", x = n_mitj_wide, startRow=3)
writeData(wb, sheet = "INDEX", x = "t_15_1" , startRow=16,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of mean of students by centre and by course levels" , startRow=16,startCol = 2)

addWorksheet(wb, "t_16_1")
n_mitj_wide2 <-dcast(tabla4b[,c(1:2,4)], academic_course ~ course_level) 
writeData(wb, sheet = "t_16_1", x = "Time evolution of mean of students by classroom ", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_16_1", x = "By course levels", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_16_1", x = n_mitj_wide2, startRow=3)
writeData(wb, sheet = "INDEX", x = "t_16_1" , startRow=17,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of mean of students by classroom and by course levels" , startRow=17,startCol = 2)


#-------------------------------------------------------------------------------
#6)	Evolucio temporal del perfil dels estudiants: genere, edat, nivell d'estudi, pais d'origen
#-------------------------------------------------------------------------------
#addWorksheet(wb, sheet = "t_19_1")
tabla5genere <- dades %>% 
  group_by(academic_course,gender) %>%
  summarize(
    n_students = n_distinct(codialumne)
  ) %>%
  group_by(academic_course) %>%
  mutate(
    pct = round((n_students / sum(n_students)) * 100,2)
  )
tabla5genere$gender[tabla5genere$gender==""] <- NA

# FORMATO WIDE:
addWorksheet(wb, sheet = "t_19_1")
n_alumes_wide <-dcast(tabla5genere[,1:3], academic_course ~ gender)
writeData(wb, sheet = "t_19_1", x = "Time evolution of the gender of students", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_19_1", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_19_1", x = n_alumes_wide,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_19_1" , startRow=20,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of the gender of students" , startRow=20,startCol = 2)

# EDAT_MEDIA 
#DUDA
addWorksheet(wb, sheet = "t_20_1")

tabla5edat <- dades %>% 
  group_by(academic_course,edat_cat) %>%
  summarize(
    n_students = n_distinct(codialumne)
  )%>%
  group_by(academic_course) %>%
  mutate(
    pct = round((n_students / sum(n_students)) * 100,2)
  )
#print(tabla5edat)
t_edat_media <- dades %>% 
  group_by(academic_course) %>%
  summarize(
    av_age = round(mean(age, na.rm = TRUE),2), # Calcula la edad media por curso academico
  )

#print(t_edat_media)
n_alumes_wide1 <-dcast(tabla5edat[,1:3], academic_course ~ edat_cat) 
agr <-merge(t_edat_media,n_alumes_wide1, by="academic_course")

writeData(wb, sheet = "t_20_1", x = "Time evolution of the age of the students", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_20_1", x = "Absolute values of ranges and mean values by academic courses", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_20_1", x = agr ,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_20_1" , startRow=21,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of the age of the students" , startRow=21,startCol = 2)


addWorksheet(wb, sheet = "t_21_1")

n_alumes_wide2 <-dcast(tabla5edat[,c(1:2,4)], academic_course ~ edat_cat) 
agr2 <-merge(t_edat_media,n_alumes_wide2, by="academic_course")
writeData(wb, sheet = "t_21_1", x = "Time evolution of the age of the students", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_21_1", x = "Percentage and mean values by academic course", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_21_1", x = agr2,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_21_1" , startRow=22,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of the age of the students-2" , startRow=22,startCol = 2)


#NIVELL DE ESTUDI
addWorksheet(wb, sheet = "t_22_1")

tabla5estudis <- dades %>% 
  group_by(academic_course,studies_grouped) %>%
  summarize( 
    n_students = n_distinct(codialumne)
  )%>%
  group_by(academic_course) %>%
  mutate(
    pct = round((n_students / sum(n_students)) * 100,2)
  )



#WIDE:
addWorksheet(wb, sheet = "t_23_1")
n_alumes_wide <-dcast(tabla5estudis[,1:3], academic_course ~ studies_grouped) 
writeData(wb, sheet = "t_23_1", x = "Time evolution of number of students by study levels", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_23_1", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_23_1", x = n_alumes_wide,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_23_1" , startRow=24,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of number of students by study levels" , startRow=24,startCol = 2)

#Pais de origen
addWorksheet(wb, sheet = "t_24_1")
tabla5agrpais <- dades %>% 
  group_by(academic_course,country_grouped) %>%
  summarize(
    n_students = n_distinct(codialumne)
  )%>%
  group_by(academic_course) %>%
  mutate(
    pct = round((n_students / sum(n_students)) * 100,2)
  )

tabla5agrpais$country_grouped[tabla5agrpais$country_grouped==""] <- NA
#print(tabla5agrpais)
writeData(wb, sheet = "t_24_1", x = "Time evolution of number ofsutentd by country of origin", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_24_1", x = tabla5agrpais,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_24_1" , startRow=25,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of number ofsutentd by country of origin and percentage" , startRow=25,startCol = 2)



#WIDE:
addWorksheet(wb, sheet = "t_25_1")
n_alumes_wide <-dcast(tabla5agrpais[,1:3], academic_course ~ country_grouped) 
writeData(wb, sheet = "t_25_1", x = "Time evolution of number ofsutentd by country of origin", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_25_1", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_25_1", x = n_alumes_wide,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_25_1" , startRow=26,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of number ofsutentd by country of origin-2" , startRow=26,startCol = 2)


#-------------------------------------------------------------------------------
#7)	Perspectiva territorial
#-------------------------------------------------------------------------------
#1 y 2-Numero de alumnos, matriculas y centros: agrupado y por niveles (nivel_mecr y desagrupniv)
#addWorksheet(wb, sheet = "t_26_1")

tabla61 <- dades %>% 
  group_by(academic_course,terr) %>%
  summarize(
    n_enrollments =n(),
    n_students = n_distinct(codialumne),
    n_centres= n_distinct(codicnl)
  )

#print(tabla61)

# WIDE:
n_matr_wide <-dcast(tabla61[,1:3], academic_course ~ terr) 
addWorksheet(wb, "t_26_1")
writeData(wb, sheet = "t_26_1", x = "Time evolution of number of enrollements by territory", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_26_1", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_26_1", x = n_matr_wide, startRow=3)
writeData(wb, sheet = "INDEX", x = "t_26_1" , startRow=27,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of number of enrollements by territory" , startRow=27,startCol = 2)


n_students_wide <-dcast(tabla61[,c(1:2,4)], academic_course ~ terr) 
addWorksheet(wb, "t_27_1")
writeData(wb, sheet = "t_27_1", x = "Time evolution of number of students by territory", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_27_1", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_27_1", x = n_students_wide, startRow=3)
writeData(wb, sheet = "INDEX", x = "t_26_1" , startRow=28,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of number of students by territory" , startRow=28,startCol = 2)


n_centres_wide <-dcast(tabla61[,c(1:2,5)], academic_course ~ terr) 
addWorksheet(wb, "t_28_1")
writeData(wb, sheet = "t_28_1", x = "Time evolution of number of centres by territory", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_28_1", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_28_1", x = n_centres_wide, startRow=3)
writeData(wb, sheet = "INDEX", x = "t_28_1" , startRow=29,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of number of centres by territory" , startRow=29,startCol = 2)


#3-NUEVO:	Evolució temporal del nombre de cursos per tipologia de curs


tabla <- dades %>% 
  group_by(academic_course,intensivity) %>%
  summarize(
    n_enrollments =n()
  )

addWorksheet(wb, "t_31_1")
#AQUI ME QUEDÉ 
n_matr_widec <-dcast(tabla, academic_course ~ intensivity) #n_enrollments por niveles
writeData(wb, sheet = "t_31_1", x = "Time evolution of the number of enrollments by intensivity", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_31_1", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_31_1", x = n_matr_widec, startRow=3)
writeData(wb, sheet = "INDEX", x = "t_31_1" , startRow=32,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of the number of enrollments by intensivity" , startRow=32,startCol = 2)



#4-Numero de cursos agrupado y por niveles 
addWorksheet(wb, sheet = "t_34_1")
tabla63 <- dades %>% 
  group_by(academic_course,terr) %>%
  summarize(
    n_courses = n_distinct(codicurs)
  )

#print(tabla63)
writeData(wb, sheet = "t_34_1", x = "Time evolution of number of courses by territory", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_34_1", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_34_1", x = tabla63,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_34_1" , startRow=35,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of number of courses by territory", startRow=35,startCol = 2)


#5-Media de estudiantes por curso agrupado y por niveles


# por niveles_merc
tabla4b <- dades %>% 
  group_by(academic_course, course_level) %>%
  summarize(
    mitj_est_centres = round(n_distinct(codialumne)/n_distinct(codicurs)*100,2), 
    mitj_est_aula = round(mean(n_stud),2) 
  )
#print(tabla4b)
addWorksheet(wb, "t_38_1")
n_mitj_wide <-dcast(tabla4b[,1:3], academic_course ~ course_level) 
writeData(wb, sheet = "t_38_1", x = "Time evolution of the mean of students by centre and by course level ", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_38_1", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_38_1", x = n_mitj_wide, startRow=3)
writeData(wb, sheet = "INDEX", x = "t_38_1" , startRow=39,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of the mean of students by centre and by course level", startRow=39,startCol = 2)

addWorksheet(wb, "t_39_1")
n_mitj_wide2 <-dcast(tabla4b[,c(1:2,4)], academic_course ~ course_level) 
writeData(wb, sheet = "t_39_1", x = "Time evolution of the mean of students by classroom and by course level ", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_39_1", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_39_1", x = n_mitj_wide2, startRow=3)
writeData(wb, sheet = "INDEX", x = "t_39_1" , startRow=40,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of the mean of students by classroom and by course level" , startRow=40,startCol = 2)



#6-perfil de estudiante: genero, edad....
addWorksheet(wb, sheet = "t_42_1")

tabla65genere <- dades %>% 
  group_by(academic_course,terr,gender) %>%
  summarize(
    n_students = n_distinct(codialumne)
  )%>%
  group_by(academic_course, terr) %>%
  mutate(
    pct_curs = round((n_students / sum(n_students)) * 100,2)
  )%>%
  group_by(academic_course) %>%
  mutate(
    pct_terr = round((n_students / sum(n_students)) * 100,2)
  )
tabla65genere$gender[tabla65genere$gender==""] <- NA
#print(tabla65genere)
writeData(wb, sheet = "t_42_1", x = "Time evolution of number ofstudents by gender and territory", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_42_1", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_42_1", x = tabla65genere,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_42_1" , startRow=43,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of number ofstudents by gender and territory" , startRow=43,startCol = 2)

# edad
addWorksheet(wb, sheet = "t_43_1")

tabla65edat <- dades %>% 
  group_by(academic_course,terr,edat_cat) %>%
  summarize(
    n_students = n_distinct(codialumne)
  )%>%
  group_by(academic_course, terr) %>%
  mutate(
    pct_curs = round((n_students / sum(n_students)) * 100,2)
  )%>%
  group_by(academic_course) %>%
  mutate(
    pct_terr = round((n_students / sum(n_students)) * 100,2)
  )

#print(tabla65edat)
writeData(wb, sheet = "t_43_1", x = "Time evolution of the age of the students and territory", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_43_1", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_43_1", x = tabla65edat,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_43_1" , startRow=44,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of the age of the students and territory" , startRow=44,startCol = 2)




addWorksheet(wb, sheet = "t_46_1")

tabla <- dades %>% 
  group_by(academic_course,dist_min_12m) %>%
  summarize(
    n_students = n_distinct(codialumne)
  )%>%
  group_by(academic_course) %>%
  mutate(
    pct = round((n_students / sum(n_students)) * 100,2)
  )
#print(tabla)

tabla_wide <-dcast(tabla[,1:3], academic_course ~ dist_min_12m)



#añadir distancia media por curso 
dis_media <- dades %>%
  group_by(academic_course)%>% 
  summarize(
    mean_course = round(mean(dist_min_aula_12m,na.rm=TRUE),2)
    )
tabla_merge <- merge(dis_media,tabla_wide, by= "academic_course" )

writeData(wb, sheet = "t_46_1", x = "Time evolution of the percentage of distance category and mean distance ", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_46_1", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_46_1", x = tabla_merge,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_46_1" , startRow=47,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of the percentage of distance category and mean distance " , startRow=47,startCol = 2)

addWorksheet(wb, sheet = "t_47_1")

tabla_niv <- dades %>% 
  group_by(academic_course,course_level,dist_min_12m) %>%
  summarize(
    n_students = n_distinct(codialumne)
  )%>%
  group_by(academic_course) %>%
  mutate(
    pct = round((n_students / sum(n_students)) * 100,2)
  )%>% 
  select(academic_course,course_level,dist_min_12m,pct)

tabla_niv0 <- na.omit(tabla_niv[tabla_niv$dist_min_12m==0,c("academic_course","course_level","pct")])
colnames(tabla_niv0)[colnames(tabla_niv0) == 'pct_curs'] <- 'pc_0' #pct de 0-s

dis_media_niv <- dades %>%
  group_by(academic_course,course_level)%>% 
  summarize(
    mean_course = round(mean(dist_min_aula_12m,na.rm=TRUE),2)
  )
tabla_merge_niv <- merge(dis_media_niv,tabla_niv0, by= c("academic_course","course_level") )
#print(tabla_merge_niv)
#HACEMOS UN FOR PARA CONSEGUIR CADA TABLA POR NIVEL Y LUEGO CONCATENARLAS EN EL EXCEL 
writeData(wb, sheet = "t_47_1", x = "Time evolution of the 0 and mean distance", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_47_1", x = "By course levels", startRow = 2, startCol = 1)
i <- 0 # primer nivel a escribir
col <- 2 
for (nivel in levels(as.factor(dades$descragrupniv))) {
  if(i==0){
    t <- tabla_merge_niv[tabla_merge_niv$course_level==nivel,c("academic_course","mean_course","pc_0")]
    writeData(wb, sheet = "t_47_1", x = nivel,startRow=3,startCol=col)
    writeData(wb, sheet = "t_47_1", x = t,startRow=4)
    i <- 1
  }
  else{ # porque ya he escrito en la primera iteracion la columna de academic_course, ya no me hace falta escribirla
    
    t <- tabla_merge_niv[tabla_merge_niv$course_level==nivel,c("mean_course","pc_0")]
    writeData(wb, sheet = "t_47_1", x = nivel,startRow=3,startCol=col)
    writeData(wb, sheet = "t_47_1", x = t,startRow=4,startCol=col)
  }
  col <- col + 2
}
writeData(wb, sheet = "INDEX", x = "t_47_1" , startRow=48,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of the 0 and mean distance by course levels" , startRow=48,startCol = 2)

addWorksheet(wb, sheet = "t_48_1")

tabla_amb <- dades %>% 
  group_by(academic_course,terr,dist_min_12m) %>%
  summarize(
    n_students = n_distinct(codialumne)
  )%>%
  group_by(academic_course) %>%
  mutate(
    pct_curs = round((n_students / sum(n_students)) * 100,2)
  )

tabla_terr0 <- na.omit(tabla_amb[tabla_amb$dist_min_12m==0,c("academic_course","terr","pct_curs")])
colnames(tabla_terr0)[colnames(tabla_terr0) == 'pct_curs'] <- 'pc_0' #pct de 0-s

dis_media_terr <- dades %>%
  group_by(academic_course,terr)%>% 
  summarize(
    mitj_course_terr = round(mean(dist_min_12m,na.rm=TRUE),2)
  )

tabla_merge_amb <- merge(dis_media_terr,tabla_terr0, by= c("academic_course","terr" ))

#HACEMOS UN FOR PARA CONSEGUIR CADA TABLA POR terr Y LUEGO CONCATENARLAS EN EL EXCEL 

writeData(wb, sheet = "t_48_1", x = "Time evolution of 0 and mean distance", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_48_1", x = "By territorial areas", startRow = 2, startCol = 1)
i <- 0 # primer nivel a escribir
col <- 2 
for (nivel in levels(as.factor(dades$terr))) {
  if(i==0){
    t <- tabla_merge_amb[tabla_merge_amb$terr==nivel,c("academic_course","mitj_course_terr","pc_0")]
    writeData(wb, sheet = "t_48_1", x = nivel,startRow=3,startCol=col)
    writeData(wb, sheet = "t_48_1", x = t,startRow=4)
    i <- 1
  }
  else{ # porque ya he escrito en la primera iteracion la columna de academic_course, ya no me hace falta escribirla
    
    t_alters <- tabla_merge_amb[tabla_merge_amb$terr==nivel,c("mitj_course_terr","pc_0")]
    writeData(wb, sheet = "t_48_1", x = nivel,startRow=3,startCol=col)
    writeData(wb, sheet = "t_48_1", x = t_alters,startRow=4,startCol=col)
  }
  col <- col + 2
}
writeData(wb, sheet = "INDEX", x = "t_48_1" , startRow=49,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of 0 and mean distance by territorial areas" , startRow=49,startCol = 2)

addWorksheet(wb, sheet = "t_49_1")
tabla<- dades %>% 
  group_by(academic_course,course_level, terr) %>%
  summarize(
    n_students = n_distinct(codialumne)
  )%>%
  group_by(academic_course) %>%
  mutate(
    pct_curs = round((n_students / sum(n_students)) * 100,2)
  )%>% 
  select(academic_course,course_level,terr,pct_curs)
#print(tabla)
tabla_wide <-dcast(tabla[], academic_course + course_level  ~ terr) 

writeData(wb, sheet = "t_49_1", x = "Time evolution of the percentage of students by course levels", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_49_1", x = "Disaggregated by territorial areas", startRow = 2, startCol = 1)

writeData(wb, sheet = "t_49_1", x = tabla_wide,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_49_1" , startRow=50,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of the percentage of students by course levels, disaggregated by territorial areas" , startRow=50,startCol = 2)

addWorksheet(wb, sheet = "t_50_1")
tabla<-   dades %>% 
  group_by(academic_course,country_grouped, terr) %>%
  summarize(
    n_students = n_distinct(codialumne)
  )%>%
  group_by(academic_course) %>%
  mutate(
    pct_curs = round((n_students / sum(n_students)) * 100,2)
  )%>% 
  select(academic_course,country_grouped,terr,pct_curs)
tabla_wide <-dcast(tabla, academic_course + country_grouped  ~ terr)

writeData(wb, sheet = "t_50_1", x = "Time evolution of the percentage of students by country origins", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_50_1", x = "Disaggregated by territorial areas", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_50_1", x = tabla_wide,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_50_1" , startRow=51,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of the percentage of students by country origins, disaggregated by territorial areas" , startRow=51,startCol = 2)


addWorksheet(wb, sheet = "t_51_1")
tablallengua1 <- dades %>% 
  group_by(academic_course,initial_language) %>%
  summarize(
    n_students = n_distinct(codialumne)
  ) %>%
  group_by(academic_course) %>%
  mutate(
    pct = round((n_students / sum(n_students)) * 100,2)
  )
#print(tablallengua1)
n_llengua1 <-dcast(tablallengua1[,1:3], academic_course ~ initial_language) #n_enrollments por niveles

writeData(wb, sheet = "t_51_1", x = "Time evolution of the numebr of students by initial languages", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_51_1", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_51_1", x = n_llengua1,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_51_1" , startRow=53,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of the numebr of students by initial languages" , startRow=53,startCol = 2)


addWorksheet(wb, sheet = "t_52_1")
tablallengua2 <- dades %>% 
  group_by(academic_course,llengua_inicial2) %>%
  summarize(
    n_students = n_distinct(codialumne)
  ) %>%
  group_by(academic_course) %>%
  mutate(
    pct = round((n_students / sum(n_students)) * 100,2)
  )
#print(tablallengua2)
n_llengua2 <-dcast(tablallengua2[,1:3], academic_course ~ llengua_inicial2) #n_enrollments por niveles

writeData(wb, sheet = "t_52_1", x = "Time evolution of the numebr of students by initial second languages", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_52_1", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_52_1", x = n_llengua2,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_52_1" , startRow=54,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of the numebr of students by initial second languages" , startRow=54,startCol = 2)


addWorksheet(wb, sheet = "t_53_1")

tabla <- dades %>% 
  group_by(academic_course,terr,initial_language) %>%
  summarize(
    n_students = n_distinct(codialumne)
  )%>%
  group_by(academic_course, terr) %>%
  mutate(
    pct_curs = round((n_students / sum(n_students)) * 100,2)
  )%>%
  group_by(academic_course) %>%
  mutate(
    pct_terr = round((n_students / sum(n_students)) * 100,2)
  )

#print(tabla)
writeData(wb, sheet = "t_53_1", x = "Time evolution of the numebr of students by initial languages", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_53_1", x = "Disaggregated by territorial areas", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_53_1", x = tabla,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_53_1" , startRow=55,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of the numebr of students by initial languages, disaggregated by territorial areas" , startRow=55,startCol = 2)


addWorksheet(wb, sheet = "t_54_1")

tabla <- dades %>% 
  group_by(academic_course,terr,llengua_inicial2) %>%
  summarize(
    n_students = n_distinct(codialumne)
  )%>%
  group_by(academic_course, terr) %>%
  mutate(
    pct_curs = round((n_students / sum(n_students)) * 100,2)
  )%>%
  group_by(academic_course) %>%
  mutate(
    pct_terr = round((n_students / sum(n_students)) * 100,2)
  )

#print(tabla)
writeData(wb, sheet = "t_54_1", x = "Time evolution of the numebr of students by initial second languages", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_54_1", x = "Disaggregated by territorial area", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_54_1", x = tabla,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_54_1" , startRow=56,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Time evolution of the numebr of students by initial second languages, disaggregated by territorial areas" , startRow=56,startCol = 2)


saveWorkbook(wb,file="Descriptive_B1.xlsx",overwrite=TRUE)



# BLOQUE 2 ----------------------------------------------------------------
wb <- createWorkbook()
addWorksheet(wb, "INDEX")
writeData(wb, sheet = "INDEX", x = "Name of the table" , startRow=1,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Description" , startRow=1,startCol = 2)


addWorksheet(wb, sheet = "t_1_2")

tabla_genero <- dades %>% 
  group_by(inscr_cat, gender) %>%
  summarize(
    n_enrollments = n()
  ) %>%
  group_by(inscr_cat) %>%
  mutate(
    pct = n_enrollments / sum(n_enrollments) * 100
  )
tabla_genero$gender[tabla_genero$gender==""] <- NA
#print(tabla_genero)

writeData(wb, sheet = "t_1_2", x = "Gender of students by the number of inscriptions", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_1_2", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_1_2", x = tabla_genero,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_1_2" , startRow=2,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Gender of students by the number of inscriptions" , startRow=2,startCol = 2)

addWorksheet(wb, sheet = "t_2_2")

tabla_edat <- dades %>% 
  group_by(inscr_cat, edat_media_cat) %>%
  summarize(
    n_enrollments = n()
  ) %>%
  group_by(inscr_cat) %>%
  mutate(
    pct = round(n_enrollments / sum(n_enrollments) * 100,3)
  )
#print(tabla_edat)
writeData(wb, sheet = "t_2_2", x = "Mean age of the students by the number of inscriptions", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_2_2", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_2_2", x = tabla_edat,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_2_2" , startRow=3,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Mean age of the students by the number of inscriptions" , startRow=3,startCol = 2)

addWorksheet(wb, sheet = "t_3_2")
tabla_agrpais <- dades %>% 
  group_by(inscr_cat, country_grouped) %>%
  summarize(
    n_enrollments = n()
  ) %>%
  group_by(inscr_cat) %>%
  mutate(
    pct = round(n_enrollments / sum(n_enrollments) * 100,3)
  )
#print(tabla_agrpais)
writeData(wb, sheet = "t_3_2", x = "Country of origin of the students by the number of inscriptions", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_3_2", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_3_2", x = tabla_agrpais,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_3_2" , startRow=4,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Country of origin of the students by the number of inscriptions" , startRow=4,startCol = 2)

addWorksheet(wb, sheet = "t_4_2")
tabla_terr <- dades %>% 
  group_by(inscr_cat, terr) %>%
  summarize(
    n_enrollments = n()
  ) %>%
  group_by(inscr_cat) %>%
  mutate(
    pct = round(n_enrollments / sum(n_enrollments) * 100,3)
  )
writeData(wb, sheet = "t_4_2", x = "Territorial areas by the number of inscriptions", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_4_2", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_4_2", x = tabla_terr,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_4_2" , startRow=5,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Territorial areas by the number of inscriptions" , startRow=5,startCol = 2)

addWorksheet(wb, sheet = "t_5_2")
tabla_nivell_descragrupniv <- dades %>% 
  group_by(inscr_cat, course_level) %>%
  summarize(
    n_enrollments = n()
  ) %>%
  group_by(inscr_cat) %>%
  mutate(
    pct = round(n_enrollments / sum(n_enrollments) * 100,3)
  )
#print(tabla_nivell_descragrupniv)
writeData(wb, sheet = "t_5_2", x = "Course levels by the number of inscriptions", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_5_2", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_5_2", x = tabla_nivell_descragrupniv,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_5_2" , startRow=6,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Course levels by the number of inscriptions" , startRow=6,startCol = 2)

addWorksheet(wb, sheet = "t_6_2")
tabla_academic_course <- dades %>% 
  group_by(inscr_cat, academic_course) %>%
  summarize(
    n_enrollments = n()
  ) %>%
  group_by(inscr_cat) %>%
  mutate(
    pct = round(n_enrollments / sum(n_enrollments) * 100,3)
  )
#print(tabla_academic_course)
writeData(wb, sheet = "t_6_2", x = "Academic course of the enrollments by the number of inscriptions", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_6_2", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_6_2", x = tabla_academic_course,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_6_2" , startRow=7,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Academic course of the enrollments by the number of inscriptions" , startRow=7,startCol = 2)

#-------------------------------------------------------------------------------
# 3)	Descripci? i perfil dels estudiants segons els nivells assolit: agregat i 
#   en funci? del nivell de partida
#-------------------------------------------------------------------------------

addWorksheet(wb, sheet = "t_7_2")
tabla <- dades %>% 
  group_by(inscr_cat,nivel_maximo) %>%
  summarize(n_students=n_distinct(codialumne)) %>%  
  group_by(inscr_cat) %>%
  mutate(prop_inscr= round(n_students/sum(n_students)*100,2) #proporciones dentro de cada inscr_cat
  )
#print(tabla)
writeData(wb, sheet = "t_7_2", x = "Maximum course level achieved by the number of inscriptions", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_7_2", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_7_2", x = tabla,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_7_2" , startRow=8,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Maximum course level achieved by the number of inscriptions" , startRow=8,startCol = 2)


#------------------------------------------------------------------------------
addWorksheet(wb, sheet = "t_8_2")
tabla_genere_nivmax <- dades %>% 
  group_by(inscr_cat,gender,nivel_maximo) %>%
  summarize(n_students=n_distinct(codialumne)
  )%>%  
  group_by(inscr_cat) %>%
  mutate(prop_inscr= round(n_students/sum(n_students)*100,3) #proporciones dentro de cada inscr_cat
  )
#print(tabla_genere_nivmax)
tabla_genere_nivmax[tabla_genere_nivmax$gender=="","gender"] <- NA
writeData(wb, sheet = "t_8_2", x = "Maximum course level achieved by the number of inscriptions and gender", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_8_2", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_8_2", x = tabla_genere_nivmax,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_8_2" , startRow=9,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Maximum course level achieved by the number of inscriptions and gender" , startRow=9,startCol = 2)

#EDAT_MEDIA_CAT (porque si usas edat_cat puede que no sea el mismo segun la matricula del alumno)
addWorksheet(wb, sheet = "t_9_2")
tabla_edat_insc_cat_nivell <- dades %>% 
  group_by(inscr_cat,edat_media_cat,nivel_maximo) %>%
  summarize(n_students=n_distinct(codialumne))%>% 
  group_by(inscr_cat) %>%
  mutate(prop_edat_cat= round(n_students/sum(n_students)*100,2))
writeData(wb, sheet = "t_9_2", x = "Maximum course level achieved by the number of inscriptions and mean age", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_9_2", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_9_2", x = tabla_edat_insc_cat_nivell,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_9_2" , startRow=10,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Maximum course level achieved by the number of inscriptions and mean age" , startRow=10,startCol = 2)

#PAIS DE ORIGEN: CON country_grouped!!
#------------------------------------------------------------------------------
addWorksheet(wb, sheet = "t_10_2")
tabla_agrpais_insc_cat_nivell <- dades %>% 
  group_by(inscr_cat,country_grouped,nivel_maximo) %>%
  summarize(n_students=n_distinct(codialumne))%>% 
  group_by(inscr_cat) %>%
  mutate(prop_agrpais= round(n_students/sum(n_students)*100,2)
  )
#tabla_agrpais_insc_cat_nivell[tabla_agrpais_insc_cat_nivell$country_grouped=="","country_grouped"] <- NA
#print(tabla_agrpais_insc_cat_nivell)
# sum(tabla_agrpais_insc_cat_nivell$n_students)
writeData(wb, sheet = "t_10_2", x = "Maximum course level achieved by the numebr of inscription and country origins", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_10_2", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_10_2", x = tabla_agrpais_insc_cat_nivell,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_10_2" , startRow=11,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Maximum course level achieved by the numebr of inscription and country origins" , startRow=11,startCol = 2)

# SEGUN NIVEL PARTIDA 

tabla_genere_insc_cat_nivell_partida <- dades %>% 
  group_by(inscr_cat,gender,nivel_maximo,nivel_partida) %>%
  summarize(count=n_distinct(codialumne))%>%  
  group_by(inscr_cat) %>%
  mutate(prop_genere= round(count/sum(count)*100,3) #proporciones dentro de cada inscr_cat
  )%>%
  select(inscr_cat,nivel_partida,nivel_maximo,gender,count,prop_genere)

addWorksheet(wb, sheet = "t_11_2")
tabla_genere_nivmax_partida <- dades %>% 
  group_by(inscr_cat,gender,nivel_maximo,nivel_partida) %>%
  summarize(n_students=n_distinct(codialumne)
  )%>%  
  group_by(inscr_cat) %>%
  mutate(prop_inscr= round(n_students/sum(n_students)*100,3) #proporciones dentro de cada inscr_cat
  )
tabla_genere_nivmax_partida[tabla_genere_nivmax_partida$gender=="","gender"] <- NA
writeData(wb, sheet = "t_11_2", x = "Maximum course level achieved by the number of inscriptions and gender", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_11_2", x = "Disaggregated by initial course level", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_11_2", x = tabla_genere_nivmax_partida,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_11_2" , startRow=12,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Maximum course level achieved by the number of inscriptions and gender, dissagregated by the initial course level " , startRow=12,startCol = 2)

#EDAT_MEDIA_CAT (porque si usas edat_cat puede que no sea el mismo segun la matricula del alumno)
addWorksheet(wb, sheet = "t_12_2")
tabla_edat_insc_cat_nivell_partida <- dades %>% 
  group_by(inscr_cat,edat_media_cat,nivel_maximo,nivel_partida) %>%
  summarize(n_students=n_distinct(codialumne))%>% 
  group_by(inscr_cat) %>%
  mutate(prop_edat_cat= round(n_students/sum(n_students)*100,2))

writeData(wb, sheet = "t_12_2", x = "Maximum course level achieved by the number of inscriptions and mean age", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_12_2", x = "Disaggregated by initial course level", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_12_2", x = tabla_edat_insc_cat_nivell_partida,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_12_2" , startRow=13,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Maximum course level achieved by the number of inscriptions and mean age, dissagregated by initial course level" , startRow=13,startCol = 2)

addWorksheet(wb, sheet = "t_13_2")
tabla_agrpais_insc_cat_nivell_partida <- dades %>% 
  group_by(inscr_cat,country_grouped,nivel_maximo,nivel_partida) %>%
  summarize(n_students=n_distinct(codialumne))%>% 
  group_by(inscr_cat) %>%
  mutate(prop_agrpais= round(n_students/sum(n_students)*100,2)
  )
writeData(wb, sheet = "t_13_2", x = "Maximum course level achieved by the number of inscriptions and country of origin", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_13_2", x = "Disaggregated by initial course level", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_13_2", x = tabla_agrpais_insc_cat_nivell_partida,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_13_2" , startRow=14,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Maximum course level achieved by the number of inscriptions and country of origin, dissagregated by initial course level" , startRow=14,startCol = 2)


#-------------------------------------------------------------------------------
# 4)	Descripci? i perfil dels casos d'abandonament, diferenciant entre qui s'inscriu 
#     per? no comen?a i qui abandona durant el curs: agregat i segons nivells
#-------------------------------------------------------------------------------

#AGREGADOS:
addWorksheet(wb, sheet = "t_14_2")
casos_alumnos <- dades%>%
  group_by(caso_ab) %>%
  summarize(
    count = n_distinct(codialumne)
  )
#print(casos_alumnos)

writeData(wb, sheet = "t_14_2", x = "Number of dropout-type cases", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_14_2", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_14_2", x = casos_alumnos,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_14_2" , startRow=15,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Number of dropout-type cases" , startRow=15,startCol = 2)

# SEGUN NIVEL
#addWorksheet(wb, sheet = "t_15_2")
casos_alumnos_nivell_descragrupniv <- dades%>%
  group_by(caso_ab,course_level) %>%
  summarize(
    count = n_distinct(codialumne)
  )

#print(casos_alumnos_nivell_descragrupniv)
#writeData(wb, sheet = "t_15_2", x = "Nombre de casos de abandonament segons nivells", startRow = 1, startCol = 1)
#writeData(wb, sheet = "t_15_2", x = "", startRow = 2, startCol = 1)
#writeData(wb, sheet = "t_15_2", x = casos_alumnos_nivell_descragrupniv,startRow=3)
# writeData(wb, sheet = "INDEX", x = "t_15_2" , startRow=16,startCol = 1) LA SUMA TOTAL NO DA BIEN, NO ES CORRECTA
#writeData(wb, sheet = "INDEX", x = "Nombre de casos  de abandonament agregados segons nivells" , startRow=16,startCol = 2)

# SEGUN NIVEL MAXIMO
addWorksheet(wb, sheet = "t_16_2")
casos_alumnos_nivell_max <- dades%>%
  group_by(caso_ab,nivel_maximo) %>%
  summarize(
    count = n_distinct(codialumne)
  )
#print(casos_alumnos_nivell_max)
writeData(wb, sheet = "t_16_2", x = "Number of dropout-type cases by the maximum course level achieved", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_16_2", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_16_2", x = casos_alumnos_nivell_max,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_16_2" , startRow=17,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Number of dropout-type cases by the maximum course level achieved" , startRow=17,startCol = 2)

#SEGUN NIVEL DE PARTIDA
addWorksheet(wb, sheet = "t_17_2")
casos_alumnos_niv_partida <- dades%>%
  group_by(caso_ab,nivel_partida) %>%
  summarize(
    count = n_distinct(codialumne)
  )
#print(casos_alumnos_niv_partida)
writeData(wb, sheet = "t_17_2", x = "Number of dropout-type cases by the initial course level", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_17_2", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_17_2", x = casos_alumnos_niv_partida,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_17_2" , startRow=18,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Number of dropout-type cases by the initial course level" , startRow=18,startCol = 2)

#SEGUN NIVEL FINAL 
addWorksheet(wb, sheet = "t_18_2")
casos_alumnos_niv_final <- dades%>%
  group_by(caso_ab,nivel_final) %>%
  summarize(
    count = n_distinct(codialumne)
  )
#print(casos_alumnos_niv_final)
writeData(wb, sheet = "t_18_2", x = "Number of dropout-type cases by the final course level", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_18_2", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_18_2", x = casos_alumnos_niv_final,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_18_2" , startRow=19,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Number of dropout-type cases by the final course level" , startRow=19,startCol = 2)


#-------------------------------------------------------------------------------
# 5)	Discontinuitat entre inscripcions: analitzar les persones que deixen de seguir 
#    els itineraris (considerant qui ho deixa despr?s d'abandonar o acabar un curs) 
#   i els patrons de retorn al CPNL: agregat i segons nivells inicials i finals
#-------------------------------------------------------------------------------

#NUMERO DE DISCONTINUIDADES DE CADA ALUMNO 
addWorksheet(wb, sheet = "t_19_2")

# Clasificar a los alumnos seg?n el n?mero de discontinuidades
clasificacion_alumnos <- dades %>%
  group_by(discontinuidades_cat_12m) %>%
  summarize(
    n_students = n()
  )
#print(clasificacion_alumnos)
writeData(wb, sheet = "t_19_2", x = "Number of discontinuities by students", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_19_2", x = "In 12 month time-lapse", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_19_2", x = clasificacion_alumnos,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_19_2" , startRow=20,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Number of discontinuities by students in 12 month time-lapse" , startRow=20,startCol = 2)

# NUMERO DE CURSOS REALIZADOS HASTA LA PRIMERA DISCONTINUIDAD
#addWorksheet(wb, sheet = "t_20_2")
cursos_hasta_primera_discontinuidad <- dades %>%
  group_by(codialumne) %>%
  mutate(
    primera_discontinuidad = ifelse(continuidad_12m == "Discontinuidad" & lead(continuidad_12m) != "Discontinuidad", row_number(), NA)
  ) %>%
  group_by(codialumne) %>%
  summarize(
    num_cursos_hasta_primera_discontinuidad = sum(!is.na(primera_discontinuidad))+1
  )
clasificacion_num_cursos <- cursos_hasta_primera_discontinuidad %>%
  group_by(num_cursos_hasta_primera_discontinuidad) %>%
  summarize(
    n_alumnos = n()
  )
#print(clasificacion_num_cursos)
#writeData(wb, sheet = "t_20_2", x = "Nombre de cursos fins la primera discontinuitat del alumne", startRow = 1, startCol = 1)
#writeData(wb, sheet = "t_20_2", x = "", startRow = 2, startCol = 1)
#writeData(wb, sheet = "t_20_2", x = clasificacion_num_cursos,startRow=3)
# writeData(wb, sheet = "INDEX", x = "t_20_2" , startRow=21,startCol = 1) NO SE INCLUYE AL FINAL
#writeData(wb, sheet = "INDEX", x = "Nombre de cursos fins la primera discontinuitat del alumne" , startRow=21,startCol = 2)

# PERFIL DE ALUMNOS 

#Numero de alumnos segun sus discontinuidades por curso academico
# AGREGATS
#addWorksheet(wb, sheet = "t_21_2")
tabla <- dades %>% 
  group_by(academic_course,discontinuidades_cat_12m) %>%
  summarize(n_students=n_distinct(codialumne))
#print(tabla)
#writeData(wb, sheet = "t_21_2", x = "Nombre de discontinuitats per alumne", startRow = 1, startCol = 1)
#writeData(wb, sheet = "t_21_2", x = "Agregat", startRow = 2, startCol = 1)
#writeData(wb, sheet = "t_21_2", x = tabla,startRow=3)
# writeData(wb, sheet = "INDEX", x = "t_21_2" , startRow=22,startCol = 1) O SE INCLUYE 
#writeData(wb, sheet = "INDEX", x = "Nombre de discontinuitats per alumne" , startRow=22,startCol = 2)

#genero de alumnos 
addWorksheet(wb, sheet = "t_22_2")
tabla1 <- dades %>% 
  group_by(discontinuidades_cat_12m,gender) %>%
  summarize(n_students=n_distinct(codialumne)) %>%
  group_by( discontinuidades_cat_12m) %>%
  mutate(pct_disc = round(n_students / sum(n_students) * 100,2))%>%
  group_by( gender) %>%
  mutate(pct = round(n_students / sum(n_students) * 100,2))
tabla1$gender[tabla1$gender==""] <- NA
#print(tabla1)
writeData(wb, sheet = "t_22_2", x = "Number of discontinuities by students and gender", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_22_2", x = "In 12 month time-lapse", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_22_2", x = tabla1,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_22_2" , startRow=23,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Number of discontinuities by students and gender" , startRow=23,startCol = 2)

#edat media de alumnos 
addWorksheet(wb, sheet = "t_23_2")
tabla2 <- dades %>% 
  group_by(discontinuidades_cat_12m,edat_media_cat) %>%
  summarize(n_students=n_distinct(codialumne))%>%
  group_by( discontinuidades_cat_12m) %>%
  mutate(pct_disc = round(n_students / sum(n_students) * 100,2))%>%
  group_by( edat_media_cat) %>%
  mutate(pct_edat= round(n_students / sum(n_students) * 100,2))
#print(tabla2)
writeData(wb, sheet = "t_23_2", x = "Number of discontinuities of students by mean age", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_23_2", x = "In 12 month time-lapse", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_23_2", x = tabla2,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_23_2" , startRow=24,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Number of discontinuities of students by mean age" , startRow=24,startCol = 2)


#pais de origen de alumnos 
addWorksheet(wb, sheet = "t_24_2")
tabla3 <- dades %>% 
  group_by(discontinuidades_cat_12m,country_grouped) %>%
  summarize(n_students=n_distinct(codialumne))%>%
  group_by( discontinuidades_cat_12m) %>%
  mutate(pct_disc = round(n_students / sum(n_students) * 100,2))%>%
  group_by( country_grouped) %>%
  mutate(pct_agrpais= round(n_students / sum(n_students) * 100,2))
#print(tabla3)
writeData(wb, sheet = "t_24_2", x = "Number of discontinuities by students and country of origin", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_24_2", x = "In 12 month time-lapse", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_24_2", x = tabla3,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_24_2" , startRow=25,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Number of discontinuities by students and country of origin" , startRow=25,startCol = 2)

# SEGONS NIVEL DE PARTIDA
addWorksheet(wb, sheet = "t_25_2")
tabla <- dades %>% 
  group_by(discontinuidades_cat_12m,nivel_partida) %>%
  summarize(n_students=n_distinct(codialumne))%>%
  group_by( discontinuidades_cat_12m) %>%
  mutate(pct_disc = round(n_students / sum(n_students) * 100,2))%>%
  group_by( nivel_partida) %>%
  mutate(pct_niv_part = round(n_students / sum(n_students) * 100,2))
#print(tabla)
writeData(wb, sheet = "t_25_2", x = "Number of discontinuities of students by initial course level", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_25_2", x = "In 12 month time-lapse", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_25_2", x = tabla,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_25_2" , startRow=26,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Number of discontinuities of students by initial course level" , startRow=26,startCol = 2)

#genero de alumnos 
addWorksheet(wb, sheet = "t_26_2")

tabla1 <- dades %>% 
  group_by( discontinuidades_cat_12m,nivel_partida,gender) %>%
  summarize(n_students=n_distinct(codialumne))%>%
  group_by( discontinuidades_cat_12m) %>%
  mutate(pct_disc = round(n_students / sum(n_students) * 100,2))%>%
  group_by( nivel_partida) %>%
  mutate(pct_niv_part = round(n_students / sum(n_students) * 100,2))%>%
  group_by( gender) %>%
  mutate(pct_genere = round(n_students / sum(n_students) * 100,2))
tabla1$gender[tabla1$gender==""] <- NA
#print(tabla1)
writeData(wb, sheet = "t_26_2", x = "Number of discontinuities of students by gender and initial course level", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_26_2", x = "In 12 month time-lapse", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_26_2", x = tabla1,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_26_2" , startRow=27,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Number of discontinuities of students by gender and initial course level" , startRow=27, startCol = 2)

#edat media de alumnos 
addWorksheet(wb, sheet = "t_27_2")
tabla2 <- dades %>% 
  group_by(discontinuidades_cat_12m,nivel_partida,edat_media_cat) %>%
  summarize(n_students=n_distinct(codialumne))%>%
  group_by( discontinuidades_cat_12m) %>%
  mutate(pct_disc = round(n_students / sum(n_students) * 100,2))%>%
  group_by( nivel_partida) %>%
  mutate(pct_niv_part = round(n_students / sum(n_students) * 100,2))%>%
  group_by( edat_media_cat) %>%
  mutate(pct_edat = round(n_students / sum(n_students) * 100,2))
#print(tabla2)
writeData(wb, sheet = "t_27_2", x = "Number of discontinuities of students by mean age and initial course level", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_27_2", x = "In 12 month time-lapse", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_27_2", x = tabla2,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_27_2" , startRow=28,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Number of discontinuities of students by mean age and initial course level" , startRow=28, startCol = 2)

#pais de origen de alumnos 
addWorksheet(wb, sheet = "t_28_2")
tabla3 <- dades %>% 
  group_by(discontinuidades_cat_12m,nivel_partida,country_grouped) %>%
  summarize(n_students=n_distinct(codialumne))%>%
  group_by( discontinuidades_cat_12m) %>%
  mutate(pct_disc = round(n_students / sum(n_students) * 100,2))%>%
  group_by( nivel_partida) %>%
  mutate(pct_niv_part = round(n_students / sum(n_students) * 100,2))%>%
  group_by( country_grouped) %>%
  mutate(pct_agrpais= round(n_students / sum(n_students) * 100,2))
#print(tabla3)
writeData(wb, sheet = "t_28_2", x = "Number of discontinuities of students by country of origin and initial course level", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_28_2", x = "In 12 month time-lapse", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_28_2", x = tabla3,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_28_2" , startRow=29,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Number of discontinuities of students by country of origin and initial course level" , startRow=29, startCol = 2)


# SEGONS NIVEL FINAL 
#addWorksheet(wb, sheet = "t_29_2")
tabla <- dades %>% 
  group_by(academic_course,discontinuidades_cat_12m,nivel_final) %>%
  summarize(n_students=n_distinct(codialumne))%>%
  group_by(academic_course, discontinuidades_cat_12m) %>%
  mutate(pct_disc = round(n_students / sum(n_students) * 100,2))%>%
  group_by(academic_course, nivel_final) %>%
  mutate(pct_niv_part = round(n_students / sum(n_students) * 100,2))
#print(tabla)
#writeData(wb, sheet = "t_29_2", x = "Nombre de discontinuitats per alumne", startRow = 1, startCol = 1)
#writeData(wb, sheet = "t_29_2", x = "Segons nivell final", startRow = 2, startCol = 1)
#writeData(wb, sheet = "t_29_2", x = tabla,startRow=3)
# writeData(wb, sheet = "INDEX", x = "t_29_2" , startRow=30,startCol = 1) ELIMINADA
#writeData(wb, sheet = "INDEX", x = "Nombre de discontinuitats. Segons nivell final" , startRow=30, startCol = 2)

#genero de alumnos 
#addWorksheet(wb, sheet = "t_30_2")

tabla1 <- dades %>% 
  group_by(academic_course, discontinuidades_cat_12m,nivel_final,gender) %>%
  summarize(n_students=n_distinct(codialumne))%>%
  group_by(academic_course, discontinuidades_cat_12m) %>%
  mutate(pct_disc = round(n_students / sum(n_students) * 100,2))%>%
  group_by(academic_course, nivel_final) %>%
  mutate(pct_niv_part = round(n_students / sum(n_students) * 100,2))%>%
  group_by(academic_course, gende3r) %>%
  mutate(pct_genere = round(n_students / sum(n_students) * 100,2))

tabla1$gender[tabla1$gender==""] <- NA
#print(tabla1)
#writeData(wb, sheet = "t_30_2", x = "Nombre de discontinuitats per alumne segons el genere", startRow = 1, startCol = 1)
#writeData(wb, sheet = "t_30_2", x = "Segons nivell final", startRow = 2, startCol = 1)
#writeData(wb, sheet = "t_30_2", x = tabla1,startRow=3)
# writeData(wb, sheet = "INDEX", x = "t_30_2" , startRow=31,startCol = 1)
#writeData(wb, sheet = "INDEX", x = "Nombre de discontinuitats per alumne segons el genere. Segons nivell final" , startRow=31, startCol = 2)

#edat media de alumnos 
#addWorksheet(wb, sheet = "t_31_2")
tabla2 <- dades %>% 
  group_by(academic_course,discontinuidades_cat_12m,nivel_final,edat_media_cat) %>%
  summarize(n_students=n_distinct(codialumne))%>%
  group_by(academic_course, discontinuidades_cat_12m) %>%
  mutate(pct_disc = round(n_students / sum(n_students) * 100,2))%>%
  group_by(academic_course, nivel_final) %>%
  mutate(pct_niv_part = round(n_students / sum(n_students) * 100,2))%>%
  group_by(academic_course, edat_media_cat) %>%
  mutate(pct_edat = round(n_students / sum(n_students) * 100,2))

#writeData(wb, sheet = "t_31_2", x = "Nombre de discontinuitats per alumne segons la edat media del alumne", startRow = 1, startCol = 1)
#writeData(wb, sheet = "t_31_2", x = "Segons nivell final", startRow = 2, startCol = 1)
#writeData(wb, sheet = "t_31_2", x = tabla2,startRow=3)
#writeData(wb, sheet = "INDEX", x = "t_31_2" , startRow=32,startCol = 1)
#writeData(wb, sheet = "INDEX", x = "Nombre de discontinuitats per alumne segons la edat media del alumne. Segons nivell final" , startRow=32, startCol = 2)

#pais de origen de alumnos 
#addWorksheet(wb, sheet = "t_32_2")
tabla3 <- dades %>% 
  group_by(academic_course,discontinuidades_cat_12m,nivel_final,country_grouped) %>%
  summarize(n_students=n_distinct(codialumne))%>%
  group_by(academic_course, discontinuidades_cat_12m) %>%
  mutate(pct_disc = round(n_students / sum(n_students) * 100,2))%>%
  group_by(academic_course, nivel_final) %>%
  mutate(pct_niv_part = round(n_students / sum(n_students) * 100,2))%>%
  group_by(academic_course, country_grouped) %>%
  mutate(pct_agrpais= round(n_students / sum(n_students) * 100,2))
#print(tabla3)
#writeData(wb, sheet = "t_32_2", x = "Nombre de discontinuitats per alumne segons pais de origen del alumne", startRow = 1, startCol = 1)
#writeData(wb, sheet = "t_32_2", x = "Segons nivell final", startRow = 2, startCol = 1)
#writeData(wb, sheet = "t_32_2", x = tabla3,startRow=3)
# writeData(wb, sheet = "INDEX", x = "t_32_2" , startRow=33,startCol = 1)
#writeData(wb, sheet = "INDEX", x = "Nombre de discontinuitats per alumne segons pais de origen del alumne. Segons nivell final" , startRow=33, startCol = 2)

# NIVEL MAXIM ASSOLIT 
addWorksheet(wb, sheet = "t_33_2")
tabla <- dades %>% 
  group_by(discontinuidades_cat_12m,nivel_maximo) %>%
  summarize(n_students=n_distinct(codialumne))%>%
  group_by( discontinuidades_cat_12m) %>%
  mutate(pct_disc = round(n_students / sum(n_students) * 100,2))%>%
  group_by( nivel_maximo) %>%
  mutate(pct_niv_part = round(n_students / sum(n_students) * 100,2))
#print(tabla)
writeData(wb, sheet = "t_33_2", x = "Number of discontinuities of students by the maximum course level achieved", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_33_2", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_33_2", x = tabla,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_33_2" , startRow=34,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Number of discontinuities of students by the maximum course level achieved" , startRow=34, startCol = 2)

#genero de alumnos 
addWorksheet(wb, sheet = "t_34_2")

tabla1 <- dades %>% 
  group_by( discontinuidades_cat_12m,nivel_maximo,gender) %>%
  summarize(n_students=n_distinct(codialumne))%>%
  group_by( discontinuidades_cat_12m) %>%
  mutate(pct_disc = round(n_students / sum(n_students) * 100,2))%>%
  group_by(nivel_maximo) %>%
  mutate(pct_niv_part = round(n_students / sum(n_students) * 100,2))%>%
  group_by( gender) %>%
  mutate(pct_gender = round(n_students / sum(n_students) * 100,2))
tabla1$gender[tabla1$gender==""] <- NA
#print(tabla1)
writeData(wb, sheet = "t_34_2", x = "Number of discontinuities of students by gender and maximum course level achieved", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_34_2", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_34_2", x = tabla1,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_34_2" , startRow=35,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Number of discontinuities of students by gender and maximum course level achieved" , startRow=35, startCol = 2)

#edat media de alumnos 
addWorksheet(wb, sheet = "t_35_2")
tabla2 <- dades %>% 
  group_by(discontinuidades_cat_12m,nivel_maximo,edat_media_cat) %>%
  summarize(n_students=n_distinct(codialumne))%>%
  group_by( discontinuidades_cat_12m) %>%
  mutate(pct_disc = round(n_students / sum(n_students) * 100,2))%>%
  group_by( nivel_maximo) %>%
  mutate(pct_niv_part = round(n_students / sum(n_students) * 100,2))%>%
  group_by( edat_media_cat) %>%
  mutate(pct_edat = round(n_students / sum(n_students) * 100,2))
#print(tabla2)
tabla2$edat_media_cat[tabla2$edat_media_cat==" "]<-NA

writeData(wb, sheet = "t_35_2", x = "Number of discontinuities of students by mean age and maximum course level achieved", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_35_2", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_35_2", x = tabla2,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_35_2" , startRow=36,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Number of discontinuities of students by mean age and maximum course level achieved" , startRow=36, startCol = 2)

#pais de origen de alumnos 
addWorksheet(wb, sheet = "t_36_2")
tabla3 <- dades %>% 
  group_by(discontinuidades_cat_12m,nivel_maximo,country_grouped) %>%
  summarize(n_students=n_distinct(codialumne))%>%
  group_by( discontinuidades_cat_12m) %>%
  mutate(pct_disc = round(n_students / sum(n_students) * 100,2))%>%
  group_by( nivel_maximo) %>%
  mutate(pct_niv_part = round(n_students / sum(n_students) * 100,2))%>%
  group_by( country_grouped) %>%
  mutate(pct_agrpais= round(n_students / sum(n_students) * 100,2))
#print(tabla3)
tabla3$country_grouped[tabla3$country_grouped==" "]<-NA

writeData(wb, sheet = "t_36_2", x = "Number of discontinuities of students by country origin and maximum course level achieved", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_36_2", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_36_2", x = tabla3,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_36_2" , startRow=37,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Number of discontinuities of students by country origin and maximum course level achieved" , startRow=37, startCol = 2)

# PERFIL DE ALUNOS QUE SE REENGANCHAN (1 O MAS VECES)

# porcentaje de reenganche por cursos academicos, para ver cuantos vuelven cada ano y si cambia mucho 

addWorksheet(wb, sheet = "t_37_2")
prop_vuelven <- dades %>%
  group_by(codialumne) %>%
  summarize(
    primer_registro = first(continuidad_12m),
    vuelven = ifelse(primer_registro == "Continuidad", 1, 0)
  ) %>%
  summarize(
    prop_vuelven = mean(vuelven),
    prop_no_vuelven = 1 - prop_vuelven
  )
#print(prop_vuelven)
writeData(wb, sheet = "t_37_2", x = "Return rate in a year", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_37_2", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_37_2", x = prop_vuelven,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_37_2" , startRow=38,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Return rate in a year" , startRow=38, startCol = 2)

addWorksheet(wb, sheet = "t_38_2")
prop_vuelven_curs <- dades %>%
  group_by(academic_course,codialumne) %>%
  summarize(
    primer_registro = first(continuidad_12m),
    vuelven = ifelse(primer_registro == "Continuidad", 1, 0)
  ) %>%
  summarize(
    prop_vuelven = mean(vuelven),
    prop_no_vuelven = 1 - prop_vuelven
  )
#print(prop_vuelven_curs)
writeData(wb, sheet = "t_38_2", x = "Return rate in a year by academic course", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_38_2", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_38_2", x = prop_vuelven_curs,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_38_2" , startRow=39,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Return rate in a year by academic course" , startRow=39, startCol = 2)

addWorksheet(wb, sheet = "t_39_2")

prop_vuelven_genere <- dades %>%
  group_by(gender,codialumne) %>%
  summarize(
    primer_registro = first(continuidad_12m),
    vuelven = ifelse(primer_registro == "Continuidad", 1, 0)
  ) %>%
  summarize(
    prop_vuelven = mean(vuelven),
    prop_no_vuelven = 1 - prop_vuelven
  )
#print(prop_vuelven_genere)
prop_vuelven_genere$gender[prop_vuelven_genere$gender==" "]<-NA

writeData(wb, sheet = "t_39_2", x = "Return rate in a year by  gender", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_39_2", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_39_2", x = prop_vuelven_genere,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_39_2" , startRow=40,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Return rate in a year by  gender" , startRow=40, startCol = 2)


addWorksheet(wb, sheet = "t_40_2")
prop_vuelven_curs_genere <- dades %>%
  group_by(academic_course,gender,codialumne) %>%
  summarize(
    primer_registro = first(continuidad_12m),
    vuelven = ifelse(primer_registro == "Continuidad", 1, 0)
  ) %>%
  summarize(
    prop_vuelven = mean(vuelven),
    prop_no_vuelven = 1 - prop_vuelven
  )
prop_vuelven_curs_genere$gender[prop_vuelven_curs_genere$gender==" "]<-NA
#print(prop_vuelven_curs_genere)

writeData(wb, sheet = "t_40_2", x = "Return rate in a year by academic course and gender", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_40_2", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_40_2", x = prop_vuelven_curs_genere,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_40_2" , startRow=41,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Return rate in a year by academic course and gender" , startRow=41, startCol = 2)


addWorksheet(wb, sheet = "t_41_2")

prop_vuelven_edat<- dades %>%
  group_by(edat_media_cat,codialumne) %>%
  summarize(
    primer_registro = first(continuidad_12m),
    vuelven = ifelse(primer_registro == "Continuidad", 1, 0)
  ) %>%
  summarize(
    prop_vuelven = mean(vuelven),
    prop_no_vuelven = 1 - prop_vuelven
  )
#print(prop_vuelven_edat)
prop_vuelven_edat[prop_vuelven_edat$edat_media_cat==" ","edat_media_cat"] <- NA

writeData(wb, sheet = "t_41_2", x = "Return rate in a year by mean age and gender", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_41_2", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_41_2", x = prop_vuelven_edat,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_41_2" , startRow=42,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Return rate in a year by mean age and gender" , startRow=42, startCol = 2)


addWorksheet(wb, sheet = "t_42_2")
prop_vuelven_curs_edat <- dades %>%
  group_by(academic_course,edat_media_cat,codialumne) %>%
  summarize(
    primer_registro = first(continuidad_12m),
    vuelven = ifelse(primer_registro == "Continuidad", 1, 0)
  ) %>%
  summarize(
    prop_vuelven = mean(vuelven),
    prop_no_vuelven = 1 - prop_vuelven
  )
#print(prop_vuelven_curs_edat)
prop_vuelven_curs_edat[prop_vuelven_curs_edat$edat_media_cat==" ","edat_media_cat"] <- NA

writeData(wb, sheet = "t_42_2", x = "Return rate in a year by academic course and mean age", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_42_2", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_42_2", x = prop_vuelven_curs_edat,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_42_2" , startRow=43,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Return rate in a year by academic course and mean age" , startRow=43, startCol = 2)


addWorksheet(wb, sheet = "t_43_2")
prop_vuelven_agrpais <- dades %>%
  group_by(country_grouped,codialumne) %>%
  summarize(
    primer_registro = first(continuidad_12m),
    vuelven = ifelse(primer_registro == "Continuidad", 1, 0)
  ) %>%
  summarize(
    prop_vuelven = mean(vuelven),
    prop_no_vuelven = 1 - prop_vuelven
  )
#print(prop_vuelven_agrpais)

prop_vuelven_agrpais[prop_vuelven_agrpais$country_grouped==" ","country_grouped"] <- NA
writeData(wb, sheet = "t_43_2", x = "Return rate in a year by country origins", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_43_2", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_43_2", x = prop_vuelven_agrpais,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_43_2" , startRow=44,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Return rate in a year by country origins" , startRow=44, startCol = 2)


addWorksheet(wb, sheet = "t_44_2")
prop_vuelven_curs_agrpais <- dades %>%
  group_by(academic_course,country_grouped,codialumne) %>%
  summarize(
    primer_registro = first(continuidad_12m),
    vuelven = ifelse(primer_registro == "Continuidad", 1, 0)
  ) %>%
  summarize(
    prop_vuelven = mean(vuelven),
    prop_no_vuelven = 1 - prop_vuelven
  )
#print(prop_vuelven_curs_agrpais)
prop_vuelven_curs_agrpais[prop_vuelven_curs_agrpais$country_grouped==" ","country_grouped"] <- NA

writeData(wb, sheet = "t_44_2", x = "Return rate in a year by academic course and country origin", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_44_2", x = "", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_44_2", x = prop_vuelven_curs_agrpais,startRow=3)
writeData(wb, sheet = "INDEX", x = "t_44_2" , startRow=45,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Return rate in a year by academic course and country origin" , startRow=45, startCol = 2)


#PROBABILIDAD DE VOLVER= #alumnos que vuelven despues de un año (Discontinuidad al menos dos veces)
#                        ----------------------------------------
# alumnos que lo dejan (que registran discontinuidad al menos una vez)

# (creo que esta calculado antes)


addWorksheet(wb, sheet = "t_45_2")
# TIEMPO MEDIO DE REENGANCHE
# Calcular el tiempo medio de reenganche para cada codialumne

tiempo_medio_reenganche <- dades %>%
  filter(discontinuidades_cat_12m != "Un solo curso") %>%
  group_by(codialumne) %>%
  summarize(
    tiempo_medio_reenganche = round(mean(tiempo_reenganche_12m, na.rm = TRUE),2) 
  )
table(dades$discontinuidades_cat_12m)
dades_filtradas <- dades %>%
  filter(discontinuidades_cat_12m != "Un solo curso" & discontinuidades_cat_12m != "Mas de un curso consecutivo/continuo")
tiempo_medio_por_categoria <- tiempo_medio_reenganche %>%
  inner_join(dades_filtradas %>% distinct(codialumne, discontinuidades_cat_12m), by = "codialumne") %>%
  group_by(discontinuidades_cat_12m) %>%
  summarize(
    tiempo_medio_categoria = mean(tiempo_medio_reenganche, na.rm = TRUE)
  )
#print(tiempo_medio_por_categoria)
writeData(wb, sheet = "t_45_2", x = "Mean re-engagement time of students ", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_45_2", x = "Segons la seva categoria de discontinuïtat", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_45_2", x = "NOTA: los casos de discontinuitat_cat que no sean realmente discontinuos se han eliminado de la tabla porque no se calcula ningun tiempo en esos casos", startRow = 3, startCol = 1)
writeData(wb, sheet = "t_45_2", x = tiempo_medio_por_categoria,startRow=4)
writeData(wb, sheet = "INDEX", x = "t_45_2" , startRow=46,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Mean re-engagement time of students " , startRow=46, startCol = 2)



# por nivel_final
#addWorksheet(wb, sheet = "t_46_2")

tiempo_medio_por_categoria_nivel_final <- tiempo_medio_reenganche %>%
  inner_join(dades_filtradas %>% distinct(codialumne, discontinuidades_cat_12m, nivel_final), by = "codialumne") %>%
  group_by(nivel_final, discontinuidades_cat_12m) %>%
  summarize(
    tiempo_medio_categoria = mean(tiempo_medio_reenganche, na.rm = TRUE)
  )
#print(tiempo_medio_por_categoria_nivel_final)
#writeData(wb, sheet = "t_46_2", x = "Temps mitjà de reenganxament dels alumnes ", startRow = 1, startCol = 1)
#writeData(wb, sheet = "t_46_2", x = "Segons la seva categoria de discontinuïtat i nivell de curs final matriculat", startRow = 2, startCol = 1)
#writeData(wb, sheet = "t_46_2", x = tiempo_medio_por_categoria_nivel_final,startRow=3)
#writeData(wb, sheet = "INDEX", x = "t_46_2" , startRow=47,startCol = 1)
#writeData(wb, sheet = "INDEX", x = "Temps mitjá de reenganxament dels alumnes. Segons la seva categoria de discontinuitat i nivell de curs final matriculat" , startRow=47, startCol = 2)


# por assoliment_grup_cat
#addWorksheet(wb, sheet = "t_47_2")

tiempo_medio_por_categoria_assoli  <- tiempo_medio_reenganche %>%
  inner_join(dades_filtradas %>% distinct(codialumne, discontinuidades_cat_12m, assoliment_grup_cat), by = "codialumne") %>%
  group_by(assoliment_grup_cat, discontinuidades_cat_12m) %>%
  summarize(
    tiempo_medio_categoria = mean(tiempo_medio_reenganche, na.rm = TRUE)
  )
#print(tiempo_medio_por_categoria_nivel_final)
#writeData(wb, sheet = "t_47_2", x = "temps mitjà de reenganxament dels alumnes ", startRow = 1, startCol = 1)
#writeData(wb, sheet = "t_47_2", x = "segons la seva categoria de discontinuïtat i causa de la discontinuïtat", startRow = 2, startCol = 1)
#writeData(wb, sheet = "t_47_2", x = tiempo_medio_por_categoria_assoli,startRow=3)
#writeData(wb, sheet = "INDEX", x = "t_47_2" , startRow=48,startCol = 1)
#writeData(wb, sheet = "INDEX", x = "Temps mitjá de reenganxament dels alumnes. Segons la seva categoria de discontinuitat i causa de la discontinuïtat" , startRow=48, startCol = 2)



addWorksheet(wb, sheet = "t_48_2")

tabla48 <- tiempo_medio_reenganche %>%
  inner_join(dades_filtradas %>% distinct(codialumne, discontinuidades_cat_12m,gender), by = "codialumne") %>%
  group_by(discontinuidades_cat_12m,gender) %>%
  summarize(
    tiempo_medio_categoria = mean(tiempo_medio_reenganche, na.rm = TRUE)
  )

tabla48[tabla48$gender=="","gender"] <- NA
writeData(wb, sheet = "t_48_2", x = "Mean re-engagement time of students by gender ", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_48_2", x = "Segons la seva categoria de discontinuïtat i genere", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_48_2", x = "NOTA: los casos de discontinuitat_cat que no sean realmente discontinuos se han eliminado de la tabla porque no se calcula ningun tiempo en esos casos", startRow = 3, startCol = 1)
writeData(wb, sheet = "t_48_2", x = tabla48,startRow=4)
writeData(wb, sheet = "INDEX", x = "t_48_2" , startRow=49,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Mean re-engagement time of students by gender" , startRow=49, startCol = 2)


addWorksheet(wb, sheet = "t_49_2")
tabla49 <- tiempo_medio_reenganche %>%
  inner_join(dades_filtradas %>% distinct(codialumne, discontinuidades_cat_12m,edat_cat), by = "codialumne") %>%
  group_by(discontinuidades_cat_12m,edat_cat) %>%
  summarize(
    tiempo_medio_categoria = mean(tiempo_medio_reenganche, na.rm = TRUE)
  )

writeData(wb, sheet = "t_49_2", x = "Mean re-engagement time of students and age category ", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_49_2", x = "Segons la seva categoria de discontinuïtat i categoria de edat ", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_49_2", x = "NOTA: los casos de discontinuitat_cat que no sean realmente discontinuos se han eliminado de la tabla porque no se calcula ningun tiempo en esos casos", startRow = 3, startCol = 1)
writeData(wb, sheet = "t_49_2", x = tabla49,startRow=4)
writeData(wb, sheet = "INDEX", x = "t_49_2" , startRow=50,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Mean re-engagement time of students by age category" , startRow=50, startCol = 2)

addWorksheet(wb, sheet = "t_50_2")
tabla50 <- tiempo_medio_reenganche %>%
  inner_join(dades_filtradas %>% distinct(codialumne, discontinuidades_cat_12m,edat_media_cat), by = "codialumne") %>%
  group_by(discontinuidades_cat_12m,edat_media_cat) %>%
  summarize(
    tiempo_medio_categoria = mean(tiempo_medio_reenganche, na.rm = TRUE)
  )

writeData(wb, sheet = "t_50_2", x = "Mean re-engagement time of students by mean age", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_50_2", x = "Segons la seva categoria de discontinuïtat i categoria de edat mitjá del alumne ", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_50_2", x = "NOTA: los casos de discontinuitat_cat que no sean realmente discontinuos se han eliminado de la tabla porque no se calcula ningun tiempo en esos casos", startRow = 3, startCol = 1)
writeData(wb, sheet = "t_50_2", x = tabla50,startRow=4)
writeData(wb, sheet = "INDEX", x = "t_50_2" , startRow=51,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Mean re-engagement time of students by mean age" , startRow=51, startCol = 2)


addWorksheet(wb, sheet = "t_51_2")
tabla51 <- tiempo_medio_reenganche %>%
  inner_join(dades_filtradas %>% distinct(codialumne, discontinuidades_cat_12m,studies_grouped), by = "codialumne") %>%
  group_by(discontinuidades_cat_12m,studies_grouped) %>%
  summarize(
    tiempo_medio_categoria = mean(tiempo_medio_reenganche, na.rm = TRUE)
  )
writeData(wb, sheet = "t_51_2", x = "Mean re-engagement time of students by study levels  ", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_51_2", x = "Segons la seva categoria de discontinuïtat i nivell de estudis ", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_51_2", x = "NOTA: los casos de discontinuitat_cat que no sean realmente discontinuos se han eliminado de la tabla porque no se calcula ningun tiempo en esos casos", startRow = 3, startCol = 1)
writeData(wb, sheet = "t_51_2", x = tabla51,startRow=4)
writeData(wb, sheet = "INDEX", x = "t_51_2" , startRow=52,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Mean re-engagement time of students by study levels" , startRow=52, startCol = 2)

addWorksheet(wb, sheet = "t_52_2")
tabla52 <- tiempo_medio_reenganche %>%
  inner_join(dades_filtradas %>% distinct(codialumne, discontinuidades_cat_12m,studies_grouped), by = "codialumne") %>%
  group_by(discontinuidades_cat_12m,studies_grouped) %>%
  summarize(
    tiempo_medio_categoria = mean(tiempo_medio_reenganche, na.rm = TRUE)
  )
writeData(wb, sheet = "t_52_2", x = "Mean re-engagement time of students by country origins ", startRow = 1, startCol = 1)
writeData(wb, sheet = "t_52_2", x = "Segons la seva categoria de discontinuïtat i pais d'origen ", startRow = 2, startCol = 1)
writeData(wb, sheet = "t_52_2", x = "NOTA: los casos de discontinuitat_cat que no sean realmente discontinuos se han eliminado de la tabla porque no se calcula ningun tiempo en esos casos", startRow = 3, startCol = 1)
writeData(wb, sheet = "t_52_2", x = tabla52,startRow=4)
writeData(wb, sheet = "INDEX", x = "t_52_2" , startRow=53,startCol = 1)
writeData(wb, sheet = "INDEX", x = "Mean re-engagement time of students by country origins" , startRow=53, startCol = 2)


saveWorkbook(wb, "Descriptive_B2.xlsx", overwrite = TRUE) 





