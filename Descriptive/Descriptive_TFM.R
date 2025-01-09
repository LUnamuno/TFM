#DESCRIPTIVE ANALYSIS
library(dplyr)
library(reshape2) #dacst
library(xtable) #codigo latex

load("cpnl_final20240910_DEF.RData")
#tablas segun orden de aparicion en el TFM
tabla1 <- dades %>% 
  group_by(academic_course) %>%
  summarize(
    n_enrollments =n(), 
    n_students = n_distinct(codialumne),
    n_centres = n_distinct(codicnl)
  )
xtable(tabla1)
tabla2 <- dades %>% 
  group_by(academic_course,studies_grouped) %>%
  summarize(
    n_students = n_distinct(codialumne)
  )%>%  
  group_by(academic_course) %>%
  mutate(pct= round(n_students/sum(n_students)*100,2) 
  )
tabla2$n_students<- NULL
tabla2 <-dcast(tabla2, academic_course ~ studies_grouped) 
xtable(tabla2)

tabla3 <- dades %>% 
  group_by(academic_course,dist_min_12m) %>%
  summarize(
    n_students = n_distinct(codialumne)
  )%>%  
  group_by(academic_course) %>%
  mutate(pct= round(n_students/sum(n_students)*100,2) 
  )
tabla3$n_students<- NULL
tabla3 <-dcast(tabla3, academic_course ~ dist_min_12m) 
tabla3 <- tabla3[,c(1,7,2,3,4,5,6,8)]
xtable(tabla3)

tabla4<- dades %>% 
  group_by(inscr_cat) %>%
  summarize(n_students=n_distinct(codialumne)
  )
xtable(tabla4)
sum(tabla4$n_students)#332696

tabla5 <- dades %>% 
  group_by(academic_course,success) %>%
  summarize(
    n_students = n_distinct(codialumne)
  )%>%  
  group_by(academic_course) %>%
  mutate(pct= round(n_students/sum(n_students)*100,2) 
  )
tabla5$n_students<- NULL
tabla5 <-dcast(tabla5, academic_course ~ success) 
tabla5 <- tabla5[,c(1,2,4,6,3,5)]
xtable(tabla5)


tabla6<- dades %>% 
  group_by(success,course_level) %>%
  summarize(n_enrollments=n() #aqui no puede ir por codialumne, porque algunos repiten cursos
  )%>%  
  group_by(success) %>%
  mutate(pct_disc= round(n_enrollments/sum(n_enrollments)*100,2) 
  )
tabla6$n_enrollments<- NULL
tabla6wide <-dcast(tabla6, success ~ course_level) 
tabla6wide_order <- tabla6wide[,c(1,2,4,6,3,5)]
xtable(tabla6wide_order)

tabla7<- dades %>% 
  group_by(discontinuidades_cat_12m) %>%
  summarize(n_students=n_distinct(codialumne)
  )

xtable(tabla7)
sum(tabla7$n_students)#332696
