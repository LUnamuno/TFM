options(future.globals.maxSize= 8291456000)
library(mlr3verse)
#library(mlr3measures)
library(mlr3learners)
library(forcats)
library(mlr3pipelines)
library(mlr3viz)
library(precrec)
library(e1071)
library(ggplot2)
#library(readxl)
#library(haven)
#library(tidygeocoder)
#library(sp)
library(dplyr)
#library(lubridate)
library(iml)
#library(counterfactuals)
#library(tidyverse)
library("future")
library("future.callr") #PARALELIZATION

load("../Data/cpnl_final20240910_DEF.RData")
set.seed(123456)


# MODELO 3 (C) ----------------------------------------------------------------
# MODELO CON VD CONTINUIDAD  

#pero en este modelo hay que ELIMINAR (HACER SUBSET) las matriculas en el Superior,
#ya que no hay opcion de seguir mariculandose en algo superior
dadesC <- subset(dades, dades$success != "no_inicia") 
dadesC <- dadesC %>%
  filter(!(success == "Apte" & course_level == "Superior")) # filtro de las matricular superiores que ya han aprobado (no tienen opcion de continuar)
dadesC <- subset(dadesC, dadesC$academic_course != "2019-2020") 

### Keep only necessary columns
dadesC <- dadesC[, c("VD_cont12m", "dist_min_12m", "course_level", "morning", "idprof", "intensivity", 
                     "academic_course", "gender", "age", "studies_grouped", "afected_law2015", "n_stud", 
                     "country_grouped", "initial_language", "pct_cat", "pct_ac", "pct_eu", "pct_la", "pct_oc", "pct_univ",
                     "pct_sec", "pct_ce", "pct_we", "cnl_name", "success")]

#582127 obvs 
dadesC <- dadesC[!is.na(dadesC$dist_min_12m) & !is.na(dadesC$course_level) & !is.na(dadesC$idprof) &
                   !is.na(dadesC$morning) & !is.na(dadesC$intensivity) & !is.na(dadesC$academic_course)
                 & !is.na(dadesC$country_grouped) & !is.na(dadesC$gender) & !is.na(dadesC$age) &
                   !is.na(dadesC$studies_grouped) & !is.na(dadesC$afected_law2015) &  !is.na(dadesC$n_stud) &
                   !is.na(dadesC$initial_language) & !is.na(dadesC$pct_cat) & !is.na(dadesC$pct_ac) & !is.na(dadesC$pct_eu) &
                   !is.na(dadesC$pct_la) & !is.na(dadesC$pct_oc) & !is.na(dadesC$pct_univ)& !is.na(dadesC$pct_sec) & !is.na(dadesC$pct_ce) & 
                   !is.na(dadesC$pct_we) & !is.na(dadesC$cnl_name) & !is.na(dadesC$success), ] 
dadesC$afected_law2015 <- as.factor(dadesC$afected_law2015)
dadesC$VD_cont12m <- as.factor(dadesC$VD_cont12m)
dadesC[sapply(dadesC, is.character)] <- lapply(dadesC[sapply(dadesC, is.character)], 
                                                             as.factor)

#578366 obvs  
taskC= as_task_classif(VD_cont12m ~
                         dist_min_12m +  course_level + morning + intensivity + idprof +
                         academic_course + gender + age + studies_grouped +  afected_law2015 + n_stud + 
                         country_grouped + initial_language  + pct_cat + pct_ac +  pct_eu + pct_la + pct_oc + pct_univ +
                         pct_sec + pct_ce + pct_we + cnl_name + success, data = dadesC)
taskC$missings()

learner = lrn("classif.ranger", predict_type="prob")

#
resampling = rsmps("cv") #cross validation
objectC = benchmark(benchmark_grid(taskC, learner, resampling))
# 

splitC = partition(taskC) # por defautl 2/3
save(splitC, file="splitC.RData")

learner$train(taskC, splitC$train_set)# train the model

predictionC = learner$predict(taskC, splitC$test_set) # predict data

# calculate performance 
predictionC$confusion
predictionC$score(msr("classif.acc")) 

rocC <- autoplot(predictionC, type = "roc")  
ggsave("../Results/ROC_plotC.png", plot = rocC)
rocC_2 <-autoplot(predictionC, type = "prc")
ggsave("../Results/PRC_plotC.png", plot = rocC_2)
# Feature importance C  ----------------------------------------------------------------

violence_xC = taskC$data(rows = splitC$test,
                         cols = taskC$feature_names)
violence_yC = taskC$data(rows = splitC$test,
                         cols = taskC$target_names)

#Reordenar la variable DENTRO  del test dataset 
violence_xC$dist_min_12m <- fct_relevel(violence_xC$dist_min_12m, "0","(0,1.5]","(1.5,5]" ,"(10,20]" ,"(5,10]",">20","NA")
#levels(violence_xC$dist_min_12m) <- c("0","(0,1.5]","(1.5,5]" ,"(10,20]" ,"(5,10]",">20","Sense dades")

predictorC = Predictor$new(learner, data = violence_xC, y = violence_yC)
#save.image("../Results/ModeloC.RData")
plan("callr", workers = 8)
#importanceC = FeatureImp$new(predictorC, loss = "ce", n.repetitions = 2) 

#importance_modC <- importanceC$plot()
#plotC <- importance_modC + scale_x_continuous(limits = c(1, NA)) 

#ggsave("../Results/importance_plotC.png", plot = plotC)
save.image("../Results/ModeloC.RData")

# DISTANCIA
effectC_dist = FeatureEffect$new(predictorC, feature = "dist_min_12m",
                                 method = "pdp+ice")

dist <- effectC_dist$plot() + xlab("Distance") + ylab("Continuity") +
                              theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("../Results/effectC_dist.png", plot = dist)
#GENERO
effectC_genere = FeatureEffect$new(predictorC, feature = "gender",
                                   method = "pdp+ice")

genere <- effectC_genere$plot() + xlab("Gender") + ylab("Continuity")
ggsave("../Results/effectC_genere.png", plot = genere)
#MATITARDA
effectC_morning = FeatureEffect$new(predictorC, feature = "morning",
                                     method = "pdp+ice")

morning <- effectC_morning$plot() + xlab("Afternoon course") + ylab("Continuity")
ggsave("../Results/effectC_morning.png", plot = morning)
#NIVELL 
effectC_nivell = FeatureEffect$new(predictorC, feature = "course_level",
                                  method = "pdp+ice")

nivell <- effectC_nivell$plot()  + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  xlab("Course level") + ylab("Continuity") + 
                                  scale_x_discrete(labels = c("1" = "Inicials", "2" = "Basic 1", "3" = "Basic 2", 
                                  "4" = "Basic 3", "5" = "Elementary", "6" = "Intermediate",
                                  "7"="Sufficiency", "8"="Superior"))
ggsave("../Results/effectC_nivell.png", plot = nivell)
#AFECTAT_LLEI2015
effectC_llei = FeatureEffect$new(predictorC, feature = "afected_law2015",
                                method = "pdp+ice")

llei <- effectC_llei$plot() + xlab("Afected requirement 45h") + ylab("Continuity") + scale_x_discrete(labels = c("1" = "Yes", "0" = "No"))
ggsave("../Results/effectC_afected_law2015.png", plot = llei)
#EDAT 
effectC_age = FeatureEffect$new(predictorC, feature = "age",
                                 method = "pdp+ice")

age <- effectC_age$plot() + xlab("Age") + ylab("Continuity")
ggsave("../Results/effectC_age.png", plot = age)
#CURSO ACADEMICO 
effectC_curs = FeatureEffect$new(predictorC, feature = "academic_course",
                                 method = "pdp+ice")

curs <- effectC_curs$plot() + xlab("Academic course") + ylab("Continuity") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("../Results/effectC_academic_course.png", plot = curs)
#numero de estudiantes 
effectC_estud = FeatureEffect$new(predictorC, feature = "n_stud",
                                  method = "pdp+ice")

estud <- effectC_estud$plot() + xlab("Number of students in class")  + ylab("Continuity")
ggsave("../Results/effectC_n_stud.png", plot = estud)

#intensivity
effectC_intensivity = FeatureEffect$new(predictorC, feature = "intensivity",
                                         method = "pdp+ice")

intensivity <- effectC_intensivity$plot() + xlab("Intensivity") + ylab("Continuity") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("../Results/effectC_intensivity.png", plot = intensivity)

#assoliment
effectC_assoliment = FeatureEffect$new(predictorC, feature = "success",
                                         method = "pdp+ice")

assoliment <- effectC_assoliment$plot() + xlab("Success") + ylab("Continuity") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
				scale_x_discrete(labels = c("Apte" = "Passed", "No apte" = "No passed", "baixa_abandonament" = "Dropout", "No presentat/Sense avaluació" = "No attended"))
ggsave("../Results/effectC_assoliment.png", plot = assoliment)

#ESTUDIOS 
effectC_estudis = FeatureEffect$new(predictorC, feature = "studies_grouped",
                                    method = "pdp+ice")
estudis <- effectC_estudis$plot() + xlab("Level of studies") + ylab("Continuity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
ggsave("../Results/effectC_estudis.png", plot = estudis)
#AGRPAIS 
effectC_agrpais = FeatureEffect$new(predictorC, feature = "country_grouped",
                                    method = "pdp+ice")
agrpais <- effectC_agrpais$plot() + xlab("Country of origin") + ylab("Continuity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
ggsave("../Results/effectC_agrpais.png", plot = agrpais)
#CNL NAME
effectC_cnl = FeatureEffect$new(predictorC, feature = "cnl_name",
                                method = "pdp+ice")

cnl <- effectC_cnl$plot() + xlab("Cnl name") + ylab("Continuity") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("../Results/effectC_cnl_name.png", plot = cnl)
#INITIAL LANGUAGE 
effectC_language = FeatureEffect$new(predictorC, feature = "initial_language",
                                     method = "pdp+ice")

language <- effectC_language$plot() + xlab("Initial language") + ylab("Dropout") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("../Results/effectC_initial_language.png", plot = language)

# Counterfactuals  ----------------------------------------------------------------
library(cluster)
library(StatMatch)

dadesC_test <- dadesC[splitC$test,]
dadesC_test <- dadesC_test[dadesC_test$VD_cont12m!=0, ] 
# PARA HACER EL CLUSTERING EL OUTCOME NO SE TIENE EN CUENTA !!! dadesC_test[,-1]

# Cargar la función claraBD desde el archivo descargado
source("ClaraFunction.R")

#gower_dist <- gower.dist(dadesC_test[,-1])

# Evaluar la calidad de los clústeres con silhouette
#silhouette_scores <- sapply(2:10, function(k) {
#  claradb_fit <-  claraBD(
#    x = dadesC_test[,-1],       # Base de datos original (no una matriz de distancias)
#    k = k,                   # Número de clusters deseados
#    metric = "gower",        # Métrica para calcular disimilaridades (soporta datos mixtos)
#    samples = 8              # Número de muestras (recomendado entre 5 y 10)
#  )
#  clusters <- claradb_fit$clustering
#  sil <- silhouette(clusters, dist = gower_dist)
  # av.silhouettes:
#  summary(sil)$avg.width
#})
# Ver el valor de k con el mayor ancho promedio del silhouette (+1 por los indices del for anterior)
#best_k <- which.max(silhouette_scores)+1
#cat("El número óptimo de clústeres según el silhouette width es:", best_k , "\n")

#claraBD_best_fit <-  claraBD(x = dadesC_test[,-1],k = best_k,metric = "gower",samples = 8)
#medoids_indices <- claraBD_best_fit$i.med 
# Filas originales de los datos correspondientes a los medoids
#representantes <- dadesC_test[medoids_indices, ] #AQUI YA PODEMOS VER COMPLETO, NO IMPORTA
#NO SE PUEDE EJECUTAR POR LA GRAN CARGA COMPUTACIONAL DE SILHOUETTE Y GOWER DISTANCE PARA EL TEST DATASET


# Crea el predictor utilizando `iml`
#taskC_clean <- taskC$clone(deep = TRUE) 
#learner$train(taskC) 
predictorC_sample <- Predictor$new(
  model = learner,            
  data = dadesC_test,       
  y = "VD_cont12m"         
)
fijasC <- c("course_level","idprof","morning","intensivity","academic_course","gender","age","studies_grouped","afected_law2015","n_stud","country_grouped","initial_language",
           "pct_cat","pct_ac","pct_eu","pct_la","pct_oc","pct_univ","pct_sec","pct_ce","pct_we","cnl_name")

fijas_indiv <- c("gender","age","studies_grouped","afected_law2015","country_grouped","initial_language","academic_course")

moc_classif <- MOCClassif$new(
  predictorC_sample,
  epsilon = 0, 
  fixed_features = fijas_indiv,     
  quiet = TRUE, 
  termination_crit = "genstag", 
  n_generations = 10L
)

# Función para calcular contrafactuales para un índice específico
calculate_counterfactual <- function(index, moc_classif, dadesC_test, desired_class, desired_prob) {
  # Seleccionar el punto de interés
  x_interest <- dadesC_test[index, , drop = FALSE]
  
  # Verificar que sea un data.frame con una sola fila
  if (!is.data.frame(x_interest) || nrow(x_interest) != 1) {
    stop("El x_interest no es válido. Debe ser un data.frame con una sola fila.")
  }
  
  # Calcular el contrafactual
  cfactuals <- moc_classif$find_counterfactuals(
    x_interest,
    desired_class = desired_class,
    desired_prob = desired_prob
  )
  
  # Evaluar y validar
  valid_cf <- cfactuals$subset_to_valid()
  
  # Retornar el resultado
  list(
    original = x_interest,
    counterfactuals = cfactuals,
    valid_cf = valid_cf
  )
}

#OTRO APPROACH 
# en vez de con best_k , elegir nosotros el numero de cluster que querriamos hacer, por ejemplo 5
claraBD_5fit <-  claraBD(x = dadesC_test[,-1],k = 5,metric = "gower",samples = 8)
medoids_indices5 <- claraBD_best_fit$i.med 
# Filas originales de los datos correspondientes a los medoids
representantes5 <- dadesC_test[medoids_indices, ]

results5 <- lapply(medoids_indices5, function(index) {
  calculate_counterfactual(
    index = index,
    moc_classif = moc_classif,
    dadesC_test = dadesC_test,
    desired_class = "0",    # Cambiar a 'CONTINUO'
    desired_prob = c(0.95, 1)
  )
})

save(results5,file="counterfactuals_ModeloC_ClaraBD5.RData")
#load("counterfactuals_ModeloC_ClaraBD5.RData")
#results5$counterfactuals no hay counterfactuals 


# VOY A RELAJAR LA DESIRED PROB 
results5_2 <- lapply(medoids_indices5, function(index) {
  calculate_counterfactual(
    index = index,
    moc_classif = moc_classif,
    dadesC_test = dadesC_test,
    desired_class = "0",    # Cambiar a 'CONTINUO'
    desired_prob = c(0.75, 1)
  )
})
save(results5_2,file="counterfactuals_ModeloC_ClaraBD5_2.RData")
#load("counterfactuals_ModeloC_ClaraBD5_2.RData")
results5_2$counterfactuals
library("openxlsx")
wb <- createWorkbook()

repr1_orig<- results5_2[[1]]$counterfactuals$x_interest
repr1_count<-results5_2[[1]]$counterfactuals$data

repr2_orig<- results5_2[[2]]$counterfactuals$x_interest
repr2_count<-results5_2[[2]]$counterfactuals$data

repr3_orig<- results5_2[[3]]$counterfactuals$x_interest
repr3_count<-results5_2[[3]]$counterfactuals$data

repr4_orig<- results5_2[[4]]$counterfactuals$x_interest
repr4_count<-results5_2[[4]]$counterfactuals$data

repr5_orig<- results5_2[[5]]$counterfactuals$x_interest
repr5_count<-results5_2[[5]]$counterfactuals$data

rows <- as.data.frame(c("1 representative",rep(" ",nrow(repr1_count)),"2 representative",rep(" ",nrow(repr2_count)),
                        "3 representative", rep(" ",nrow(repr3_count)),"4 representative",rep(" ",nrow(repr4_count)),
                        "5 representative",rep(" ",nrow(repr5_count))))

#como hay mas de un counterfactual por cada representante, en este caso no lo trasposamos y escribiremos un excel
# en vez de un codigo latex
tablaB_MOC_5<- rbind("1_orig"=repr1_orig,
                     "1_count"=repr1_count,
                     "2_orig"=repr2_orig,
                     "2_count"=repr2_count,
                     "3_orig"=repr3_orig,
                     "3_count"=repr3_count,
                     "4_orig"=repr4_orig,
                     "4_count"=repr4_count,
                     "5_orig"=repr5_orig,
                     "5_count"=repr5_count)
col <- colnames(tablaB_MOC_5)
tablaB_MOC_5 <- cbind(rows, tablaB_MOC_5)
colnames(tablaB_MOC_5) <- c(" ",col)
#guardar en Excel
addWorksheet(wb, sheet = "MOC_2")

writeData(wb, sheet = "MOC_2", x = "MOC counterfactuals for 5 clusters", startRow = 1, startCol = 1)
writeData(wb, sheet = "MOC_2", x = "Relaxing desired_probabilities to 0.75-1", startRow = 2, startCol = 1)
writeData(wb, sheet = "MOC_2", x = tablaB_MOC_5,startRow=3)


# + VOY A HACER UN MOC SIN VARIABLES FIJAS :) 
moc_classif_libre <- MOCClassif$new(
  predictorC_sample,
  epsilon = 0,    
  quiet = TRUE, 
  termination_crit = "genstag", 
  n_generations = 10L
)
results5_3 <- lapply(medoids_indices5, function(index) {
  calculate_counterfactual(
    index = index,
    moc_classif = moc_classif_libre,
    dadesC_test = dadesC_test,
    desired_class = "0",    # Cambiar a 'CONTINUO'
    desired_prob = c(0.75, 1)
  )
})
save(results5_3,file="counterfactuals_ModeloC_ClaraBD5_3.RData")
load("counterfactuals_ModeloC_ClaraBD5_3.RData")
#results5_3$counterfactuals


repr1_orig<- results5_3[[1]]$counterfactuals$x_interest
repr1_count<-results5_3[[1]]$counterfactuals$data

repr2_orig<- results5_3[[2]]$counterfactuals$x_interest
repr2_count<-results5_3[[2]]$counterfactuals$data

repr3_orig<- results5_3[[3]]$counterfactuals$x_interest
repr3_count<-results5_3[[3]]$counterfactuals$data

repr4_orig<- results5_3[[4]]$counterfactuals$x_interest
repr4_count<-results5_3[[4]]$counterfactuals$data

repr5_orig<- results5_3[[5]]$counterfactuals$x_interest
repr5_count<-results5_3[[5]]$counterfactuals$data

rows <- as.data.frame(c("1 representative",rep(" ",nrow(repr1_count)),"2 representative",rep(" ",nrow(repr2_count)),
                        "3 representative", rep(" ",nrow(repr3_count)),"4 representative",rep(" ",nrow(repr4_count)),
                        "5 representative",rep(" ",nrow(repr5_count))))

#como hay mas de un counterfactual por cada representante, en este caso no lo trasposamos y escribiremos un excel
# en vez de un codigo latex
tablaB_MOC_5_libre<- rbind("1_orig"=repr1_orig,
                           "1_count"=repr1_count,
                           "2_orig"=repr2_orig,
                           "2_count"=repr2_count,
                           "3_orig"=repr3_orig,
                           "3_count"=repr3_count,
                           "4_orig"=repr4_orig,
                           "4_count"=repr4_count,
                           "5_orig"=repr5_orig,
                           "5_count"=repr5_count)
col <- colnames(tablaB_MOC_5_libre)
tablaB_MOC_5_libre <- cbind(rows, tablaB_MOC_5_libre)
colnames(tablaB_MOC_5_libre) <- c(" ", col)
#guardar en Excel
addWorksheet(wb, sheet = "MOC_4")

writeData(wb, sheet = "MOC_4", x = "MOC counterfactuals for 5 clusters", startRow = 1, startCol = 1)
writeData(wb, sheet = "MOC_4", x = "Relaxing desired_probabilities to 0.75-1 and leaving covariates free", startRow = 2, startCol = 1)
writeData(wb, sheet = "MOC_4", x = tablaB_MOC_5_libre,,startRow=3)

saveWorkbook(wb, "MOC_ModeloC.xlsx", overwrite = TRUE) 



# CREAR EL METODO WHAT-IF EN VEZ DE MOC
# estemetodo busca el punto mas cercano en la bbdd con la probabilidad deseada
# en este caso no se pueden fijar covariables

#library(counterfactuals) necesario para Whatif

calculate_counterfactual_whatif <- function(index,pred,dades,desired_class, desired_prob,n_cont) {
  # Seleccionar el punto de interes-representante
  x_interest <- dades[index, , drop = FALSE]
  
  # Verificar que sea un data.frame con una sola fila
  if (!is.data.frame(x_interest) || nrow(x_interest) != 1) {
    stop("El x_interest no es válido. Debe ser un data.frame con una sola fila.")
  }
  
  whatif = WhatIfClassif$new(pred, n_counterfactuals= n_cont)
  cfe= whatif$find_counterfactuals (x_interest,desired_clas= desired_class,desired_prob =desired_prob)
  result = data.frame(cfe$evaluate(show_diff=TRUE))
  list(original = x_interest,
       counterfactuals = result # es un data frame nrows= n_cont
  )
}
# busco 1 counterfactuals (1L) mas cercanos al representante de cada cluster
# medoids_indices5 son los medoids encontrados con claraBD para 5 clusters
results_whatif5<- lapply(medoids_indices5, function(index) {
  calculate_counterfactual_whatif(
    index = index,
    pred= predictorC_sample,
    dades = dadesC_test,
    desired_class = "0",    # Cambiar a 'CONTINUO'
    desired_prob = c(0.75, 1),
    n_cont= 1L
  )
})
save(results_whatif5,file="counterfactuals_ModeloC_Whatif5.RData")
load("counterfactuals_ModeloC_Whatif5.RData")

# el primero es el real valor del outcome (sabemos que es 0)
tablaC_whatif_5<- rbind("1_orig"=results_whatif5[[1]]$original[2:24] ,
                        "1_count"=results_whatif5[[1]]$counterfactuals[1:23],
                        "2_orig"=results_whatif5[[2]]$original[2:24],
                        "2_count"=results_whatif5[[2]]$counterfactuals[1:23],
                        "3_orig"=results_whatif5[[3]]$original[2:24] ,
                        "3_count"=results_whatif5[[3]]$counterfactuals[1:23],
                        "4_orig"=results_whatif5[[4]]$original[2:24] ,
                        "4_count"=results_whatif5[[4]]$counterfactuals[1:23],
                        "5_orig"=results_whatif5[[5]]$original[2:24],
                        "5_count"=results_whatif5[[5]]$counterfactuals[1:23])
tablaC_whatif_5_traspose <- as.data.frame(t(tablaB_whatif_5))

tablaC_whatif_5_stats <- rbind("1"=as.data.frame(results_whatif5[[1]]$counterfactuals[24:28]),
                               "2"=as.data.frame(results_whatif5[[2]]$counterfactuals[24:28]),
                               "3"=as.data.frame(results_whatif5[[3]]$counterfactuals[24:28]),
                               "4"=as.data.frame(results_whatif5[[4]]$counterfactuals[24:28]),
                               "5"=as.data.frame(results_whatif5[[5]]$counterfactuals[24:28]))
tabla_latex <- xtable(tablaC_whatif_5_traspose, caption= "WHAT IF model C")
cat(print(tabla_latex, type = "latex", include.rownames = TRUE),file = "WhatIfC_5_values.txt")

tabla_latex2 <- xtable(tablaC_whatif_5_stats, caption= "WHAT IF model C")
cat(print(tabla_latex2, type = "latex", include.rownames = TRUE),file = "WhatIfC_5_stats.txt")

# dist_min=0  ----------------------------------------------------------------
dadesC_modif <- dadesC
dadesC_modif[splitC$test,"dist_min_12m"] <- "0"
taskC_modif = as_task_classif(VD_cont12m ~
                                dist_min_12m +  course_level + idprof + morning + intensivity + academic_course + gender +
                                age + studies_grouped +  afected_law2015 + n_stud + country_grouped + initial_language  +
                                pct_cat + pct_ac +  pct_eu + pct_la + pct_oc +
                                pct_univ + pct_sec + pct_ce + pct_we + cnl_name, data = dadesC_modif)

predictionC_modif = learner$predict(taskC_modif, splitC$test_set)

# Valores del outcome (VD_dropout)
actual_values <- dadesC[splitC$test_set, "VD_cont12m"]
predicted_values_original <- predictionC$response
predicted_values_modified <- predictionC_modif$response

# Matriz de confusión para predicciones después de modificar la variable
conf_matrix_modified <- table(Actual = actual_values, Predicted = predicted_values_modified)
conf_matrix_modified_latex <- xtable(conf_matrix_modified, caption= "Matriz de confusión para predicciones con variable modificada:")
cat(print(conf_matrix_modified_latex, type = "latex", include.rownames = TRUE),
    file = "Dist0_modeloC_1.txt")


# Matriz de confusión-comparativa de predicciones
conf_matrix_comparison <- table(Original = predicted_values_original, Modified = predicted_values_modified)
conf_matrix_comparison_latex <- xtable(conf_matrix_comparison, caption="Matriz de confusión entre predicciones iniciales y predicciones modificadas:")
cat(print(conf_matrix_comparison_latex, type = "latex", include.rownames = TRUE),
    file = "Dist0_modeloC_2.txt")
