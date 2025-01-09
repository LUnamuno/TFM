options(future.globals.maxSize= 6291456000)
library(mlr3verse)
library(mlr3measures)
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
library(counterfactuals)
#library(tidyverse)
library("future")
library("future.callr") #PARALELIZATION

load("../Data/cpnl_final20240910_DEF.RData")
set.seed(123456)


# MODELO 2 (B) ----------------------------------------------------------------
#Model per a la probabilitat d’abandonar el curs (entre els que inicien)

dadesB <- subset(dades, dades$success != "no_inicia") 
#615534 obvs 

dadesB <- dadesB[!is.na(dadesB$dist_min_12m) & !is.na(dadesB$course_level) & !is.na(dadesB$idprof) &
                     !is.na(dadesB$morning) & !is.na(dadesB$intensivity) & !is.na(dadesB$academic_course)
                   & !is.na(dadesB$country_grouped) & !is.na(dadesB$gender) & !is.na(dadesB$age) &
                     !is.na(dadesB$studies_grouped) & !is.na(dadesB$afected_law2015) &  !is.na(dadesB$n_stud) &
                     !is.na(dadesB$initial_language) & !is.na(dadesB$pct_cat) & !is.na(dadesB$pct_ac) & !is.na(dadesB$pct_eu) &
                     !is.na(dadesB$pct_la) &!is.na(dadesB$pct_oc) & !is.na(dadesB$pct_univ)& !is.na(dadesB$pct_sec) & !is.na(dadesB$pct_ce) & 
                     !is.na(dadesB$pct_we) & !is.na(dadesB$cnl_name), ] 
					 
dadesB$afected_law2015 <- as.factor(dadesB$afected_law2015)
dadesB$VD_dropout <- as.factor(dadesB$VD_dropout)
dadesB[sapply(dadesB, is.character)] <- lapply(dadesB[sapply(dadesB, is.character)],as.factor)
#IMPORTANTE: para los counterfactuals necesito que las variables sean o numericas o factores(verificar antes de seguir)

### Keep only necessary columns
dadesB <- dadesB[, c("VD_dropout", "dist_min_12m", "course_level", "morning", "idprof", "intensivity", 
                     "academic_course", "gender", "age", "studies_grouped", "afected_law2015", "n_stud", 
                     "country_grouped", "initial_language", "pct_cat", "pct_ac", "pct_eu", "pct_la", "pct_oc", "pct_univ",
                     "pct_sec", "pct_ce", "pct_we", "cnl_name")]
 
#611624 obvs  
taskB = as_task_classif(VD_dropout ~
                           dist_min_12m +  course_level + idprof + morning + intensivity + academic_course + gender +
                           age + studies_grouped +  afected_law2015 + n_stud + country_grouped + initial_language  +
                           pct_cat + pct_ac +  pct_eu + pct_la + pct_oc +
                           pct_univ + pct_sec + pct_ce + pct_we + cnl_name, data = dadesB)
taskB$missings()

learner = lrn("classif.ranger", predict_type="prob")

#
resampling = rsmps("cv") #cross validation
objectB = benchmark(benchmark_grid(taskB, learner, resampling))
# 

splitB = partition(taskB) # por default 2/3
save(splitB, file="splitB.RData")

learner$train(taskB, splitB$train_set)# train the model

predictionB = learner$predict(taskB, splitB$test_set) # predict data

# calculate performance%
predictionB$confusion
predictionB$score(msr("classif.acc")) 

rocB <- autoplot(predictionB, type = "roc") 
ggsave("../Results/ROC_plotB.png", plot = rocB)
rocB_2 <-autoplot(predictionB, type = "prc")
ggsave("../Results/PRC_plotB.png", plot = rocB_2)
#We can also plot the precision-recall curve (PRC) which visualizes the PPV/precision vs. TPR/recall. The main difference between ROC curves and PR curves is that the number of true-negatives are ignored in the latter. This can be useful in imbalanced populations where the positive class is rare, and where a classifier with high TPR may still not be very informative and have low PPV

# Feature importance C ----------------------------------------------------------------
violence_xB = taskB$data(rows = splitB$test,
                           cols = taskB$feature_names)
# contiene 1/3 de los rows y 22 covariables porque son las que le hemos metido al modelo 
# target in test data
violence_yB = taskB$data(rows = splitB$test,
                           cols = taskB$target_names)
#Reordenar la variable DENTRO  del test dataset 
violence_xB$dist_min_12m <- fct_relevel(violence_xB$dist_min_12m, "0","(0,1.5]","(1.5,5]" ,"(10,20]" ,"(5,10]",">20","NA")
#levels(violence_xB$dist_min_12m) <- c("0","(0,1.5]","(1.5,5]" ,"(10,20]" ,"(5,10]",">20","Sense dades") PARA INGLES NO NECESITO CAMBIARLO 

predictorB = Predictor$new(learner, data = violence_xB, y = violence_yB)


plan("callr", workers = 8)
#importanceB = FeatureImp$new(predictorB, loss = "ce", n.repetitions = 2) 

#importance_modB <- importanceB$plot()
#plotB <- importance_modB + scale_x_continuous(limits = c(1, NA)) 

#ggsave("../Results/importance_plotB.png", plot = plotB)
save.image("../Results/ModeloB.RData")
# Feature effects  ----------------------------------------------------------------
#load("ModeloB.RData")
#method = "pdp+ice" ->  indicate that we want to visualize ICE curves with a PD plot (average of the ICE curves)

# DISTANCIA
effectB_dist = FeatureEffect$new(predictorB, feature = "dist_min_12m",
                                 method = "pdp+ice")

dist <- effectB_dist$plot() + xlab("Distance") + ylab("Dropout") +
                              theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("../Results/effectB_dist.png", plot = dist)
#GENERO
effectB_genere = FeatureEffect$new(predictorB, feature = "gender",
                                   method = "pdp+ice")

genere <- effectB_genere$plot() + xlab("Gender") + ylab("Dropout")
ggsave("../Results/effectB_genere.png", plot = genere)
#MATITARDA
effectB_morning = FeatureEffect$new(predictorB, feature = "morning",
                                     method = "pdp+ice")

morning <- effectB_morning$plot() + xlab("Afternoon course") + ylab("Dropout")
ggsave("../Results/effectB_morning.png", plot = morning)
#NIVELL 
effectB_nivell = FeatureEffect$new(predictorB, feature = "course_level",
                                  method = "pdp+ice")

nivell <- effectB_nivell$plot()  + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  xlab("Course level") + ylab("Dropout") + 
                                    scale_x_discrete(labels = c("1" = "Inicials", "2" = "Basic 1", "3" = "Basic 2", 
                                      "4" = "Basic 3", "5" = "Elementary", "6" = "Intermediate",
                                        "7"="Sufficiency", "8"="Superior"))
ggsave("../Results/effectB_nivell.png", plot = nivell)
#AFECTAT_LLEI2015
effectB_llei = FeatureEffect$new(predictorB, feature = "afected_law2015",
                                method = "pdp+ice")

llei <- effectB_llei$plot() + xlab("Afected requirement 45h") + ylab("Dropout") + scale_x_discrete(labels = c("1" = "Yes", "0" = "No"))
ggsave("../Results/effectB_afected_law2015.png", plot = llei)
#EDAT 
effectB_age = FeatureEffect$new(predictorB, feature = "age",
                                 method = "pdp+ice")

age <- effectB_age$plot() + xlab("Age") + ylab("Dropout")
ggsave("../Results/effectB_age.png", plot = age)
#CURSO ACADEMICO 
effectB_curs = FeatureEffect$new(predictorB, feature = "academic_course",
                                 method = "pdp+ice")

curs <- effectB_curs$plot() + xlab("Academic course") + ylab("Dropout") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("../Results/effectB_academic_course.png", plot = curs)
#numero de estudiantes 
effectB_estud = FeatureEffect$new(predictorB, feature = "n_stud",
                                  method = "pdp+ice")

estud <- effectB_estud$plot() + xlab("Number of students in class")  + ylab("Dropout")
ggsave("../Results/effectB_n_stud.png", plot = estud)

#intensivity
effectB_intensivity = FeatureEffect$new(predictorB, feature = "intensivity",
                                         method = "pdp+ice")
intensivity <- effectB_intensivity$plot() + xlab("Intensivity") + ylab("Dropout") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
ggsave("../Results/effectB_intensivity.png", plot = intensivity)
#ESTUDIOS 
effectB_estudis = FeatureEffect$new(predictorB, feature = "studies_grouped",
                                        method = "pdp+ice")
estudis <- effectB_estudis$plot() + xlab("Level of studies") + ylab("Dropout") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
ggsave("../Results/effectB_estudis.png", plot = estudis)
#AGRPAIS 
effectB_agrpais = FeatureEffect$new(predictorB, feature = "country_grouped",
                                        method = "pdp+ice")
agrpais <- effectB_agrpais$plot() + xlab("Country of origin") + ylab("Dropout") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
ggsave("../Results/effectB_agrpais.png", plot = agrpais)
#CNL NAME
effectB_cnl = FeatureEffect$new(predictorB, feature = "cnl_name",
                                method = "pdp+ice")

cnl <- effectB_cnl$plot() + xlab("Cnl name") + ylab("Dropout") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("../Results/effectB_cnl_name.png", plot = cnl)
#INITIAL LANGUAGE 
effectB_language = FeatureEffect$new(predictorB, feature = "initial_language",
                                     method = "pdp+ice")

language <- effectB_language$plot() + xlab("Initial language") + ylab("Dropout") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("../Results/effectB_initial_language.png", plot = language)

# Counterfactuals  ----------------------------------------------------------------
library(cluster)
library(StatMatch) #gower.dist devuelve una matriz cuadrada :) 

dadesB_test <- dadesB[splitB$test,]
dadesB_test <- dadesB_test[dadesB_test$VD_dropout!=0, ]  

#EJECUTAMOS CLUSTERING CON EL METODO PAM Y BUSCAMOS EL NUM OPTIMO DE CLUSTERS:

#CLUSTERING SIN LA VARIABLE OUTCOME dadesB_test[,-1]

gower_dist <- gower.dist(dadesB_test[,-1])

# Evaluar la calidad de los clústeres con silhouette
silhouette_scores <- sapply(2:10, function(k) {
  pam_fit <- pam(gower_matrix, diss = TRUE, k = k) #CLUSTERING CON PAM para k nunmero de clusters 
  pam_fit$silinfo$avg.width  # Ancho promedio del silhouette
})
# Ver el valor de k con el mayor ancho promedio del silhouette (+1 por los indices del for anterior)
best_k <- which.max(silhouette_scores)+1
cat("El número óptimo de clústeres según el silhouette width es:", best_k , "\n")

pam_best_fit <- pam(gower_matrix, diss = TRUE, k = best_k)
medoids_indices <- as.numeric(pam_best_fit$medoids) 
# Filas originales de los datos correspondientes a los medoids
representantes <- dadesB_test[medoids_indices, ]
#print(representantes)

# Crea el predictor utilizando `iml`
#taskB_clean <- taskB$clone(deep = TRUE) 
#learner$train(taskB) 
predictorB_sample <- Predictor$new(
  model = learner,            
  data = dadesB_test,       
  y = "VD_dropout"         
)
fijas <- c("course_level","idprof","morning","intensivity","academic_course","gender","age","studies_grouped","afected_law2015","n_stud","country_grouped","initial_language",
           "pct_cat","pct_ac","pct_eu","pct_la","pct_oc","pct_univ","pct_sec","pct_ce","pct_we","cnl_name")

fijas_indiv <- c("gender","age","studies_grouped","afected_law2015","country_grouped","initial_language","academic_course")


moc_classif <- MOCClassif$new(
  predictorB_sample,
  epsilon = 0, 
  fixed_features = fijas_indiv,     
  quiet = TRUE, 
  termination_crit = "genstag", 
  n_generations = 10L
)

# Función para calcular contrafactuales para un índice específico
calculate_counterfactual <- function(index, moc_classif, dadesB_test, desired_class, desired_prob) {
  # Seleccionar el punto de interés
  x_interest <- dadesB_test[index, , drop = FALSE]
  
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

# Ejecutar la función para múltiples índices
results <- lapply(medoids_indices, function(index) {
  calculate_counterfactual(
    index = index,
    moc_classif = moc_classif,
    dadesB_test = dadesB_test,
    desired_class = "0",    # Cambiar a 'no abandono'
    desired_prob = c(0.95, 1)
  )
})

save(results,file="counterfactuals_PAMbestk.RData")
load("counterfactuals_PAMbestk.RData")

results$counterfactuals

results$original
results$valid_cf

#OTRO APPROACH 
# en vez de con best_k , elegir nosotros el numero de cluster que querriamos hacer, por ejemplo 5
pam_5fit <- pam(gower_matrix, diss = TRUE, k = 5)
medoids_indices5 <- as.numeric(pam_5fit$medoids) 
# Filas originales de los datos correspondientes a los medoids
representantes5 <- dadesB_test[medoids_indices5, ]

results5 <- lapply(medoids_indices5, function(index) {
  calculate_counterfactual(
    index = index,
    moc_classif = moc_classif,
    dadesB_test = dadesB_test,
    desired_class = "0",    # Cambiar a 'no abandono'
    desired_prob = c(0.95, 1)
  )
})

save(results5,file="counterfactuals_PAM5.RData")

load("counterfactuals_PAM5.RData")
results5$counterfactuals

results5$original
results5$valid_cf

# VOY A RELAJAR LA DESIRED PROB 
resultsbestk_2 <- lapply(medoids_indices, function(index) {
  calculate_counterfactual(
    index = index,
    moc_classif = moc_classif,
    dadesC_test = dadesB_test,
    desired_class = "0",    # Cambiar a 'CONTINUO'
    desired_prob = c(0.75, 1)
  )
})
save(resultsbestk_2,file="counterfactuals_ModeloB_PAMbestk_2.RData")
#load("counterfactuals_ModeloB_PAMbestk_2.RData")
#resultsbestk_2 solo son 2 clusters 

repr1_orig<- resultsbestk_2[[1]]$counterfactuals$x_interest
repr1_count<-resultsbestk_2[[1]]$counterfactuals$data

repr2_orig<- resultsbestk_2[[2]]$counterfactuals$x_interest
repr2_count<-resultsbestk_2[[2]]$counterfactuals$data

rows <- as.data.frame(c("1 representative",rep(" ",nrow(repr1_count)),"2 representative",rep(" ",nrow(repr2_count))))

#como hay mas de un counterfactual por cada representante, en este caso no lo trasposamos y escribiremos un excel
# en vez de un codigo latex
tablaB_MOC_bestk<- rbind("1_orig"=repr1_orig,
                            "1_count"=repr1_count,
                            "2_orig"=repr2_orig,
                            "2_count"=repr2_count)
col<- colnames(tablaB_MOC_bestk)
tablaB_MOC_bestk <- cbind(rows, tablaB_MOC_bestk)
colnames(tablaB_MOC_bestk) <- c(" ",col)
#guardar como excel:
#install.packages("openxlsx")
library("openxlsx")
wb <- createWorkbook()
addWorksheet(wb, sheet = "MOC_1")

writeData(wb, sheet = "MOC_1", x = "MOC counterfactuals for best k clusters", startRow = 1, startCol = 1)
writeData(wb, sheet = "MOC_1", x = "Relaxing desired_probabilities to 0.75-1", startRow = 2, startCol = 1)
writeData(wb, sheet = "MOC_1", x = tablaB_MOC_bestk,startRow=3)


results5_2 <- lapply(medoids_indices5, function(index) {
  calculate_counterfactual(
    index = index,
    moc_classif = moc_classif,
    dadesC_test = dadesB_test,
    desired_class = "0",    # Cambiar a 'CONTINUO'
    desired_prob = c(0.75, 1)
  )
})
save(results5_2,file="counterfactuals_ModeloB_PAM5_2.RData")
#load("counterfactuals_ModeloB_PAM5_2.RData")
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
  predictorB_sample,
  epsilon = 0,    
  quiet = TRUE, 
  termination_crit = "genstag", 
  n_generations = 10L
)
resultsbestk_3 <- lapply(medoids_indices function(index) {
  calculate_counterfactual(
    index = index,
    moc_classif = moc_classif_libre,
    dadesC_test = dadesB_test,
    desired_class = "0",    # Cambiar a 'CONTINUO'
    desired_prob = c(0.75, 1)
  )
})
save(resultsbestk_3,file="counterfactuals_ModeloB_PAMbestk_3.RData")
#load("counterfactuals_ModeloB_PAMbestk_3.RData")
#resultsbestk_3 solo son 2 clusters 
repr1_orig<- resultsbestk_3[[1]]$counterfactuals$x_interest
repr1_count<-resultsbestk_3[[1]]$counterfactuals$data

repr2_orig<- resultsbestk_3[[2]]$counterfactuals$x_interest
repr2_count<-resultsbestk_3[[2]]$counterfactuals$data

rows <- as.data.frame(c("1 representative",rep(" ",nrow(repr1_count)),"2 representative",rep(" ",nrow(repr2_count))))

#como hay mas de un counterfactual por cada representante, en este caso no lo trasposamos y escribiremos un excel
# en vez de un codigo latex
tablaB_MOC_bestk_libre<- rbind("1_orig"=repr1_orig,
                         "1_count"=repr1_count,
                         "2_orig"=repr2_orig,
                         "2_count"=repr2_count)
col<- colnames(tablaB_MOC_bestk_libre)
tablaB_MOC_bestk_libre <- cbind(rows, tablaB_MOC_bestk_libre)
colnames(tablaB_MOC_bestk_libre) <- c(" ",col)
#guardar como excel:
addWorksheet(wb, sheet = "MOC_3")

writeData(wb, sheet = "MOC_3", x = "MOC counterfactuals for best k clusters", startRow = 1, startCol = 1)
writeData(wb, sheet = "MOC_3", x = "Relaxing desired_probabilities to 0.75-1 and leaving covariates free", startRow = 2, startCol = 1)
writeData(wb, sheet = "MOC_3", x = tablaB_MOC_bestk_libre,startRow=3)

results5_3 <- lapply(medoids_indices5, function(index) {
  calculate_counterfactual(
    index = index,
    moc_classif = moc_classif_libre,
    dadesC_test = dadesB_test,
    desired_class = "0",    # Cambiar a 'CONTINUO'
    desired_prob = c(0.75, 1)
  )
})
save(results5_3,file="counterfactuals_ModeloB_PAM5_3.RData")
#load("counterfactuals_ModeloB_PAM5_3.RData")
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

saveWorkbook(wb, "MOC_ModeloB.xlsx", overwrite = TRUE) 


# CREAR EL METODO WHAT-IF EN VEZ DE MOC 
# estemetodo busca el punto mas cercano en la bbdd con la probabilidad deseada
# en este caso no se pueden fijar covariables 

library(counterfactuals) #necesario para Whatif

calculate_counterfactual_whatif <- function(index,pred,dades,desired_class, desired_prob,n_cont) {
  # Seleccionar el punto de interes-representante
  x_interest <- dades[index, , drop = FALSE]
  
  # Verificar que sea un data.frame con una sola fila
  if (!is.data.frame(x_interest) || nrow(x_interest) != 1) {
    stop("El x_interest no es válido. Debe ser un data.frame con una sola fila.")
  }
  
  whatif = WhatIfClassif$new(pred, n_counterfactuals= n_cont)
  cfe= whatif$find_counterfactuals (x_interest,desired_clas= desired_class,desired_prob =desired_prob)
  result= data.frame(cfe$evaluate(show_diff=TRUE))
  list(original = x_interest,
       counterfactuals = result # es un data frame nrows= n_cont
      )
}
#usando medoids_indices miro los coutnerfactuals de los best k de PAM 
results_whatif_bestk<- lapply(medoids_indices, function(index) {
  calculate_counterfactual_whatif(
    index = index,
    pred= predictorB,
    dades = dadesB_test,
    desired_class = "0",    # Cambiar a 'no abandono'
    desired_prob = c(0.75, 1)
    n_cont= 1L
  )
})
save(results_whatifbestk,file="counterfactuals_ModeloB_Whatifbestk.RData")
#load("counterfactuals_ModeloB_Whatifbestk.RData")
#son solo dos, entonces convierto tablas para latex
tablaB_whatif_bestk<- rbind("1_orig"=results_whatif_bestk[[1]]$original[2:24] ,
                            "1_count"=results_whatif_bestk[[1]]$counterfactuals[1:23],
                            "2_orig"=results_whatif_bestk[[2]]$original[2:24],
                            "2_count"=results_whatif_bestk[[2]]$counterfactuals[1:23])
tablaB_whatif_bestk_traspose <- as.data.frame(t(tablaB_whatif_bestk))

tablaB_whatif_bestk_stats <- rbind("1"=as.data.frame(results_whatif_bestk[[1]]$counterfactuals[24:28]),
                                   "2"=as.data.frame(results_whatif_bestk[[2]]$counterfactuals[24:28]))

tabla_latex <- xtable(tablaB_whatif_bestk_traspose, caption= "WHAT IF")
cat(print(tabla_latex, type = "latex", include.rownames = TRUE),file = "WhatIfB_bestk_values.txt")

tabla_latex2 <- xtable(tablaB_whatif_bestk_stats, caption= "WHAT IF")
cat(print(tabla_latex2, type = "latex", include.rownames = TRUE),file = "WhatIfB_bestk_stats.txt")

# usando medoids_indices5 miro los counterfactuals de los 5 representantes de PAM k=5
results_whatif5<- lapply(medoids_indices5, function(index) {
  calculate_counterfactual_whatif(
    index = index,
    pred= predictorB,
    dades = dadesB_test,
    desired_class = "0",    # Cambiar a 'no abandono'
    desired_prob = c(0.75, 1)
    n_cont= 1L
  )
})
save(results_whatif5,file="counterfactuals_ModeloB_Whatif5.RData")
# el primero es el real valor del outcome (sabemos que es 0)
tablaB_whatif_5<- rbind("1_orig"=results_whatif5[[1]]$original[2:24] ,
                        "1_count"=results_whatif5[[1]]$counterfactuals[1:23],
                        "2_orig"=results_whatif5[[2]]$original[2:24],
                        "2_count"=results_whatif5[[2]]$counterfactuals[1:23],
                        "3_orig"=results_whatif5[[3]]$original[2:24] ,
                        "3_count"=results_whatif5[[3]]$counterfactuals[1:23],
                        "4_orig"=results_whatif5[[4]]$original[2:24] ,
                        "4_count"=results_whatif5[[4]]$counterfactuals[1:23],
                        "5_orig"=results_whatif5[[5]]$original[2:24],
                        "5_count"=results_whatif5[[5]]$counterfactuals[1:23])
tablaB_whatif_5_traspose <- as.data.frame(t(tablaB_whatif_5))

tablaB_whatif_5_stats <- rbind("1"=as.data.frame(results_whatif5[[1]]$counterfactuals[24:28]),
                               "2"=as.data.frame(results_whatif5[[2]]$counterfactuals[24:28]),
                               "3"=as.data.frame(results_whatif5[[3]]$counterfactuals[24:28]),
                               "4"=as.data.frame(results_whatif5[[4]]$counterfactuals[24:28]),
                               "5"=as.data.frame(results_whatif5[[5]]$counterfactuals[24:28]))
tabla_latex <- xtable(tablaB_whatif_5_traspose, caption= "WHAT IF")
cat(print(tabla_latex, type = "latex", include.rownames = TRUE),file = "WhatIfB_5_values.txt")

tabla_latex2 <- xtable(tablaB_whatif_5_stats, caption= "WHAT IF")
cat(print(tabla_latex2, type = "latex", include.rownames = TRUE),file = "WhatIfB_5_stats.txt")

# dist_min=0  ----------------------------------------------------------------
dadesB_modif <- dadesB
dadesB_modif[splitB$test,"dist_min_12m"] <- "0"
taskB_modif = as_task_classif(VD_dropout ~
                          dist_min_12m +  course_level + idprof + morning + intensivity + academic_course + gender +
                          age + studies_grouped +  afected_law2015 + n_stud + country_grouped + initial_language  +
                          pct_cat + pct_ac +  pct_eu + pct_la + pct_oc +
                          pct_univ + pct_sec + pct_ce + pct_we + cnl_name, data = dadesB_modif)

predictionB_modif = learner$predict(taskB_modif, splitB$test_set) 

# Valores del outcome (VD_dropout)
actual_values <- dadesB[splitB$test_set, "VD_dropout"]
predicted_values_original <- predictionB$response
predicted_values_modified <- predictionB_modif$response

# Matriz de confusión para predicciones después de modificar la variable 
conf_matrix_modified <- table(Actual = actual_values, Predicted = predicted_values_modified)
conf_matrix_modified_latex <- xtable(conf_matrix_modified, caption= "Matriz de confusión para predicciones con variable modificada:")
cat(print(conf_matrix_modified_latex, type = "latex", include.rownames = TRUE), 
    file = "Dist0_modeloB_1.txt")


# Matriz de confusión-comparativa de predicciones 
conf_matrix_comparison <- table(Original = predicted_values_original, Modified = predicted_values_modified)
conf_matrix_comparison_latex <- xtable(conf_matrix_comparison, caption="Matriz de confusión entre predicciones iniciales y predicciones modificadas:")
cat(print(conf_matrix_comparison_latex, type = "latex", include.rownames = TRUE), 
    file = "Dist0_modeloB_2.txt")

