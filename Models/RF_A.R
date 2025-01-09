options(future.globals.maxSize= 3670016000)
library(mlr3verse)
library(mlr3measures)
library(mlr3learners)
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

load("../Data/cpnl_final20240910.RData")
load("cpnl_final20240910.RData")
set.seed(123456)

# MODELO 1 (A)----------------------------------------------------------------
#Model per a la probabilitat de no iniciar el curs


dadesA <- dades[!is.na(dades$course_level) & !is.na(dades$morning) & !is.na(dades$intensivity) &
                  !is.na(dades$academic_course) & !is.na(dades$country_grouped) & !is.na(dades$gender) & !is.na(dades$age) &
                  !is.na(dades$studies_grouped) & !is.na(dades$afected_law2015)  &
                  !is.na(dades$initial_language) & !is.na(dades$cnl_name), ] 
				  
dadesA$afected_law2015 <- as.factor(dadesA$afected_law2015)
dadesA$VD_start <- as.factor(dadesA$VD_start)
dadesA[sapply(dadesA, is.character)] <- lapply(dadesA[sapply(dadesA, is.character)], 
                                                             as.factor)

### Keep only necessary columns
dadesA <- dadesA[, c("VD_start", "course_level", "morning", "intensivity", 
                     "academic_course", "gender", "age", "studies_grouped", "afected_law2015", 
                     "country_grouped", "initial_language", "cnl_name")]
					 
#665306 obvs
taskA = as_task_classif(VD_start ~ 
                          gender + age + studies_grouped + country_grouped + initial_language +
                          afected_law2015 + course_level + academic_course + morning + intensivity + 
                          cnl_name, data = dadesA)
taskA$missings()

learner = lrn("classif.ranger", predict_type="prob")


resampling = rsmps("cv") #cross validation
objectA = benchmark(benchmark_grid(taskA, learner, resampling))


splitA = partition(taskA) # por default 2/3
save(splitA, file="splitA.RData")

learner$train(taskA, splitA$train_set)# train the model

predictionA = learner$predict(taskA, splitA$test_set) # predict data

# calculate performance
predictionA$confusion
predictionA$score(msr("classif.acc")) 

rocA <- autoplot(predictionA, type = "roc") 
ggsave("../Results/ROC_plotA.png", plot = rocA)
rocA_2 <-autoplot(predictionA, type = "prc")
ggsave("../Results/PRC_plotA.png", plot = rocA_2)
# Feature importance A  ----------------------------------------------------------------
###  (TIME CONSUMING!!!!!!)

violence_xA = taskA$data(rows = splitA$test,
                         cols = taskA$feature_names)
# contiene 1/3 de los rows y 22 covariables porque son las que le hemos metido al modelo 
# target in test data
violence_yA = taskA$data(rows = splitA$test,
                         cols = taskA$target_names)

predictorA = Predictor$new(learner, data = violence_xA, y = violence_yA)


plan("callr", workers = 8)
#importanceA = FeatureImp$new(predictorA, loss = "ce", n.repetitions = 2) 

#importance_modA <- importanceA$plot()
#plotA <- importance_modA + scale_x_continuous(limits = c(1, NA)) 
#ggsave("../Results/importance_plotA.png", plot = plotA)
save.image("../Results/ModeloA.RData")
# Feature effects A ----------------------------------------------------------------
#load("../Data/ModeloA.RData")
#method = "pdp+ice" ->  indicate that we want to visualize ICE curves with a PD plot (average of the ICE curves)

#GENERO
effectA_genere = FeatureEffect$new(predictorA, feature = "gender",
                                   method = "pdp+ice")

genere <- effectA_genere$plot() + xlab("Gender") + ylab("Not started")
ggsave("../Results/effectA_genere.png", plot = genere)
#MATITARDA
effectA_morning = FeatureEffect$new(predictorA, feature = "morning",
                                      method = "pdp+ice")

morning <- effectA_morning$plot()  + xlab("Afternoon course") + ylab("Not started")
ggsave("../Results/effectA_morning.png", plot = morning)
#NIVELL 
effectA_nivell = FeatureEffect$new(predictorA, feature = "course_level",
                                   method = "pdp+ice")

nivell <- effectA_nivell$plot()  + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  xlab("Course level") + ylab("Not started") + 
                                         scale_x_discrete(labels = c("1" = "Inicials", "2" = "Basic 1", "3" = "Basic 2", 
                                                                     "4" = "Basic 3", "5" = "Elementary", "6" = "Intermediate",
                                                                     "7"="Sufficiency", "8"="Superior"))
ggsave("../Results/effectA_nivell.png", plot = nivell)
#AFECTAT_LLEI2015
effectA_llei = FeatureEffect$new(predictorA, feature = "afected_law2015",
                                 method = "pdp+ice")

llei <- effectA_llei$plot() + xlab("Afected requirement 45h") + ylab("Not started") +
                               scale_x_discrete(labels = c("1" = "Yes", "0" = "No"))
ggsave("../Results/effectA_afected_law2015.png", plot = llei)
#intensivity
effectA_intensivity = FeatureEffect$new(predictorA, feature = "intensivity",
                                         method = "pdp+ice")

intensivity <- effectA_intensivity$plot() + xlab("Intensivity") + ylab("Not started") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(labels = c("1" = "Monthly", "2" = "Bimonthly", "3" = "Three-monthly", 
                              "4" = "Four-monthly", "5" = "Six-monthly", "6" = "Annual"))
ggsave("../Results/effectA_intensivity.png", plot = intensivity)
#EDAT 
effectA_age = FeatureEffect$new(predictorA, feature = "age",
                                method = "pdp+ice")

age <- effectA_age$plot() + xlab("Age") + ylab("Not started")
ggsave("../Results/effectA_age.png", plot = age)
#CURSO ACADEMICO 
effectA_curs = FeatureEffect$new(predictorA, feature = "academic_course",
                                 method = "pdp+ice")

curs <- effectA_curs$plot() + xlab("Academic course") + ylab("Not started") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("../Results/effectA_academic_course.png", plot = curs)

#ESTUDIOS
effectA_estudis = FeatureEffect$new(predictorA, feature = "studies_grouped",
                                 method = "pdp+ice")

estudis <- effectA_estudis$plot() + xlab("Level of studies") + ylab("Not started") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("../Results/effectA_estudis.png", plot = estudis)


#PAIS ORIGEN
effectA_agrpais = FeatureEffect$new(predictorA, feature = "country_grouped",
                                    method = "pdp+ice")

agrpais <- effectA_agrpais$plot() + xlab("Country of origin") + ylab("Not started") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("../Results/effectA_agrpais.png", plot = agrpais)
#CNL NAME
effectA_cnl = FeatureEffect$new(predictorA, feature = "cnl_name",
                                    method = "pdp+ice")

cnl <- effectA_cnl$plot() + xlab("Cnl name") + ylab("Not started") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("../Results/effectA_cnl_name.png", plot = cnl)
#INITIAL LANGUAGE 
effectA_language = FeatureEffect$new(predictorA, feature = "initial_language",
                                    method = "pdp+ice")

language <- effectA_language$plot() + xlab("Initial language") + ylab("Not started") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("../Results/effectA_initial_language.png", plot = language)