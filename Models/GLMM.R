#GLMM MODELS 
library(lme4)
#en LDA usabamos tambien
library(foreign)
library(lattice)
library(ggplot2)
library(nlme) #masked  lmList() and collapse()
library(geepack)
library(dplyr)
library(emmeans)
library(ggeffects)
library(plm)
library(MuMIn)
library(performance) # para colinearidades
library(texreg) # para escribir los modelos en tablas de latex 
library(xtable) #mas formatos en latex 

#install.packages("openxlsx")
library("openxlsx")

load("cpnl_final20240910_DEF.RData")
set.seed(123456)

### Keep only necessary columns
dades <- dades[, c("VD_start", "VD_dropout","VD_cont12m","VD_cont6m","VD_cont1m", "dist_min_12m", "course_level", "morning", "idprof", "intensivity", 
                   "academic_course", "gender", "age", "studies_grouped", "afected_law2015", "n_stud", 
                   "country_grouped", "initial_language", "pct_cat", "pct_ac", "pct_eu", "pct_la", "pct_oc", "pct_univ",
                   "pct_sec", "pct_ce", "pct_we", "cnl_name", "success","codialumne","codicurs")]
summary(dades)


# MODELO NO_INICIA (A) ---------------------------------------------------------------
dadesA <- dades[!is.na(dades$course_level) & !is.na(dades$morning) & !is.na(dades$intensivity) &
                  !is.na(dades$academic_course) & !is.na(dades$country_grouped) & !is.na(dades$gender) & !is.na(dades$age) &
                  !is.na(dades$studies_grouped) & !is.na(dades$afected_law2015)  &
                  !is.na(dades$initial_language) & !is.na(dades$cnl_name), ] 
dadesA$afected_law2015 <- as.factor(dadesA$afected_law2015)
dadesA$VD_start <- as.factor(dadesA$VD_start)
dadesA$codialumne <- as.factor(dadesA$codialumne)
dadesA[sapply(dadesA, is.character)] <- lapply(dadesA[sapply(dadesA, is.character)], 
                                               as.factor)
#PARA USAR LA MISMA DIVISION QUE EN RF:
load("splitA.RData")
dadesA_train <- dadesA[splitA$train,]
dadesA_test <- dadesA[splitA$test,]


#TODAS LAS COVARIABLES MENOS CNL_NAME 
modA1<-glmer(VD_start~gender + age + studies_grouped + country_grouped + initial_language +
               afected_law2015 + course_level + academic_course + morning + intensivity  + 
             (1|codialumne), family=binomial,data=dadesA_train,
             control = glmerControl(optimizer = "bobyqa", 
                                    optCtrl = list(maxfun = 100000)))
check_collinearity(modA1) 
summary(modA1)
wb <- createWorkbook()
addWorksheet(wb, "modA1")
writeData(wb, sheet = "modA1", x = cbind("."=rownames(summary(modA1)$coefficients),summary(modA1)$coefficients) , startRow=1,startCol = 1)
#añado la columna de rownames para que salga explicitamente en el excel. 

code_modA1 <- capture.output(texreg(modA1))
writeLines(code_modA1, "GLMM_modA1.tex")

#sin initial_language
modA2<-glmer(VD_start~gender + age + studies_grouped + country_grouped + 
               afected_law2015 + course_level + academic_course + morning + intensivity  + 
               (1|codialumne), family=binomial,data=dadesA_train,
             control = glmerControl(optimizer = "bobyqa", 
                                    optCtrl = list(maxfun = 100000)))
check_collinearity(modA2) 
summary(modA2)
code_modA2 <- capture.output(texreg(modA2))
writeLines(code_modA2, "GLMM_modA2.tex")
addWorksheet(wb, "modA2")
writeData(wb, sheet = "modA2", x = cbind("."=rownames(summary(modA2)$coefficients),summary(modA2)$coefficients) , startRow=1,startCol = 1)

result_anova<- anova(modA2,modA1) 
# EL MEJOR MODELO:
if (result_anova$`Pr(>Chisq)` > 0.05) {
  modA <- modA2
} else {
  modA <- modA1
}

#USANDO UNA MEJOR APROXIMACION DEL MODELO FINAL:
#nAGQ are the number of point per axis for evaluating the adaptative Gaussian Hermite approximation to the log likelihood. 
#DEFAULT = 1 =Laplace approximation. 
#**values greater that 1 produce greater accuracy in the evaluation of log likelihood (but they are more expensive in time).

modA <- update(modA,nAGQ= 5)


#screenreg(list(modA3,modA2))  ENSEÑAR LA TABLA CHULITA EN EL CONSOLE 
# escribir la tabla en codigo LATEX 
#latex_code <- capture.output(texreg(list(modA1, modA2)))
#writeLines(latex_code, "GLMM_modA1vs2.tex")


# AHORA USANDO TEST SET PREDECIMOS SU VALORES Y DESPUES COMPARAMOS CON SUS VERDADEROS OUTCOMES 

newdata <- dadesA_test[,c("gender","age","studies_grouped","country_grouped","afected_law2015","course_level",
"academic_course","morning","intensivity","codialumne")] #sin el outcom 

dadesA_test$ConditionalProb<-round(predict(modA,newdata=newdata,type="response",allow.new.levels=T),2) # este va a ser el más util 
dadesA_test$MarginalProb<-round(predict(modA,newdata=newdata,type="response",allow.new.levels=T,re.form=NA),2) #ignoran los efectos aleatorios
dadesA_test$ConditionalOdds<-round(exp(predict(modA,newdata=newdata,allow.new.levels=T)),2)
dadesA_test$MarginalOdds<-round(exp(predict(modA,newdata=newdata,allow.new.levels=T,re.form=NA)),2)

#ahora tenemos las predicciones y el outcome original: 
dadesA_test[,c("VD_start","ConditionalProb","MarginalProb","ConditionalOdds","MarginalOdds")]
#eligiendo 0.5 como UMBRAL DE CLASIFICACION
umbral <- 0.5
dadesA_test$Pred_Conditional <- ifelse(dadesA_test$ConditionalProb > umbral, 1, 0)
dadesA_test$Pred_Marginal <- ifelse(dadesA_test$MarginalProb > umbral, 1, 0)

# MATRICES DE CONFUSION 
table_Conditional <- table(Observed = dadesA_test$VD_start, Predicted = dadesA_test$Pred_Conditional)
table_Marginal <- table(Observed = dadesA_test$VD_start, Predicted = dadesA_test$Pred_Marginal)

# Convertir a formato LaTeX
latex_table_conditional <- xtable(table_Conditional, caption = "Matriz de confusión (Predicciones Condicionales)")
latex_table_marginal <- xtable(table_Marginal, caption = "Matriz de confusión (Predicciones Marginales)")

# Guardar las tablas en archivos de texto plano
cat(print(latex_table_conditional, type = "latex", include.rownames = TRUE), 
    file = "conditional_CM_modA.txt")
cat(print(latex_table_marginal, type = "latex", include.rownames = TRUE), 
    file = "marginal_CM_modA.txt")
# GOODNESS OF FIT A ---------------------------------------------------------
#install.packages('performance')
#install.packages('DHARMa')
library(DHARMa)
library(performance)

#modelo A final 
sim_res_modA <- simulateResiduals(fittedModel = modA)
plot(sim_res_modA)
testUniformity(sim_res_modA)
testDispersion(sim_res_modA)
testZeroInflation(sim_res_modA)  # También útil si hay sospecha de inflación de ceros
testOutliers(sim_res_modA)
outliers(sim_res_modA)

# MODELO DROPOUT (B) ---------------------------------------------------------------

dadesB <- subset(dades, dades$success != "no_inicia") 
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

# la misma dividion que con RF para train-test
load("splitB.RData")
dadesB_train <- dadesB[splitB$train,]
dadesB_test <- dadesB[splitB$test,]

dadesB_train$idprof <- droplevels(dadesB_train$idprof) # por si en el split alguna idprof se queda a 0, sino peta
#original model: TODAS MENOS CNL_NAME NI IDPROF
modB1<-glmer(VD_dropout~dist_min_12m +  course_level + morning + intensivity + academic_course + gender +
               age + studies_grouped +  afected_law2015 + n_stud + country_grouped + initial_language  +
               pct_cat + pct_ac +  pct_eu + pct_la + pct_oc +
               pct_univ + pct_sec + pct_ce + pct_we  + 
               (1|codialumne), family=binomial,data=dadesB_train,
             control = glmerControl(optimizer = "bobyqa", 
                                    optCtrl = list(maxfun = 100000)))

check_collinearity(modB1)  
summary(modB1)
code_modB1 <- capture.output(texreg(modB1))
writeLines(code_modB1, "GLMM_modB1.tex")
addWorksheet(wb, "modB1")
writeData(wb, sheet = "modB1", x = cbind("."=rownames(summary(modB1)$coefficients),summary(modB1)$coefficients) , startRow=1,startCol = 1)

#sin initial_language
modB2<-glmer(VD_dropout~dist_min_12m +  course_level  + morning + intensivity + academic_course + gender +
               age + studies_grouped +  afected_law2015 + n_stud + country_grouped   +
               pct_cat + pct_ac +  pct_eu + pct_la + pct_oc +
               pct_univ + pct_sec + pct_ce + pct_we  + 
               (1|codialumne), family=binomial,data=dadesB_train,
             control = glmerControl(optimizer = "bobyqa", 
                                    optCtrl = list(maxfun = 100000)))

check_collinearity(modB2)  
summary(modB2)
code_modB2 <- capture.output(texreg(modB2))
writeLines(code_modB2, "GLMM_modB2.tex")
addWorksheet(wb, "modB2")
writeData(wb, sheet = "modB2", x = cbind("."=rownames(summary(modB2)$coefficients),summary(modB2)$coefficients) , startRow=1,startCol = 1)

result_anovaB<- anova(modB2,modB1) 
# EL MEJOR MODELO:
if (result_anovaB$`Pr(>Chisq)` > 0.05) {
  modB <- modB2
} else {
  modB <- modB1
}

#USANDO UNA MEJOR APROXIMACION DEL MODELO FINAL:
modB <- update(modB,nAGQ= 5)

# AHORA USANDO TEST SET PREDECIMOS SU VALORES Y DESPUES COMPARAMOS CON SUS VERDADEROS OUTCOMES 

newdataB <- dadesB_test[,c("gender","age","studies_grouped","country_grouped","afected_law2015","course_level",
                          "academic_course","morning","intensivity","codialumne","dist_min_12m","n_stud",
                          "pct_cat","pct_ac","pct_eu","pct_la","pct_oc","pct_univ","pct_sec","pct_ce","pct_we")] 

dadesB_test$ConditionalProb<-round(predict(modB,newdata=newdataB,type="response",allow.new.levels=T),2) # este va a ser el más util 
dadesB_test$MarginalProb<-round(predict(modB,newdata=newdataB,type="response",allow.new.levels=T,re.form=NA),2) #ignoran los efectos aleatorios
dadesB_test$ConditionalOdds<-round(exp(predict(modB,newdata=newdataB,allow.new.levels=T)),2)
dadesB_test$MarginalOdds<-round(exp(predict(modB,newdata=newdataB,allow.new.levels=T,re.form=NA)),2)

#ahora tenemos las predicciones y el outcome original: 
dadesB_test[,c("VD_dropout","ConditionalProb","MarginalProb","ConditionalOdds","MarginalOdds")]

#eligiendo 0.5 como UMBRAL DE CLASIFICACION
umbral <- 0.5
dadesB_test$Pred_Conditional <- ifelse(dadesB_test$ConditionalProb > umbral, 1, 0)
dadesB_test$Pred_Marginal <- ifelse(dadesB_test$MarginalProb > umbral, 1, 0)

# MATRICES DE CONFUSION 
table_ConditionalB <- table(Observed = dadesB_test$VD_dropout, Predicted = dadesB_test$Pred_Conditional)
table_MarginalB <- table(Observed = dadesB_test$VD_dropout, Predicted = dadesB_test$Pred_Marginal)

# Convertir a formato LaTeX
latex_table_conditionalB <- xtable(table_ConditionalB, caption = "Matriz de confusión (Predicciones Condicionales)")
latex_table_marginalB <- xtable(table_MarginalB, caption = "Matriz de confusión (Predicciones Marginales)")

# Guardar las tablas en archivos de texto plano
cat(print(latex_table_conditionalB, type = "latex", include.rownames = TRUE), 
    file = "conditional_CM_modB.txt")
cat(print(latex_table_marginalB, type = "latex", include.rownames = TRUE), 
    file = "marginal_CM_modB.txt")
# GOODNESS OF FIT B---------------------------------------------------------

#modelo B final (modB1 que esta en modB)
sim_res_modB <- simulateResiduals(fittedModel = modB)
plot(sim_res_modB)
testUniformity(sim_res_modB)
testDispersion(sim_res_modB)
testZeroInflation(sim_res_modB)  
testOutliers(sim_res_modB)
outliers(sim_res_modB)

# MODELO CONT (C) ---------------------------------------------------------------
dadesC <- subset(dades, dades$success != "no_inicia") 
dadesC <- dadesC %>%
  filter(!(success == "Apte" & course_level == "Superior")) # filtro de las matricular superiores que ya han aprobado (no tienen opcion de continuar)
dadesC <- subset(dadesC, dadesC$academic_course != "2019-2020") 

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
dadesC$success <- droplevels(dadesB$success) 
dadesC$academic_course <- droplevels(dadesB$academic_course)
dadesC$course_level <- droplevels(dadesB$course_level)

# la misma dividion que con RF para train-test
load("splitC.RData")
dadesC_train <- dadesB[splitC$train,]
dadesC_test <- dadesB[splitC$test,]

#original model: SIN IDPROF NI CNL_NAME
modC1<-glmer(VD_cont12m ~ dist_min_12m +  course_level  + morning + intensivity + academic_course + gender +
               age + studies_grouped +  afected_law2015 + n_stud + country_grouped + initial_language  +
               pct_cat + pct_ac +  pct_eu + pct_la + pct_oc +
               pct_univ + pct_sec + pct_ce + pct_we  + success + 
               (1|codialumne), family=binomial,data=dadesC_train,
             control = glmerControl(optimizer = "bobyqa", 
                                    optCtrl = list(maxfun = 100000)))
check_collinearity(modC1) 
summary(modC1)
code_modC1 <- capture.output(texreg(modC1))
writeLines(code_modC1, "GLMM_modC1.tex")
addWorksheet(wb, "modC1")
writeData(wb, sheet = "modC1", x = cbind("."=rownames(summary(modC1)$coefficients),summary(modC1)$coefficients) , startRow=1,startCol = 1)

# sin initial_language
modC2<-glmer(VD_cont12m ~ dist_min_12m +  course_level  + morning + intensivity + academic_course + gender +
               age + studies_grouped +  afected_law2015 + n_stud + country_grouped  +
               pct_cat + pct_ac +  pct_eu + pct_la + pct_oc +
               pct_univ + pct_sec + pct_ce + pct_we  + success + 
               (1|codialumne), family=binomial,data=dadesC_train,
             control = glmerControl(optimizer = "bobyqa", 
                                    optCtrl = list(maxfun = 100000)))
check_collinearity(modC2)  
summary(modC2)
code_modC2 <- capture.output(texreg(modC2))
writeLines(code_modC2, "GLMM_modC2.tex")
addWorksheet(wb, "modC2")
writeData(wb, sheet = "modC2", x = cbind("."=rownames(summary(modC2)$coefficients),summary(modC2)$coefficients) , startRow=1,startCol = 1)


result_anovaC<- anova(modC2,modC1) 
# EL MEJOR MODELO:
if (result_anovaC$`Pr(>Chisq)` > 0.05) {
  modC <- modC2
  # SI ESTE ES EL MEJOR DE AMBOS, EJECUTO EL MODELO CON INTERACCION A PARTIR DE EL 
  modC_inter<-glmer(VD_cont12m~dist_min_12m*success +  course_level  + morning + intensivity + academic_course + gender +
                      age + studies_grouped +  afected_law2015 + n_stud + country_grouped +
                      pct_cat + pct_ac +  pct_eu + pct_la + pct_oc +
                      pct_univ + pct_sec + pct_ce + pct_we  +  
                      (1|codialumne), family=binomial,data=dadesC_train,
                    control = glmerControl(optimizer = "bobyqa", 
                                           optCtrl = list(maxfun = 100000)))
  check_collinearity(modC_inter)  
  summary(modC_inter)
  code_modC_inter <- capture.output(texreg(modC_inter))
  writeLines(code_modC_inter, "GLMM_modC_inter2.tex")
  
  addWorksheet(wb, "modC_inter")
  writeData(wb, sheet = "modC_inter", x = cbind("."=rownames(summary(modC_inter)$coefficients),summary(modC_inter)$coefficients) , startRow=1,startCol = 1)
  saveWorkbook(wb, "GLMM_mod_coefficients.xlsx", overwrite = TRUE) 
  
  #ahora comparo modC con modC_inter
  result_anovaCinter<- anova(modC,modC_inter) 
  # EL MEJOR MODELO:
  if (result_anovaCinter$`Pr(>Chisq)` > 0.05) {
    modC_final <- modC
  } else {
    modC_final <- modC_inter
  }
} else {
  modC <- modC1
  #SI ES ESTE OTRO: 
  modC_inter<-glmer(VD_cont12m~dist_min_12m*success +  course_level  + morning + intensivity + academic_course + gender +
                      age + studies_grouped +  afected_law2015 + n_stud + country_grouped + initial_language  +
                      pct_cat + pct_ac +  pct_eu + pct_la + pct_oc +
                      pct_univ + pct_sec + pct_ce + pct_we  +  
                      (1|codialumne), family=binomial,data=dadesC_train,
                    control = glmerControl(optimizer = "bobyqa", 
                                           optCtrl = list(maxfun = 100000)))
  check_collinearity(modC_inter)  
  summary(modC_inter)
  code_modC_inter <- capture.output(texreg(modC_inter))
  writeLines(code_modC_inter, "GLMM_modC_inter1.tex")
  result_anovaCinter<- anova(modC,modC_inter) 
  
  addWorksheet(wb, "modC_inter")
  writeData(wb, sheet = "modC_inter", x = cbind("."=rownames(summary(modC_inter)$coefficients),summary(modC_inter)$coefficients) , startRow=1,startCol = 1)
  saveWorkbook(wb, "GLMM_mod_coefficients.xlsx", overwrite = TRUE) 
  
  # EL MEJOR MODELO:
  if (result_anovaCinter$`Pr(>Chisq)` > 0.05) {
    modC_final <- modC
  } else {
    modC_final <- modC_inter
  }
}

#modC_final !! 


#USANDO UNA MEJOR APROXIMACION DEL MODELO FINAL:
modC_final <- update(modC_final,nAGQ= 5)

# AHORA USANDO TEST SET PREDECIMOS SU VALORES Y DESPUES COMPARAMOS CON SUS VERDADEROS OUTCOMES 

newdataC <- dadesC_test[,c("gender","age","studies_grouped","country_grouped","afected_law2015","course_level",
                           "academic_course","morning","intensivity","codialumne","dist_min_12m","n_stud",
                           "pct_cat","pct_ac","pct_eu","pct_la","pct_oc","pct_univ","pct_sec","pct_ce","pct_we","success")] 

dadesC_test$ConditionalProb<-round(predict(modC_final,newdata=newdataC,type="response",allow.new.levels=T),2) # este va a ser el más util 
dadesC_test$MarginalProb<-round(predict(modC_final,newdata=newdataC,type="response",allow.new.levels=T,re.form=NA),2) #ignoran los efectos aleatorios
dadesC_test$ConditionalOdds<-round(exp(predict(modC_final,newdata=newdataC,allow.new.levels=T)),2)
dadesC_test$MarginalOdds<-round(exp(predict(modC_final,newdata=newdataC,allow.new.levels=T,re.form=NA)),2)

#ahora tenemos las predicciones y el outcome original: 
dadesC_test[,c("VD_dropout","ConditionalProb","MarginalProb","ConditionalOdds","MarginalOdds")]

#eligiendo 0.5 como UMBRAL DE CLASIFICACION
umbral <- 0.5
dadesC_test$Pred_Conditional <- ifelse(dadesC_test$ConditionalProb > umbral, 1, 0)
dadesC_test$Pred_Marginal <- ifelse(dadesC_test$MarginalProb > umbral, 1, 0)

# MATRICES DE CONFUSION 
table_ConditionalC <- table(Observed = dadesC_test$VD_cont12m, Predicted = dadesC_test$Pred_Conditional)
table_MarginalC <- table(Observed = dadesC_test$VD_cont12m, Predicted = dadesC_test$Pred_Marginal)

# Convertir a formato LaTeX
latex_table_conditionalC <- xtable(table_ConditionalC, caption = "Matriz de confusión (Predicciones Condicionales)")
latex_table_marginalC <- xtable(table_MarginalC, caption = "Matriz de confusión (Predicciones Marginales)")

# Guardar las tablas en archivos de texto plano
cat(print(latex_table_conditionalC, type = "latex", include.rownames = TRUE), 
    file = "conditional_CM_modC.txt")
cat(print(latex_table_marginalC, type = "latex", include.rownames = TRUE), 
    file = "marginal_CM_modC.txt")

# GOODNESS OF FIT C ---------------------------------------------------------

#modelo C final modC_final
sim_res_modC <- simulateResiduals(fittedModel = modC_final)
plot(sim_res_modC)
testUniformity(sim_res_modC)
testDispersion(sim_res_modC)
testZeroInflation(sim_res_modC)  
testOutliers(sim_res_modC)
outliers(sim_res_modC)
