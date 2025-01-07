
library(dplyr)
load("../Data/cpnl_final20240910.RData")

#ANTONIOS CHANGES IN VARIABLES (Stata) y sus traducciones al ingles
dades$estudis_corr_mod <- as.factor(dades$estudis_corr_mod)
dades <- dades %>%
  mutate(est_agrup = case_when(
    estudis_corr_mod %in% c("No sap llegir o escriure", "Sense estudis o primària incompleta") ~ "Sense estudis o primària incompleta",
    estudis_corr_mod %in% c("Ed. primària (EGB completa, batxillerat elem.)") ~ "Ed. primària (agrupada amb ESO)",
    estudis_corr_mod %in% c("ESO (ESO, F.prof. grau mitjà, FP 1r grau)") ~ "Ed. primària (agrupada amb ESO)",
    estudis_corr_mod %in% c("Secundària (batx., f. prof.grau sup., FP 2n grau)") ~ "Secundària",
    estudis_corr_mod %in% c("Universitaris (grau, màster, dip., llic., doct.)") ~ "Universitaris",
    estudis_corr_mod %in% c("Sense dades") ~ "Sense dades"
  )) %>%
  mutate(est_agrup = factor(est_agrup, levels = c(
    "Sense estudis o primària incompleta",
    "Ed. primària (agrupada amb ESO)",
    "Secundària",
    "Universitaris",
    "Sense dades"
  ))) %>%
  mutate(est_agrup = recode_factor(est_agrup,
                                   "Sense estudis o primària incompleta" = "No studies or incomplete primary",
                                   "Ed. primària (agrupada amb ESO)" = "Primary and lower secondary",
                                   "Secundària" = "Upper secondary",
                                   "Universitaris" = "University",
                                   "Sense dades" = "NA"
  ))

#summary(dades$est_agrup)

#LLENGUA INICIAL 
dades <- dades %>% 
  mutate( llengua_inicial = recode_factor(llengua_inicial1,
                                                      "Català" = "Catalan",
                                                      "Castellà" = "Spanish",
                                                      "Altra llengua romànica" = "Other Romance language",
                                                      "Àrab" = "Arabic",
                                                      "Altres llengües" = "Other languages",
                                                      "Sense dades"= "NA"
  ))
#summary(dades$llengua_inicial)


#NIVELL DESAGRUPAT 
dades <- dades %>%   
  mutate(nivell_desagrupat = recode_factor(nivell_desagrupat, 
                                           "Inicials"= "Initials",
                                           "Bàsic 1"= "Basic 1",
                                           "Bàsic 2"= "Basic 2",
                                           "Bàsic 3"="Basic 3",
                                           "Elementals"= "Elementary",
                                           "Intermedis"="Intermediate",
                                          "Suficiència"= "Sufficiency",
                                           "Superior"= "Superior"
                                           ))
#summary(dades$nivell_desagrupat)

#VARIABLE PAIS DE ORIGEN : (Stata)
# Crear las variables origen
class (dades$agrpais_corr_mod)
dades$agrpais_corr_mod <- as.factor(dades$agrpais_corr_mod)
summary(dades$agrpais_corr_mod)
dades$agrpais_corr_mod[is.na(dades$agrpais_corr_mod)] <- "NA"
dades <- dades %>%
  mutate(
    # Crear la variable origen agrupando categorías y TRADUCIENDO A INGLES
    origen = recode_factor(agrpais_corr_mod,
      "Catalunya" = "Catalonia",
      "Resta de l'Estat Espanyol" = "Rest of Spain",
      "Unió Europea" = "European Union",
      "Europa Extracomunitària" = "Non-EU Europe",
      "Amèrica Llatina" = "Latin America",
      "EUA i Canadà" = "USA and Canada",
      "Nord d'Àfrica" = "North Africa",
      "Resta d'Àfrica" = "Rest of Africa",
      "Àsia" = "Asia",
      "Oceania" = "Asia", # Agrupando Oceania dentro de Asia como ha hecho ANTONIO
      "Sense dades" = "NA"
    )
  )

#summary(dades$origen)

#INTENSIVITAT 
dades <- dades %>%
  mutate(
    # Recodificar la variable intensivitat
    intensivitat_agrup = case_when(
      intensivitat %in% c(4, 5, 6) ~ 3,  # "quadrimestral/semestral/anual"
      intensivitat == 3 ~ 2,             # "trimestral"
      intensivitat == 2 ~ 1,             # "mensual/bimestral"
      intensivitat == 1 ~ 1 
    )
  ) %>%
  mutate(
    # Convertir a factor con etiquetas descriptivas
    intensivitat_agrup = factor(intensivitat_agrup, levels = 1:3, labels = c(
      "Monthly/bimonthly", "Three monthly", "Four-Six monthly/Annual"
    ))
  )
#summary(dades$intensivitat_agrup)

#VERIFICO QUE LAS SIGUIENTES COLUMNAS SON NUMERICAS
cols_num <- c("edat", "pc_ccaa", "pc_cat","pc_ue","pc_op","pc_al","pc_sec","pc_univ","pc_eo","pc_se")  
dades[cols_num] <- lapply(dades[cols_num], as.numeric)  

#RENOMBRO LAS VARIABLES PARA MI TFM: solo renombro las covariables implicadas
dades <- dades %>% rename(dist_min_12m=dist_min_aula_cat_12m,
                          course_level=nivell_desagrupat,
                          morning=matitarda,
                          intensivity=intensivitat_agrup,
                          academic_course= cursacademic,
                          country_grouped=origen,
                          gender=genere_corr,
                          age= edat,
                          studies_grouped=est_agrup,
                          afected_law2015=afectat_llei2015,
                          n_stud= n_estud,
                          initial_language=llengua_inicial,
                          cnl_name=nomcnl_agr,
                          pct_cat=pc_cat,#catalonia
                          pct_ac=pc_ccaa, #autonomout countries
                          pct_eu=pc_ue, #european union 
                          pct_la= pc_al, #latin america
                          pct_oc=pc_op, #other countries
                          pct_sec=pc_sec, # secondary 
                          pct_univ=pc_univ, #university 
                          pct_ce=pc_eo, #cojmpulsory education 
                          pct_we=pc_se, #without education 
                          success = assoliment_grup) 

dades$VD_start <- as.factor(ifelse(dades$success=="no_inicia",1,0)) #primer modelo

dades <- dades %>% rename(VD_dropout=VD) #segundo modelo 

dades$VD_cont12m <- as.factor(ifelse(dades$continuidad_12m=="Continuidad",0,1))#tercer modelo 
dades$VD_cont6m <- as.factor(ifelse(dades$continuidad_6m=="Continuidad",0,1))
dades$VD_cont1m <- as.factor(ifelse(dades$continuidad_1m=="Continuidad",0,1))

dades[sapply(dades, is.character)] <- lapply(dades[sapply(dades, is.character)], 
                                             as.factor)

dades$gender <- recode_factor(dades$gender, 
                              "Dona"="Woman",
                              "Home"="Man")
dades$morning <- recode_factor(dades$morning,
                               "Matí"="Morning",
                               "Tarda"="Afternoon")
dades$afected_law2015 <- recode_factor(dades$afected_law2015,
                                       "0"="No",
                                       "1"="Yes")
dades$discontinuidades_cat_12m <- recode_factor(dades$discontinuidades_cat_12m,
                                                ">1 discontinuidad"=">1 discontinuity",
                                                "Mas de un curso consecutivo/continuo"="More than a couse consecutive/continued",
                                                "Un solo curso"="Only a course",
                                                "Una discontinuidad"="1 discontinuity")
dades$discontinuidades_cat_6m <- recode_factor(dades$discontinuidades_cat_6m,
                                               ">1 discontinuidad"=">1 discontinuity",
                                               "Mas de un curso consecutivo/continuo"="More than a couse consecutive/continued",
                                               "Un solo curso"="Only a course",
                                               "Una discontinuidad"="1 discontinuity")
dades$discontinuidades_cat_1m <- recode_factor(dades$discontinuidades_cat_1m,
                                               ">1 discontinuidad"=">1 discontinuity",
                                               "Mas de un curso consecutivo/continuo"="More than a couse consecutive/continued",
                                               "Un solo curso"="Only a course",
                                               "Una discontinuidad"="1 discontinuity")
dades$nivel_final <- recode_factor(dades$nivel_final, 
                                   "Bàsics"= "Basics",
                                   "Inicials"= "Initials",
                                   "Intermedis"="Intermediate",
                                   "Suficiència"="Suficiency")
dades$nivel_maximo <- recode_factor(dades$nivel_maximo, 
                                    "Bàsics"= "Basics",
                                    "Inicials"= "Initials",
                                    "Intermedis"="Intermediate",
                                    "Suficiència"="Suficiency")
dades$nivel_partida <- recode_factor(dades$nivel_partida, 
                                     "Bàsics"= "Basics",
                                     "Inicials"= "Initials",
                                     "Intermedis"="Intermediate",
                                     "Suficiència"="Suficiency")

dades$success <- recode_factor(dades$success,
                               "Apte"="Passed",
                               "baixa_abandonament"="Dropout",
                               "No apte"="Not passed",
                               "no_inicia"="Not started",
                               "No presentat/Sense avaluació"="Not submitted/not assessed"
)

save(dades, file= "cpnl_final20240910_DEF.RData")
