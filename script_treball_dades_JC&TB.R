################################################################################
# Authors: Jose Calderon & Toni Bosch                                          #
# Date: December 2023                                                          #
################################################################################

################################################################################
# ------------------ LIBRARIES ------------------------------------------------#
library(tidyverse)
library(dplyr)
library(ggplot2)
library(inspectdf)

################################################################################
# ------------------ LOAD DATA ------------------------------------------------#
accidents <- read.csv(file = 'data/Accidents_de_tr_nsit_amb_morts_o_ferits_greus_a_Catalunya.csv',
                      stringsAsFactors = T)

# Variable mes --> month of the year
accidents$mes <- format(as.Date(accidents$dat), "%B")
accidents$mes <- as.factor(accidents$mes)

# We will create some auxiliar variables
# Variable puntuacioAccident --> How bad is the accident

#!!!!!!!!!!!!!!!!!!!!!! REVISAR! FERITS LLEUS 
accidents <- mutate(accidents,
                    puntuacioAccident = 9 * F_MORTS + 3 * F_FERITS_GREUS + 1 * F_FERITS_LLEUS)

# Variable severitat --> If score bigger than 9, considered severe
accidents <- mutate(accidents, severitat = ifelse(puntuacioAccident < 9, 0, 1))
accidents$severitat <- as.factor(accidents$severitat)


################################################################################
# -------------------- CLEAN DATA ---------------------------------------------#

visual_inspection <- function(df, col1, col2) {

  plt1 <- ggplot(data = df, aes_string(x = col2, fill = col1)) +
    geom_bar(position = 'nudge', alpha = 0.7) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  tbl <- prop.table(table(df[[col1]], df[[col2]]), margin = 2)
  
  smry <- summary(df[[col2]])
  
  # percent of whole
  pcnt <- df %>% 
    count(df[[col2]]) %>%
    mutate(pct = n / sum(n))%>%
    arrange(pct)
  #print(pcnt)
  
  plt2 <- ggplot(data = pcnt, aes(x=reorder(pcnt[,1],pct), y = pct))+
    geom_bar(stat = "identity") +
    coord_flip() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    #barplot(pcnt$pct,main="Percentage of whole", horiz=TRUE,
                  #names.arg=pcnt[,1])

 #fig <- gridExtra::grid.arrange(plt1, plt2, nrow = 1)
  
 print(plt1)
 print(plt2)
 print("Table:")
 print(tbl)
 print("")
 print("Summary:")
 print(smry)

}


# ALL variables D_INFLLUIR_Xxxx must be eliminated
d_influit_cols <- grep("^D_INFLUIT", names(accidents))
accidents <- accidents[, -d_influit_cols]

# Variable Any --> Out
accidents <- select(accidents, -Any)

# Variable dat --> Out
accidents <- select(accidents, -dat)

# Variable hor --> Out
accidents <- select(accidents, -hor)

# Variable via --> Out, because there are to many levels.
accidents <- select(accidents, -via)

# Variable pk --> Out
accidents <- select(accidents, -pk)

# Variable nomMun --> Out
accidents <- select(accidents, -nomMun)

# Variable nomCom --> Out
accidents <- select(accidents, -nomCom)

# Variable puntuacioAccident --> Out
accidents <- select(accidents, -puntuacioAccident)

# Variable F_MORTS --> Out
accidents <- select(accidents, -F_MORTS)

# Variable F_FERITS_GREUS --> Out
accidents <- select(accidents, -F_FERITS_GREUS)

# Variable F_FERITS_LLEUS --> Out
accidents <- select(accidents, -F_FERITS_LLEUS)

# Variable F_VICTIMES --> Out
accidents <- select(accidents, -F_VICTIMES)

# Variable F_UNITATS_IMPLICADES --> Out
accidents <- select(accidents, -F_UNITATS_IMPLICADES)

# Variable F_VIANANTS_IMPLICADES --> In
visual_inspection(accidents, "severitat", "F_VIANANTS_IMPLICADES")

#Transform discrete variable as just two factors.
accidents$F_VIANANTS_IMPLICADES <- ifelse(accidents$F_VIANANTS_IMPLICADES == 0 
                                          | accidents$F_VIANANTS_IMPLICADES == 1 
                                          | accidents$F_VIANANTS_IMPLICADES == 2 , "[0-2]", "[>2]")

accidents$F_VIANANTS_IMPLICADES <- as.factor(accidents$F_VIANANTS_IMPLICADES)

# Variable F_BICICLETES_IMPLICADES --> In
visual_inspection(accidents, "severitat", "F_BICICLETES_IMPLICADES")
#Transform discrete variable as just two factors.
accidents$F_BICICLETES_IMPLICADES <- ifelse(accidents$F_BICICLETES_IMPLICADES == 0 
                                            | accidents$F_BICICLETES_IMPLICADES == 1 
                                            | accidents$F_BICICLETES_IMPLICADES == 2 , "[0-2]", "[>2]")

accidents$F_BICICLETES_IMPLICADES <- as.factor(accidents$F_BICICLETES_IMPLICADES)

# Variable F_CICLOMOTORS_IMPLICADES --> In
visual_inspection(accidents, "severitat", "F_CICLOMOTORS_IMPLICADES")

# Transform discrete variable as just two factors.
accidents$F_CICLOMOTORS_IMPLICADES <- ifelse(accidents$F_CICLOMOTORS_IMPLICADES == 0, "[0]", "[>0]")
accidents$F_CICLOMOTORS_IMPLICADES <- as.factor(accidents$F_CICLOMOTORS_IMPLICADES)

# Variable F_MOTOCICLETES_IMPLICADES --> In
visual_inspection(accidents, "severitat", "F_MOTOCICLETES_IMPLICADES")

ggplot(data = accidents, aes(fill = severitat)) +
  geom_bar(aes(x = F_MOTOCICLETES_IMPLICADES), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

prop.table(table(accidents$severitat, accidents$F_MOTOCICLETES_IMPLICADES), margin = 2)
# Transform discrete variable as just two factors.
accidents$F_MOTOCICLETES_IMPLICADES <- ifelse(accidents$F_MOTOCICLETES_IMPLICADES == 0 
                                              | accidents$F_MOTOCICLETES_IMPLICADES == 1 
                                              | accidents$F_MOTOCICLETES_IMPLICADES == 2 , "[0-2]", "[>2]")
accidents$F_MOTOCICLETES_IMPLICADES <- as.factor(accidents$F_MOTOCICLETES_IMPLICADES)

# Variable F_VEH_LLEUGERS_IMPLICADES --> In
visual_inspection(accidents, "severitat", "F_VEH_LLEUGERS_IMPLICADES")
ggplot(data = accidents, aes(fill = severitat)) +
  geom_bar(aes(x = F_VEH_LLEUGERS_IMPLICADES), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

prop.table(table(accidents$severitat, accidents$F_VEH_LLEUGERS_IMPLICADES), margin = 2)
# Transform discrete variable as just two factors.
accidents$F_VEH_LLEUGERS_IMPLICADES <- ifelse(accidents$F_VEH_LLEUGERS_IMPLICADES == 0 
                                              | accidents$F_VEH_LLEUGERS_IMPLICADES == 1, "[0-1]", "[>1]")

accidents$F_VEH_LLEUGERS_IMPLICADES <- as.factor(accidents$F_VEH_LLEUGERS_IMPLICADES)

# Variable F_VEH_PESANTS_IMPLICADES --> In
visual_inspection(accidents, "severitat", "F_VEH_PESANTS_IMPLICADES")
# Transform discrete variable as just two factors.
accidents$F_VEH_PESANTS_IMPLICADES <- ifelse(accidents$F_VEH_PESANTS_IMPLICADES == "0", "[0]", "[>0]")
accidents$F_VEH_PESANTS_IMPLICADES <- as.factor(accidents$F_VEH_PESANTS_IMPLICADES)

# Variable F_ALTRES_UNIT_IMPLICADES --> Out
visual_inspection(accidents, "severitat", "F_ALTRES_UNIT_IMPLICADES")

accidents <- select(accidents, -F_ALTRES_UNIT_IMPLICADES)

# Variable F_UNIT_DESC_IMPLICADES --> Out
visual_inspection(accidents, "severitat", "F_UNIT_DESC_IMPLICADES")

accidents <- select(accidents, -F_UNIT_DESC_IMPLICADES)

# Variable D_ACC_AMB_FUGA --> Out
visual_inspection(accidents, "severitat", "D_ACC_AMB_FUGA")

accidents <- select(accidents, -D_ACC_AMB_FUGA)

# Variable D_BOIRA --> Out
visual_inspection(accidents, "severitat", "D_BOIRA")

accidents <- select(accidents, -D_BOIRA)

# Variable D_CARACT_ENTORN --> Out
visual_inspection(accidents, "severitat", "D_CARACT_ENTORN")

accidents <- select(accidents, -D_CARACT_ENTORN)

# Variable D_CARRIL_ESPECIAL --> Out
visual_inspection(accidents, "severitat", "D_CARRIL_ESPECIAL")

accidents <- select(accidents, -D_CARRIL_ESPECIAL)

# Variable D_CIRCULACIO_MESURES_ESP --> Out
visual_inspection(accidents, "severitat", "D_CIRCULACIO_MESURES_ESP")

accidents <- select(accidents, -D_CIRCULACIO_MESURES_ESP)

# Variable D_CLIMATOLOGIA --> In 
visual_inspection(accidents, "severitat", "D_CLIMATOLOGIA")

# Loads of cases, we eliminate 4 classes
accidents$D_CLIMATOLOGIA <- as.character(accidents$D_CLIMATOLOGIA)
accidents$D_CLIMATOLOGIA <- ifelse(accidents$D_CLIMATOLOGIA == "Pluja dèbil" 
                                   | accidents$D_CLIMATOLOGIA == "Pluja forta", "Pluja", accidents$D_CLIMATOLOGIA)

accidents <- filter(accidents, D_CLIMATOLOGIA != "Sense especificar")

accidents$D_CLIMATOLOGIA <- as.factor(accidents$D_CLIMATOLOGIA)

# Variable D_FUNC_ESP_VIA --> Out
visual_inspection(accidents, "severitat", "D_FUNC_ESP_VIA")

accidents <- select(accidents, -D_FUNC_ESP_VIA)

# Variable D_GRAVETAT --> Out 
visual_inspection(accidents, "severitat", "D_GRAVETAT")

accidents <- select(accidents, -D_GRAVETAT)

# Variable D_INTER_SECCIO --> In
visual_inspection(accidents, "severitat", "D_INTER_SECCIO")

# Variable D_LIMIT_VELOCITAT --> Out
visual_inspection(accidents, "severitat", "D_LIMIT_VELOCITAT")

accidents <- select(accidents, -D_LIMIT_VELOCITAT)

# Variable D_LLUMINOSITAT --> In. 
visual_inspection(accidents, "severitat", "D_LLUMINOSITAT")
#Transformation to just 2 factors
accidents$D_LLUMINOSITAT <- as.character(accidents$D_LLUMINOSITAT)

accidents$D_LLUMINOSITAT <- ifelse(accidents$D_LLUMINOSITAT == "De dia, dia clar" 
                                   | accidents$D_LLUMINOSITAT == "De nit, il·luminació artificial suficient", "Il·luminació alta", accidents$D_LLUMINOSITAT)

accidents$D_LLUMINOSITAT <- ifelse(accidents$D_LLUMINOSITAT == "Alba o capvespre" 
                                   | accidents$D_LLUMINOSITAT == "De dia, dia fosc"
                                   | accidents$D_LLUMINOSITAT == "De nit, il·luminació artificial insuficient", "Il·luminació baixa", accidents$D_LLUMINOSITAT)

accidents$D_LLUMINOSITAT <- ifelse(accidents$D_LLUMINOSITAT == "De nit, sense llum artificial", "Il·luminació nul·la", accidents$D_LLUMINOSITAT)
#erase unpecified data
accidents <- filter(accidents, D_LLUMINOSITAT != "Sense especificar")
accidents$D_LLUMINOSITAT <- as.factor(accidents$D_LLUMINOSITAT)

# Variable D_REGULACIO_PRIORITAT --> Out
visual_inspection(accidents, "severitat", "D_REGULACIO_PRIORITAT")
#loads of NA
accidents <- select(accidents, -D_REGULACIO_PRIORITAT)

# Variable D_SENTITS_VIA --> Out
visual_inspection(accidents, "severitat", "D_SENTITS_VIA")
#loads of NA
accidents <- select(accidents, -D_SENTITS_VIA)

# Variable D_SUBTIPUS_ACCIDENT --> Out
visual_inspection(accidents, "severitat", "D_SUBTIPUS_ACCIDENT")
#Loads of subclasses, no big differences
accidents <- select(accidents, -D_SUBTIPUS_ACCIDENT)

# Variable D_SUBTIPUS_TRAM --> Out
visual_inspection(accidents, "severitat", "D_SUBTIPUS_TRAM")
#Loads of subclasses, no big differences
accidents <- select(accidents, -D_SUBTIPUS_TRAM)

# Variable D_SUBZONA --> In
#correlates with zone and v_carretera, we can eliminate both then.
visual_inspection(accidents, "severitat", "D_SUBZONA")

# Variable zona --> Out, subZona available 
visual_inspection(accidents, "severitat", "zona")

accidents <- select(accidents, -zona)

# Variable C_VELOCITAT_VIA --> Out
visual_inspection(accidents, "severitat", "C_VELOCITAT_VIA")

accidents <- select(accidents, -C_VELOCITAT_VIA)

# Variable nomDem --> In
visual_inspection(accidents, "severitat", "nomDem")

# Variable D_SUPERFICIE --> Out
visual_inspection(accidents, "severitat", "D_SUPERFICIE")
#COULD KEEP IT? AS TWO FACTORS? --> No big difference in probablity --> Out
accidents <- select(accidents, -D_SUPERFICIE)

# Variable D_TIPUS_VIA --> In
visual_inspection(accidents, "severitat", "D_TIPUS_VIA")
#Keep and transform autovia & autopista into same variable
accidents$D_TIPUS_VIA <- as.character(accidents$D_TIPUS_VIA)

accidents$D_TIPUS_VIA <- ifelse(accidents$D_TIPUS_VIA == "Autopista" 
                                | accidents$D_TIPUS_VIA == "Autovia", "Auto", accidents$D_TIPUS_VIA)

accidents$D_TIPUS_VIA <- as.factor(accidents$D_TIPUS_VIA)

# Variable D_TITULARITAT_VIA --> Out
visual_inspection(accidents, "severitat", "D_TITULARITAT_VIA")
#not much sense
accidents <- select(accidents, -D_TITULARITAT_VIA)

# Variable D_TRACAT_ALIMETRIC --> Out due an unknown function error
#visual_inspection(accidents, "severitat", "D_TRACAT_ALIMETRIC")

accidents <- select(accidents, -D_TRACAT_ALTIMETRIC)

# Variable D_VENT --> In
visual_inspection(accidents, "severitat", "D_VENT")

# Variable grupDiaLab --> In
visual_inspection(accidents, "severitat", "grupDiaLab")

# Variable grupHor --> In
visual_inspection(accidents, "severitat", "grupHor")

accidents$grupHor <- as.character(accidents$grupHor)
#transform into just dia & nit
accidents$grupHor <- ifelse(accidents$grupHor == "Matí" 
                            | accidents$grupHor == "Tarda", "Dia", accidents$grupHor)

accidents$grupHor <- as.factor(accidents$grupHor)

# Variable tipAcc --> In 
visual_inspection(accidents, "severitat", "tipAcc")

#transform into less variables
accidents$tipAcc <- as.character(accidents$tipAcc)
accidents$tipAcc <- ifelse(accidents$tipAcc == "Col.lisió d'un vehicle contra un obstacle de la calcada" 
                           | accidents$tipAcc == "Col.lisió de vehicles en marxa", "Col.lisió", accidents$tipAcc)
accidents$tipAcc <- as.factor(accidents$tipAcc)

# Variable tipDia --> Out
visual_inspection(accidents, "severitat", "tipDia")

accidents <- select(accidents, -tipDia)




################################################################################
# -------------------- CREATE DATASETS ----------------------------------------#
set.seed(1964)

# lets split the data into "train data" & "test data"
trainSize = round(dim(accidents)[1]*0.7, 0)
train = sample(1:dim(accidents)[1], trainSize)
test = -train

accidents_train = accidents[train, ]
accidents_test = accidents[test, ]

accidents_train <- na.omit(accidents_train)
accidents_test <- na.omit(accidents_test)
# Look for similar proportion of severe accidents in both datasets
prop.table(table(accidents_train$severitat))
prop.table(table(accidents_test$severitat))
summary(accidents_train)
summary(accidents_test)

################################################################################
# -------------------- EXPLORATORI ANALYSIS -----------------------------------#

# a brief summary of the highest value of each column
show_plot(inspect_imb(accidents_train))

cols <- colnames(select(accidents_train, -severitat))
for (i in 1:length(cols)) {
  print(cols[i])
   visual_inspection(accidents, "severitat", i)
  
}

################################################################################
# ----------------- LOGISTIC REGRESSION MODELS --------------------------------#


var_with_min_p_value <- function(mod) {
  # Funtion to get the variable with the highest p_value of the regression
  # Also prints a summary of the model

    # Show results
  smry <- summary(mod)
  
  
  print("Model:")
  print(smry)
  print("---------")
  cat(" Model AIC is:", extractAIC(mod)[2])
  print("---------")
  # Extract the p-values for each variable
  p_values <- smry$coefficients[, "Pr(>|z|)"]
  max(p_values)
  print(which.max(p_values))
  # Identify the variable with the highest p-value
  variable_with_highest_pvalue <- names(p_values)[which.max(p_values)]

  # Print the variable with the highest p-value
  cat("Variable with the highest p-value:", variable_with_highest_pvalue, "\n")
  cat("Value:", p_values, "\n") 
  
}

#--- Mod Full------------------------------
# The first models we propose is the model with all the variables
modFull <- glm(severitat ~ ., family = 'binomial', accidents_train)

var_with_min_p_value(modFull)
# Althouh D_TIPUS_VIA has a variable that is non significant some of other variables are significant, so
# D_Vent is the variable with all its subvariables with the higest pvalue so it's removed

#--- Mod Null------------------------------
# We also can make a Null model to make sure a Logistic regression adds some 
# value 
modNull <- glm(severitat ~ 1, family = 'binomial', accidents_train)

var_with_min_p_value(modNull)


# Next step, get rid of the variable with the worst p-value and analise AIC
#--- Mod16------------------------------
mod16 <- glm(severitat ~ nomDem 
             + F_VIANANTS_IMPLICADES 
             + F_BICICLETES_IMPLICADES 
             + F_CICLOMOTORS_IMPLICADES 
             + F_MOTOCICLETES_IMPLICADES
             + F_VEH_LLEUGERS_IMPLICADES 
             + F_VEH_PESANTS_IMPLICADES
             + D_CLIMATOLOGIA
             + D_INTER_SECCIO
             + D_LLUMINOSITAT
             + D_SUBZONA
             + D_TIPUS_VIA
             + grupDiaLab
             + grupHor 
             + tipAcc
             + mes, family = 'binomial', accidents_train)
var_with_min_p_value(mod16)
# Now D_TIPUS_VIA is chosen to go out because the pvalues are still the highest. 

#--- Mod15------------------------------
mod15 <- glm(severitat ~ nomDem 
             + F_VIANANTS_IMPLICADES 
             + F_BICICLETES_IMPLICADES 
             + F_CICLOMOTORS_IMPLICADES 
             + F_MOTOCICLETES_IMPLICADES
             + F_VEH_LLEUGERS_IMPLICADES 
             + F_VEH_PESANTS_IMPLICADES
             + D_INTER_SECCIO
             + D_LLUMINOSITAT
             + D_SUBZONA
             + D_CLIMATOLOGIA
             + grupDiaLab
             + grupHor 
             + tipAcc
             + mes, family = 'binomial', accidents_train)
var_with_min_p_value(mod15)
#D_CLIMATOLOGIA is out because it has an overall bigger pvalue than other variables

#--- Mod14------------------------------
mod14 <- glm(severitat ~ nomDem 
             + F_VIANANTS_IMPLICADES 
             + F_BICICLETES_IMPLICADES 
             + F_CICLOMOTORS_IMPLICADES 
             + F_MOTOCICLETES_IMPLICADES
             + F_VEH_LLEUGERS_IMPLICADES 
             + F_VEH_PESANTS_IMPLICADES
             + D_INTER_SECCIO
             + D_LLUMINOSITAT
             + D_SUBZONA
             + grupDiaLab
             + grupHor 
             + tipAcc
             + mes, family = 'binomial', accidents_train)
var_with_min_p_value(mod14)
# in this case, the variable mes seems to be non significant to

#--- Mod13------------------------------
mod13 <- glm(severitat ~ nomDem 
             + F_VIANANTS_IMPLICADES 
             + F_BICICLETES_IMPLICADES 
             + F_CICLOMOTORS_IMPLICADES 
             + F_MOTOCICLETES_IMPLICADES
             + F_VEH_LLEUGERS_IMPLICADES 
             + F_VEH_PESANTS_IMPLICADES
             + D_INTER_SECCIO
             + D_LLUMINOSITAT
             + D_SUBZONA
             + grupDiaLab
             + grupHor 
             + tipAcc, family = 'binomial', accidents_train)
var_with_min_p_value(mod13)
# next variable is D_INTERSECCIO

#--- Mod12------------------------------
mod12 <- glm(severitat ~ nomDem 
             + F_VIANANTS_IMPLICADES 
             + F_BICICLETES_IMPLICADES 
             + F_CICLOMOTORS_IMPLICADES 
             + F_MOTOCICLETES_IMPLICADES
             + F_VEH_LLEUGERS_IMPLICADES 
             + F_VEH_PESANTS_IMPLICADES
             + D_LLUMINOSITAT
             + D_SUBZONA
             + grupDiaLab
             + grupHor 
             + tipAcc, family = 'binomial', accidents_train)
var_with_min_p_value(mod12)
# AIC has increased, so taking out D_INTERSECCIO hasn't improved resuts.
# Instead we are tryig to take out tipAcc to see If we get something:

mod12b <- glm(severitat ~ nomDem 
             + F_VIANANTS_IMPLICADES 
             + F_BICICLETES_IMPLICADES 
             + F_CICLOMOTORS_IMPLICADES 
             + F_MOTOCICLETES_IMPLICADES
             + F_VEH_LLEUGERS_IMPLICADES 
             + F_VEH_PESANTS_IMPLICADES
             + D_INTER_SECCIO
             + D_LLUMINOSITAT
             + D_SUBZONA
             + grupDiaLab
             + grupHor, family = 'binomial', accidents_train)
var_with_min_p_value(mod12b)
# Looks even worse, so  the proposal is rejected

# On mod13, all other variables are significant. Thus the best model it's the model13. 


# Test Anova --------------------------------------------------------------

anova(modFull, mod16, mod15, mod14, mod13, mod12,mod12b, modNull,test = 'Chisq')
# L'unic pvalor proper a 0 de la taula es el del mod12, de manera que el mod12 es significativament pitjor que els altres. 
# En canvi, entre el mod17, mod16, mod15, mod14 i mod13 no hi ha diferencies quant a deviancia (el pvalor no es proper a 0). Per tant, escollim el mod13, ja que es bo i te menys regressors.
sapply(list(modFull, mod16, mod15, mod14, mod13, mod12,mod12b, modNull), extractAIC)
# min value is the one from mod13!

# Model definitiu (mod13). Passem a anomenar-lo mod -----------------------

modLog <- glm(severitat ~ nomDem 
             + F_VIANANTS_IMPLICADES 
             + F_BICICLETES_IMPLICADES 
             + F_CICLOMOTORS_IMPLICADES 
             + F_MOTOCICLETES_IMPLICADES
             + F_VEH_LLEUGERS_IMPLICADES 
             + F_VEH_PESANTS_IMPLICADES
             + D_INTER_SECCIO
             + D_LLUMINOSITAT
             + D_SUBZONA
             + grupDiaLab
             + grupHor 
             + tipAcc, family = 'binomial', accidents_train)
summary(modLog) 


#------------- Validation of model residuals ------------------------------------#

par(mfrow = c(2, 2))
plot(modLog, ask = F)
par(mfrow = c(1, 1))

ggplot(data.frame(res = residuals.glm(modLog, type = 'deviance'),
                  pred = predict.glm(modLog, type = 'response')),
       aes(x = pred, y = res)) +
  geom_point() +
  labs(x = 'fitted response values', y = 'deviance residuals')



# AUC ---------------------------------------------------------------------
library(randomForest)
library(pls)
library(caret)
library(ROCR)
library(vcdExtra)
predMod <- prediction(predictions = predict(modLog, newdata = accidents_test, type = 'response'),
                      labels = accidents_test$severitat)

performance(prediction.obj = predMod, measure = 'auc')@y.values # L'AUC del classificador te un valor de 0.7366847.



# Corba ROC: Escollim el threshold ----------------------------------------
rocMod <- performance(prediction.obj = predMod, 
                      measure = 'tpr', 
                      x.measure = 'fpr')

rocMod <- data.frame(x.values = rocMod@x.values[[1]],
                     y.values = rocMod@y.values[[1]],
                     cutoff = rocMod@alpha.values[[1]],
                     model = 'model')

thresholds <- seq(0.1,0.9,0.1)
cutoff <- rep(NA,length(thresholds))
for (i in 1:(length(thresholds)))
  cutoff[i] = which.min(abs(rocMod$cutoff - thresholds[i]))

ggplot(data = rocMod, aes(x = x.values, y = y.values)) +
  geom_step(direction = 'vh', col = 'tomato', size = 1) + 
  geom_point(data = rocMod[cutoff,], aes(x = x.values, y = y.values),col = 'tomato', size=2) +
  geom_text(data = rocMod[cutoff,], aes(x = x.values, y = y.values, label = thresholds), nudge_y = 0.03,
            col = 'blue') +
  labs(x = '1-specificity (False Positive Rate: FPR)', y = 'sensitivity (True Positive Rate (TPR)',
       title = 'ROC curve with thresholds')  +
  scale_x_continuous(breaks=seq(0,1,0.2)) +
  scale_y_continuous(breaks=seq(0,1,0.2))
# Escollim 0.1 com a threshold, ja que volem tenir una sensibilimodLogtat elevada.

# Taula de confusio -------------------------------------------------------
pred <- predict(modLog, newdata = accidents_test, type = 'response')
table(accidents_test$severitat, pred > 0.15, dnn = c('actual', 'predicted'))

# Sensitivity = TP/(FN+TP) ------------------------------------------------
sum(accidents_test$severitat == 1 & pred > 0.15)/sum(accidents_test$severitat == 1)

# Specificity = TN/(TN+FP) ------------------------------------------------
sum(accidents_test$severitat == 0 & pred < 0.15)/sum(accidents_test$severitat == 0)



################################################################################
# -------------------- TREE MODELS --------------------------------------------#
library(tree) # to build trees
library(randomForest) # bagging and random forest
library(gbm) #boosting

baggedTrees <- randomForest(severitat ~ .,  
                           data = accidents_train,
                           ntree = 1009,
                           nodesize = 5,
                           mtry = ncol(accidents_train)-1,
                           replace = TRUE,
                           keep.forest = TRUE,
                           xtest = accidents_test[, cols],
                           ytest = accidents_test$severitat,
                           do.trace = 1)

plot(x = 1:length(baggedTrees$test$err.rate[,'Test']), y = baggedTrees$test$err.rate[,'Test'], type = 'l', ylim = c( 0,0.5))
lines(x=1:length(baggedTrees$test$err.rate[,'Test']), y = baggedTrees$err.rate[,'OOB'], col = 'red')
# plot errors
par(mfrow = c(1,1))
plot(baggedTrees$test$mse, type = 'b', pch = 16, xlab = 'no trees', ylab = 'test MSE', 
     main = 'Test set MSE')







