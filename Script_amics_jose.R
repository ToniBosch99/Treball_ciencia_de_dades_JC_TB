# Llibreries --------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(ggplot2)
library(randomForest)
library(pls)
library(caret)
library(ROCR)
library(vcdExtra)



# Carrega de dades --------------------------------------------------------

accidents <- read.csv(file = 'Accidents_de_tr_nsit_amb_morts_o_ferits_greus_a_Catalunya.csv',
                      stringsAsFactors = T)



# Creacio de la resposta (i altres variables) -----------------------------

# Variable accidentScore --> Es crea

accidents <- mutate(accidents, accidentScore = 9 * F_MORTS + 3 * F_FERITS_GREUS + 1 * F_FERITS_LLEUS)

# Variable severity --> Es crea

accidents <- mutate(accidents, severity = ifelse(accidentScore < 9, 0, 1))
accidents$severity <- as.factor(accidents$severity)

# Variable mes --> Es crea

accidents$mes <- format(as.Date(accidents$dat), "%B")
accidents$mes <- as.factor(accidents$mes)



# Training Set i Test Set -------------------------------------------------

set.seed(2023)
split <- accidents$severity %>%
  createDataPartition(p = 0.7, list = FALSE)  
train.data  <- accidents[split, ]
test.data <- accidents[-split, ]

# Comprovem que hi hagi aproximadament la mateixa proporcio d'accidents severs i molt severs en el training set i en el test set

prop.table(table(train.data$severity))
prop.table(table(test.data$severity))



# Configuracio de les dades -----------------------------------------------

# Variable accidentScore --> S'elimina

accidents <- select(accidents, -accidentScore)

# Variable Any --> S'elimina

accidents <- select(accidents, -Any)

# Variable zona --> S'elimina

accidents <- select(accidents, -zona)

# Variable dat --> S'elimina

accidents <- select(accidents, -dat)

# Variable via --> S'elimina

accidents <- select(accidents, -via)

# Variable pk --> S'elimina

accidents <- select(accidents, -pk)

# Variable nomMun --> S'elimina

accidents <- select(accidents, -nomMun)

# Variable nomCom --> S'elimina

accidents <- select(accidents, -nomCom)

# Variable nomDem --> Es mante

ggplot(data = train.data, aes(fill = severity)) +
  geom_bar(aes(x = nomDem), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

prop.table(table(train.data$severity, train.data$nomDem), margin = 2)

# Variable F_MORTS --> S'elimina

accidents <- select(accidents, -F_MORTS)

# Variable F_FERITS_GREUS --> S'elimina

accidents <- select(accidents, -F_FERITS_GREUS)

# Variable F_FERITS_LLEUS --> S'elimina

accidents <- select(accidents, -F_FERITS_LLEUS)

# Variable F_VICTIMES --> S'elimina

accidents <- select(accidents, -F_VICTIMES)

# Variable F_UNITATS_IMPLICADES --> S'elimina

accidents <- select(accidents, -F_UNITATS_IMPLICADES)

# Variable F_VIANANTS_IMPLICADES --> Es mante i se'n fan agrupacions

ggplot(data = train.data, aes(fill = severity)) +
  geom_bar(aes(x = F_VIANANTS_IMPLICADES), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

prop.table(table(train.data$severity, train.data$F_VIANANTS_IMPLICADES), margin = 2)

accidents$F_VIANANTS_IMPLICADES <- ifelse(accidents$F_VIANANTS_IMPLICADES == 0 
                                          | accidents$F_VIANANTS_IMPLICADES == 1 
                                          | accidents$F_VIANANTS_IMPLICADES == 2 , "[0-2]", "[>2]")

accidents$F_VIANANTS_IMPLICADES <- as.factor(accidents$F_VIANANTS_IMPLICADES)

# Variable F_BICICLETES_IMPLICADES --> Es mante i se'n fan agrupacions

ggplot(data = train.data, aes(fill = severity)) +
  geom_bar(aes(x = F_BICICLETES_IMPLICADES), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

prop.table(table(train.data$severity, train.data$F_BICICLETES_IMPLICADES), margin = 2)

accidents$F_BICICLETES_IMPLICADES <- ifelse(accidents$F_BICICLETES_IMPLICADES == 0 
                                            | accidents$F_BICICLETES_IMPLICADES == 1 
                                            | accidents$F_BICICLETES_IMPLICADES == 2 , "[0-2]", "[>2]")

accidents$F_BICICLETES_IMPLICADES <- as.factor(accidents$F_BICICLETES_IMPLICADES)

# Variable F_CICLOMOTORS_IMPLICADES --> Es mante i se'n fan agrupacions

ggplot(data = train.data, aes(fill = severity)) +
  geom_bar(aes(x = F_CICLOMOTORS_IMPLICADES), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

prop.table(table(train.data$severity, train.data$F_CICLOMOTORS_IMPLICADES), margin = 2)

accidents$F_CICLOMOTORS_IMPLICADES <- ifelse(accidents$F_CICLOMOTORS_IMPLICADES == 0, "[0]", "[>0]")

accidents$F_CICLOMOTORS_IMPLICADES <- as.factor(accidents$F_CICLOMOTORS_IMPLICADES)

# Variable F_MOTOCICLETES_IMPLICADES --> Es mante i se'n fan agrupacions

ggplot(data = train.data, aes(fill = severity)) +
  geom_bar(aes(x = F_MOTOCICLETES_IMPLICADES), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

prop.table(table(train.data$severity, train.data$F_MOTOCICLETES_IMPLICADES), margin = 2)

accidents$F_MOTOCICLETES_IMPLICADES <- ifelse(accidents$F_MOTOCICLETES_IMPLICADES == 0 
                                              | accidents$F_MOTOCICLETES_IMPLICADES == 1 
                                              | accidents$F_MOTOCICLETES_IMPLICADES == 2 , "[0-2]", "[>2]")

accidents$F_MOTOCICLETES_IMPLICADES <- as.factor(accidents$F_MOTOCICLETES_IMPLICADES)

# Variable F_VEH_LLEUGERS_IMPLICADES --> Es mante i se'n fan agrupacions

ggplot(data = train.data, aes(fill = severity)) +
  geom_bar(aes(x = F_VEH_LLEUGERS_IMPLICADES), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

prop.table(table(train.data$severity, train.data$F_VEH_LLEUGERS_IMPLICADES), margin = 2)

accidents$F_VEH_LLEUGERS_IMPLICADES <- ifelse(accidents$F_VEH_LLEUGERS_IMPLICADES == 0 
                                              | accidents$F_VEH_LLEUGERS_IMPLICADES == 1, "[0-1]", "[>1]")

accidents$F_VEH_LLEUGERS_IMPLICADES <- as.factor(accidents$F_VEH_LLEUGERS_IMPLICADES)

# Variable F_VEH_PESANTS_IMPLICADES --> Es mante i se'n fan agrupacions

ggplot(data = train.data, aes(fill = severity)) +
  geom_bar(aes(x = F_VEH_PESANTS_IMPLICADES), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

prop.table(table(train.data$severity, train.data$F_VEH_PESANTS_IMPLICADES), margin = 2)

accidents$F_VEH_PESANTS_IMPLICADES <- ifelse(accidents$F_VEH_PESANTS_IMPLICADES == "0", "[0]", "[>0]")

accidents$F_VEH_PESANTS_IMPLICADES <- as.factor(accidents$F_VEH_PESANTS_IMPLICADES)

# Variable F_ALTRES_UNIT_IMPLICADES --> S'elimina

prop.table(table(train.data$severity, train.data$F_ALTRES_UNIT_IMPLICADES), margin = 2)

accidents <- select(accidents, -F_ALTRES_UNIT_IMPLICADES)

# Variable F_UNIT_DESC_IMPLICADES --> S'elimina

prop.table(table(train.data$severity, train.data$F_UNIT_DESC_IMPLICADES), margin = 2)

accidents <- select(accidents, -F_UNIT_DESC_IMPLICADES)

# Variable C_VELOCITAT_VIA --> S'elimina

summary(train.data$C_VELOCITAT_VIA)

prop.table(table(train.data$severity, train.data$C_VELOCITAT_VIA), margin = 2)
prop.table(table(train.data$severity, train.data$D_SUBZONA), margin = 2)

accidents <- select(accidents, -C_VELOCITAT_VIA)

# Variable D_ACC_AMB_FUGA --> S'elimina

summary(train.data$D_ACC_AMB_FUGA)

prop.table(table(train.data$severity, train.data$D_ACC_AMB_FUGA), margin = 2)

accidents <- select(accidents, -D_ACC_AMB_FUGA)

# Variable D_BOIRA --> S'elimina

summary(train.data$D_BOIRA)

prop.table(table(train.data$severity, train.data$D_BOIRA), margin = 2)

accidents <- select(accidents, -D_BOIRA)

# Variable D_CARACT_ENTORN --> S'elimina

summary(train.data$D_CARACT_ENTORN)

accidents <- select(accidents, -D_CARACT_ENTORN)

# Variable D_CARRIL_ESPECIAL --> S'elimina

summary(train.data$D_CARRIL_ESPECIAL)

accidents <- select(accidents, -D_CARRIL_ESPECIAL)

# Variable D_CIRCULACIO_MESURES_ESP --> S'elimina

summary(train.data$D_CIRCULACIO_MESURES_ESP)

accidents <- select(accidents, -D_CIRCULACIO_MESURES_ESP)

# Variable D_CLIMATOLOGIA --> Es mante i se'n fan agrupacions. S'eliminen 4 files

ggplot(data = train.data, aes(fill = severity)) +
  geom_bar(aes(x = D_CLIMATOLOGIA), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

prop.table(table(train.data$severity, train.data$D_CLIMATOLOGIA), margin = 2)

summary(train.data$D_CLIMATOLOGIA)

accidents$D_CLIMATOLOGIA <- as.character(accidents$D_CLIMATOLOGIA)

accidents$D_CLIMATOLOGIA <- ifelse(accidents$D_CLIMATOLOGIA == "Pluja dèbil" 
                                   | accidents$D_CLIMATOLOGIA == "Pluja forta", "Pluja", accidents$D_CLIMATOLOGIA)

accidents <- filter(accidents, D_CLIMATOLOGIA != "Sense especificar")

accidents$D_CLIMATOLOGIA <- as.factor(accidents$D_CLIMATOLOGIA)

# Variable D_FUNC_ESP_VIA --> S'elimina

summary(train.data$D_FUNC_ESP_VIA)

prop.table(table(train.data$severity, train.data$D_FUNC_ESP_VIA), margin = 2)

accidents <- select(accidents, -D_FUNC_ESP_VIA)

# Variable D_GRAVETAT --> S'elimina

accidents <- select(accidents, -D_GRAVETAT)

# Variable D_INFLUIT_BOIRA --> S'elimina

accidents <- select(accidents, -D_INFLUIT_BOIRA)

# Variable D_INFLUIT_CARACT_ENTORN --> S'elimina

accidents <- select(accidents, -D_INFLUIT_CARACT_ENTORN)

# Variable D_INFLUIT_CIRCULACIO --> S'elimina

accidents <- select(accidents, -D_INFLUIT_CIRCULACIO)

# Variable D_INFLUIT_ESTAT_CLIMA --> S'elimina

accidents <- select(accidents, -D_INFLUIT_ESTAT_CLIMA)

# Variable D_INFLUIT_INTEN_VENT --> S'elimina

accidents <- select(accidents, -D_INFLUIT_INTEN_VENT)

# Variable D_INFLUIT_LLUMINOSITAT --> S'elimina

accidents <- select(accidents, -D_INFLUIT_LLUMINOSITAT)

# Variable D_INFLUIT_MESU_ESP --> S'elimina

accidents <- select(accidents, -D_INFLUIT_MESU_ESP)

# Variable D_INFLUIT_OBJ_CALCADA --> S'elimina

accidents <- select(accidents, -D_INFLUIT_OBJ_CALCADA)

# Variable D_INFLUIT_SOLCS_RASES --> S'elimina

accidents <- select(accidents, -D_INFLUIT_SOLCS_RASES)

# Variable D_INFLUIT_VISIBILITAT --> S'elimina

accidents <- select(accidents, -D_INFLUIT_VISIBILITAT)

# Variable D_INTER_SECCIO --> Es mante

ggplot(data = train.data, aes(fill = severity)) +
  geom_bar(aes(x = D_INTER_SECCIO), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

prop.table(table(train.data$severity, train.data$D_INTER_SECCIO), margin = 2)

# Variable D_LIMIT_VELOCITAT --> S'elimina

accidents <- select(accidents, -D_LIMIT_VELOCITAT)

# Variable D_LLUMINOSITAT --> Es mante i se'n fan agrupacions. S'eliminen 2 files

ggplot(data = train.data, aes(fill = severity)) +
  geom_bar(aes(x = D_LLUMINOSITAT), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

prop.table(table(train.data$severity, train.data$D_LLUMINOSITAT), margin = 2)

summary(train.data$D_LLUMINOSITAT)

accidents$D_LLUMINOSITAT <- as.character(accidents$D_LLUMINOSITAT)

accidents$D_LLUMINOSITAT <- ifelse(accidents$D_LLUMINOSITAT == "De dia, dia clar" 
                                   | accidents$D_LLUMINOSITAT == "De nit, il·luminació artificial suficient", "Il·luminació alta", accidents$D_LLUMINOSITAT)

accidents$D_LLUMINOSITAT <- ifelse(accidents$D_LLUMINOSITAT == "Alba o capvespre" 
                                   | accidents$D_LLUMINOSITAT == "De dia, dia fosc"
                                   | accidents$D_LLUMINOSITAT == "De nit, il·luminació artificial insuficient", "Il·luminació baixa", accidents$D_LLUMINOSITAT)

accidents$D_LLUMINOSITAT <- ifelse(accidents$D_LLUMINOSITAT == "De nit, sense llum artificial", "Il·luminació nul·la", accidents$D_LLUMINOSITAT)

accidents <- filter(accidents, D_LLUMINOSITAT != "Sense especificar")

accidents$D_LLUMINOSITAT <- as.factor(accidents$D_LLUMINOSITAT)

# Variable D_REGULACIO_PRIORITAT --> S'elimina

summary(train.data$D_REGULACIO_PRIORITAT)

prop.table(table(train.data$severity, train.data$D_REGULACIO_PRIORITAT), margin = 2)

accidents <- select(accidents, -D_REGULACIO_PRIORITAT)

# Variable D_SENTITS_VIA --> S'elimina

summary(train.data$D_SENTITS_VIA)

accidents <- select(accidents, -D_SENTITS_VIA)

# Variable D_SUBTIPUS_ACCIDENT --> S'elimina

summary(train.data$D_SUBTIPUS_ACCIDENT)

accidents <- select(accidents, -D_SUBTIPUS_ACCIDENT)

# Variable D_SUBTIPUS_TRAM --> S'elimina

summary(train.data$D_SUBTIPUS_TRAM)

accidents <- select(accidents, -D_SUBTIPUS_TRAM)

# Variable D_SUBZONA --> Es mante

ggplot(data = train.data, aes(fill = severity)) +
  geom_bar(aes(x = D_SUBZONA), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

prop.table(table(train.data$severity, train.data$D_SUBZONA), margin = 2)

# Variable D_SUPERFICIE --> S'elimina

summary(train.data$D_SUPERFICIE)

prop.table(table(train.data$severity, train.data$D_SUPERFICIE), margin = 2)

accidents <- select(accidents, -D_SUPERFICIE)

# Variable D_TIPUS_VIA --> Es mante i se'n fan agrupacions

ggplot(data = train.data, aes(fill = severity)) +
  geom_bar(aes(x = D_TIPUS_VIA), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

prop.table(table(train.data$severity, train.data$D_TIPUS_VIA), margin = 2)

summary(train.data$D_TIPUS_VIA)

accidents$D_TIPUS_VIA <- as.character(accidents$D_TIPUS_VIA)

accidents$D_TIPUS_VIA <- ifelse(accidents$D_TIPUS_VIA == "Autopista" 
                                | accidents$D_TIPUS_VIA == "Autovia", "Auto", accidents$D_TIPUS_VIA)

accidents$D_TIPUS_VIA <- as.factor(accidents$D_TIPUS_VIA)

# Variable D_TITULARITAT_VIA --> S'elimina

summary(train.data$D_TITULARITAT_VIA)

accidents <- select(accidents, -D_TITULARITAT_VIA)

# Variable D_TRACAT_ALIMETRIC --> S'elimina

summary(train.data$D_TRACAT_ALTIMETRIC)

accidents <- select(accidents, -D_TRACAT_ALTIMETRIC)

# Variable D_VENT --> Es mante

ggplot(data = train.data, aes(fill = severity)) +
  geom_bar(aes(x = D_VENT), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

prop.table(table(train.data$severity, train.data$D_VENT), margin = 2)

# Variable grupDiaLab --> Es mante

ggplot(data = train.data, aes(fill = severity)) +
  geom_bar(aes(x = grupDiaLab), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

prop.table(table(train.data$severity, train.data$grupDiaLab), margin = 2)

# Variable hor --> S'elimina

accidents <- select(accidents, -hor)

# Variable grupHor --> Es mante i se'n fan agrupacions

ggplot(data = train.data, aes(fill = severity)) +
  geom_bar(aes(x = grupHor), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

prop.table(table(train.data$severity, train.data$grupHor), margin = 2)

summary(train.data$grupHor)

accidents$grupHor <- as.character(accidents$grupHor)

accidents$grupHor <- ifelse(accidents$grupHor == "Matí" 
                            | accidents$grupHor == "Tarda", "Dia", accidents$grupHor)

accidents$grupHor <- as.factor(accidents$grupHor)

# Variable tipAcc --> Es mante i se'n fan agrupacions 

ggplot(data = train.data, aes(fill = severity)) +
  geom_bar(aes(x = tipAcc), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

prop.table(table(train.data$severity, train.data$tipAcc), margin = 2)

summary(train.data$tipAcc)

accidents$tipAcc <- as.character(accidents$tipAcc)

accidents$tipAcc <- ifelse(accidents$tipAcc == "Col.lisió d'un vehicle contra un obstacle de la calcada" 
                           | accidents$tipAcc == "Col.lisió de vehicles en marxa", "Col.lisió", accidents$tipAcc)

accidents$tipAcc <- as.factor(accidents$tipAcc)

# Variable tipDia --> S'elimina

summary(train.data$tipDia)

accidents <- select(accidents, -tipDia)

# Comprovacio que no queden NA's

lapply(accidents, function(x) sum(is.na(x)))

# Canvi dels noms d'algunes variables

names(accidents)[names(accidents) == "F_VIANANTS_IMPLICADES"] <- "F_VIANANTS"
names(accidents)[names(accidents) == "F_BICICLETES_IMPLICADES"] <- "F_BICICLETES"
names(accidents)[names(accidents) == "F_CICLOMOTORS_IMPLICADES"] <- "F_CICLOMOTORS"
names(accidents)[names(accidents) == "F_MOTOCICLETES_IMPLICADES"] <- "F_MOTOCICLETES"
names(accidents)[names(accidents) == "F_VEH_LLEUGERS_IMPLICADES"] <- "F_VEH_LLEUGERS"
names(accidents)[names(accidents) == "F_VEH_PESANTS_IMPLICADES"] <- "F_VEH_PESANTS"



# Training Set i Test Set -------------------------------------------------

set.seed(2023)
split <- accidents$severity %>%
  createDataPartition(p = 0.7, list = FALSE)  
train.data  <- accidents[split, ]
test.data <- accidents[-split, ]

# Comprovem que hi hagi aproximadament la mateixa proporcio d'accidents severs i molt severs en el training set i en el test set

prop.table(table(train.data$severity))
prop.table(table(test.data$severity))



# Analisi exploratori -----------------------------------------------------

# Summary

summary(accidents)

summary(accidents)

library(inspectdf)
show_plot(inspect_imb(accidents))

# Proporcio de les observacions amb severity = 0 (accident sever) i severity = 1 (accident molt sever)

prop.table(table(accidents$severity)) #quin % d'accidents son molt severs? 

# Proporcio de les observacions amb severity = 0 (accident sever) i severity = 1 (accident molt sever) en funcio de grupDiaLab

prop.table(table(accidents$severity, accidents$grupDiaLab), margin = 2) #severity rate by grupdiaLab?
mosaicplot(table(accidents$grupDiaLab, accidents$severity), color = c('#F8766D', '#00BFC4'), ylab="severity",xlab="grupdiaLab")
ggplot(data = accidents, aes(fill = severity)) +
  geom_bar(aes(x = grupDiaLab), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Proporcio de les observacions amb severity = 0 (accident sever) i severity = 1 (accident molt sever) en funcio de grupHor
#=distribucio de casos by grupHor
prop.table(table(accidents$severity, accidents$grupHor), margin = 2) #severity rate by franja horaria (dia-nit)?
mosaicplot(table(accidents$grupHor, accidents$severity), color = c('#F8766D', '#00BFC4'), ylab="severity",xlab="grupHor")
ggplot(data = accidents, aes(fill = severity)) +
  geom_bar(aes(x = grupHor), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#MES
accidents$mes <- factor(accidents$mes,levels = c("gener", "febrer", "març", "abril","maig","juny","juliol","agost","setembre","octubre","novembre","desembre"))
prop.table(table(accidents$severity, accidents$mes), margin = 2) #severity rate by franja horaria (dia-nit)?
mosaicplot(table(accidents$mes, accidents$severity), color = c('#F8766D', '#00BFC4'), ylab="severity",xlab="mes")
ggplot(data = accidents, aes(fill = severity)) +
  geom_bar(aes(x = mes), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Histograma de les variables X

ggplot(data = accidents, aes(fill = severity)) +
  geom_bar(aes(x = nomDem), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = accidents, aes(fill = severity)) +
  geom_bar(aes(x = F_VIANANTS), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = accidents, aes(fill = severity)) +
  geom_bar(aes(x = F_BICICLETES), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = accidents, aes(fill = severity)) +
  geom_bar(aes(x = F_CICLOMOTORS), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = accidents, aes(fill = severity)) +
  geom_bar(aes(x = F_MOTOCICLETES), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = accidents, aes(fill = severity)) +
  geom_bar(aes(x = F_VEH_LLEUGERS), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = accidents, aes(fill = severity)) +
  geom_bar(aes(x = F_VEH_PESANTS), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = accidents, aes(fill = severity)) +
  geom_bar(aes(x = D_CLIMATOLOGIA), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = accidents, aes(fill = severity)) +
  geom_bar(aes(x = D_INTER_SECCIO), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = accidents, aes(fill = severity)) +
  geom_bar(aes(x = D_LLUMINOSITAT), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = accidents, aes(fill = severity)) +
  geom_bar(aes(x = D_SUBZONA), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = accidents, aes(fill = severity)) +
  geom_bar(aes(x = D_TIPUS_VIA), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = accidents, aes(fill = severity)) +
  geom_bar(aes(x = D_VENT), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = accidents, aes(fill = severity)) +
  geom_bar(aes(x = grupDiaLab), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = accidents, aes(fill = severity)) +
  geom_bar(aes(x = grupHor), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = accidents, aes(fill = severity)) +
  geom_bar(aes(x = tipAcc), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = accidents, aes(fill = severity)) +
  geom_bar(aes(x = mes), position = 'nudge', alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Regressio logistica -----------------------------------------------------

# Model amb 17 variables --------------------------------------------------

mod17 <- glm(severity ~ nomDem 
             + F_VIANANTS 
             + F_BICICLETES 
             + F_CICLOMOTORS 
             + F_MOTOCICLETES
             + F_VEH_LLEUGERS 
             + F_VEH_PESANTS
             + D_CLIMATOLOGIA
             + D_INTER_SECCIO
             + D_LLUMINOSITAT
             + D_SUBZONA
             + D_TIPUS_VIA
             + D_VENT
             + grupDiaLab
             + grupHor 
             + tipAcc
             + mes, family = 'binomial', train.data)
summary(mod17)

# Creem un diccionari amb el minim pvalor de cada categoria
diccionari <- list(nomDem = 0, 
                   F_VIANANTS = 0,
                   F_BICICLETES = 0,
                   F_CICLOMOTORS = 0,
                   F_MOTOCICLETES = 0,
                   F_VEH_LLEUGERS = 0,
                   F_VEH_PESANTS = 0,
                   D_CLIMATOLOGIA = 0,
                   D_INTER_SECCIO = 0,
                   D_LLUMINOSITAT = 0,
                   D_SUBZONA = 0,
                   D_TIPUS_VIA = 0,
                   D_VENT = 0,
                   grupDiaLab = 0,
                   grupHor = 0,
                   tipAcc = 0,
                   mes = 0)

for (i in 1:length(diccionari)) {
  p.valors <- summary(mod17)$coefficients[,4]
  vars <- grep(paste0("^", as.character(names(diccionari[i]))), names(p.valors))
  p.valors <- p.valors[vars]
  min.p.valors <- min(p.valors)
  diccionari[i] <- min.p.valors
}

# Extreiem les claus i els valors del diccionari
claus <- names(diccionari)
valors <- unlist(diccionari)

# Trobem la posicio amb el maxim valor
max.pos <- which.max(valors)

# Trobem la clau amb el maxim valor
max.clau <- claus[max.pos]

# Mostrem la clau amb el maxim valor
diccionari[max.clau]



# Model amb 16 variables (traiem D_VENT) ----------------------------------

mod16 <- glm(severity ~ nomDem 
             + F_VIANANTS 
             + F_BICICLETES 
             + F_CICLOMOTORS 
             + F_MOTOCICLETES
             + F_VEH_LLEUGERS 
             + F_VEH_PESANTS
             + D_CLIMATOLOGIA
             + D_INTER_SECCIO
             + D_LLUMINOSITAT
             + D_SUBZONA
             + D_TIPUS_VIA
             + grupDiaLab
             + grupHor 
             + tipAcc
             + mes, family = 'binomial', train.data)
summary(mod16) # L'AIC ha disminuit i la deviancia s'ha mantingut practicament constant. Per tant, treure D_VENT ha estat una bona decisio.

# Creem un diccionari amb el minim pvalor de cada categoria
diccionari <- list(nomDem = 0, 
                   F_VIANANTS = 0,
                   F_BICICLETES = 0,
                   F_CICLOMOTORS = 0,
                   F_MOTOCICLETES = 0,
                   F_VEH_LLEUGERS = 0,
                   F_VEH_PESANTS = 0,
                   D_CLIMATOLOGIA = 0,
                   D_INTER_SECCIO = 0,
                   D_LLUMINOSITAT = 0,
                   D_SUBZONA = 0,
                   D_TIPUS_VIA = 0,
                   grupDiaLab = 0,
                   grupHor = 0,
                   tipAcc = 0,
                   mes = 0)

for (i in 1:length(diccionari)) {
  p.valors <- summary(mod16)$coefficients[,4]
  vars <- grep(paste0("^", as.character(names(diccionari[i]))), names(p.valors))
  p.valors <- p.valors[vars]
  min.p.valors <- min(p.valors)
  diccionari[i] <- min.p.valors
}

# Extreiem les claus i els valors del diccionari
claus <- names(diccionari)
valors <- unlist(diccionari)

# Trobem la posicio amb el maxim valor
max.pos <- which.max(valors)

# Trobem la clau amb el maxim valor
max.clau <- claus[max.pos]

# Mostrem la clau amb el maxim valor
diccionari[max.clau]



# Model amb 15 variables (traiem D_CLIMATOLOGIA) --------------------------

mod15 <- glm(severity ~ nomDem 
             + F_VIANANTS 
             + F_BICICLETES 
             + F_CICLOMOTORS 
             + F_MOTOCICLETES
             + F_VEH_LLEUGERS 
             + F_VEH_PESANTS
             + D_INTER_SECCIO
             + D_LLUMINOSITAT
             + D_SUBZONA
             + D_TIPUS_VIA
             + grupDiaLab
             + grupHor 
             + tipAcc
             + mes, family = 'binomial', train.data)
summary(mod15) # L'AIC ha disminuit i la deviancia s'ha mantingut practicament constant. Per tant, treure D_CLIMATOLOGIA ha estat una bona decisio.

# Creem un diccionari amb el minim pvalor de cada categoria
diccionari <- list(nomDem = 0, 
                   F_VIANANTS = 0,
                   F_BICICLETES = 0,
                   F_CICLOMOTORS = 0,
                   F_MOTOCICLETES = 0,
                   F_VEH_LLEUGERS = 0,
                   F_VEH_PESANTS = 0,
                   D_INTER_SECCIO = 0,
                   D_LLUMINOSITAT = 0,
                   D_SUBZONA = 0,
                   D_TIPUS_VIA = 0,
                   grupDiaLab = 0,
                   grupHor = 0,
                   tipAcc = 0,
                   mes = 0)

for (i in 1:length(diccionari)) {
  p.valors <- summary(mod15)$coefficients[,4]
  vars <- grep(paste0("^", as.character(names(diccionari[i]))), names(p.valors))
  p.valors <- p.valors[vars]
  min.p.valors <- min(p.valors)
  diccionari[i] <- min.p.valors
}

# Extreiem les claus i els valors del diccionari
claus <- names(diccionari)
valors <- unlist(diccionari)

# Trobem la posicio amb el maxim valor
max.pos <- which.max(valors)

# Trobem la clau amb el maxim valor
max.clau <- claus[max.pos]

# Mostrem la clau amb el maxim valor
diccionari[max.clau]



# Model amb 14 variables (traiem mes) -------------------------------------

mod14 <- glm(severity ~ nomDem 
             + F_VIANANTS 
             + F_BICICLETES
             + F_CICLOMOTORS 
             + F_MOTOCICLETES
             + F_VEH_LLEUGERS 
             + F_VEH_PESANTS
             + D_INTER_SECCIO
             + D_LLUMINOSITAT
             + D_SUBZONA
             + D_TIPUS_VIA
             + grupDiaLab
             + grupHor 
             + tipAcc, family = 'binomial', train.data)
summary(mod14) # L'AIC ha disminuit i la deviancia s'ha mantingut practicament constant. Per tant, treure mes ha estat una bona decisio.

# Creem un diccionari amb el minim pvalor de cada categoria
diccionari <- list(nomDem = 0, 
                   F_VIANANTS = 0,
                   F_BICICLETES = 0,
                   F_CICLOMOTORS = 0,
                   F_MOTOCICLETES = 0,
                   F_VEH_LLEUGERS = 0,
                   F_VEH_PESANTS = 0,
                   D_INTER_SECCIO = 0,
                   D_LLUMINOSITAT = 0,
                   D_SUBZONA = 0,
                   D_TIPUS_VIA = 0,
                   grupDiaLab = 0,
                   grupHor = 0,
                   tipAcc = 0)

for (i in 1:length(diccionari)) {
  p.valors <- summary(mod14)$coefficients[,4]
  vars <- grep(paste0("^", as.character(names(diccionari[i]))), names(p.valors))
  p.valors <- p.valors[vars]
  min.p.valors <- min(p.valors)
  diccionari[i] <- min.p.valors
}

# Extreiem les claus i els valors del diccionari
claus <- names(diccionari)
valors <- unlist(diccionari)

# Trobem la posicio amb el maxim valor
max.pos <- which.max(valors)

# Trobem la clau amb el maxim valor
max.clau <- claus[max.pos]

# Mostrem la clau amb el maxim valor
diccionari[max.clau]



# Model amb 13 variables (traiem D_INTER_SECCIO) --------------------------

mod13 <- glm(severity ~ nomDem 
             + F_VIANANTS 
             + F_BICICLETES 
             + F_CICLOMOTORS 
             + F_MOTOCICLETES
             + F_VEH_LLEUGERS 
             + F_VEH_PESANTS
             + D_LLUMINOSITAT
             + D_SUBZONA
             + D_TIPUS_VIA
             + grupDiaLab
             + grupHor 
             + tipAcc, family = 'binomial', train.data)
summary(mod13) # L'AIC ha augmentat. Per tant, treure D_INTER_SECCIO no ha estat una bona decisio.

# En el mod14, la seguent variable amb major pvalor es D_TIPUS_VIA. Enlloc de treure D_INTER_SECCIO, traiem D_TIPUS_VIA



# Model amb 13 variables (traiem D_TIPUS_VIA) -----------------------------

mod13 <- glm(severity ~ nomDem 
             + F_VIANANTS 
             + F_BICICLETES 
             + F_CICLOMOTORS 
             + F_MOTOCICLETES
             + F_VEH_LLEUGERS 
             + F_VEH_PESANTS
             + D_INTER_SECCIO
             + D_LLUMINOSITAT
             + D_SUBZONA
             + grupDiaLab
             + grupHor 
             + tipAcc, family = 'binomial', train.data)
summary(mod13) # L'AIC s'ha mantingut constant i la deviancia s'ha mantingut practicament constant. Per tant, treure D_TIPUS_VIA ha estat una bona decisio.

# Creem un diccionari amb el minim pvalor de cada categoria
diccionari <- list(nomDem = 0, 
                   F_VIANANTS = 0,
                   F_BICICLETES = 0,
                   F_CICLOMOTORS = 0,
                   F_MOTOCICLETES = 0,
                   F_VEH_LLEUGERS = 0,
                   F_VEH_PESANTS = 0,
                   D_INTER_SECCIO = 0,
                   D_LLUMINOSITAT = 0,
                   D_SUBZONA = 0,
                   grupDiaLab = 0,
                   grupHor = 0,
                   tipAcc = 0)

for (i in 1:length(diccionari)) {
  p.valors <- summary(mod13)$coefficients[,4]
  vars <- grep(paste0("^", as.character(names(diccionari[i]))), names(p.valors))
  p.valors <- p.valors[vars]
  min.p.valors <- min(p.valors)
  diccionari[i] <- min.p.valors
}

# Extreiem les claus i els valors del diccionari
claus <- names(diccionari)
valors <- unlist(diccionari)

# Trobem la posicio amb el maxim valor
max.pos <- which.max(valors)

# Trobem la clau amb el maxim valor
max.clau <- claus[max.pos]

# Mostrem la clau amb el maxim valor
diccionari[max.clau]



# Model amb 12 variables (traiem D_INTER_SECCIO) --------------------------

mod12 <- glm(severity ~ nomDem 
             + F_VIANANTS 
             + F_BICICLETES
             + F_CICLOMOTORS 
             + F_MOTOCICLETES
             + F_VEH_LLEUGERS 
             + F_VEH_PESANTS
             + D_LLUMINOSITAT
             + D_SUBZONA
             + grupDiaLab
             + grupHor 
             + tipAcc, family = 'binomial', train.data)
summary(mod12) # L'AIC ha augmentat. Per tant, treure D_INTER_SECCIO no ha estat una bona decisio.

# En el mod13, la resta de variables son significatives. Per tant, no en podem treure cap altra. Ens quedem amb el mod13. 



# Test Anova --------------------------------------------------------------

anova(mod17, mod16, mod15, mod14, mod13, mod12, test = 'Chisq')
# L'unic pvalor proper a 0 de la taula es el del mod12, de manera que el mod12 es significativament pitjor que els altres. 
# En canvi, entre el mod17, mod16, mod15, mod14 i mod13 no hi ha diferencies quant a deviancia (el pvalor no es proper a 0). Per tant, escollim el mod13, ja que es bo i te menys regressors.



# Model definitiu (mod13). Passem a anomenar-lo mod -----------------------

mod <- glm(severity ~ nomDem 
           + F_VIANANTS
           + F_BICICLETES 
           + F_CICLOMOTORS 
           + F_MOTOCICLETES
           + F_VEH_LLEUGERS 
           + F_VEH_PESANTS
           + D_INTER_SECCIO
           + D_LLUMINOSITAT
           + D_SUBZONA
           + grupDiaLab
           + grupHor 
           + tipAcc, family = 'binomial', train.data)
summary(mod) 



# Validacio dels residus del model ------------------------------------

par(mfrow = c(2, 2))
plot(mod, ask = F)
par(mfrow = c(1, 1))

ggplot(data.frame(res = residuals.glm(mod, type = 'deviance'),
                  pred = predict.glm(mod, type = 'response')),
       aes(x = pred, y = res)) +
  geom_point() +
  labs(x = 'fitted response values', y = 'deviance residuals')



# AUC ---------------------------------------------------------------------

predMod <- prediction(predictions = predict(mod, newdata = test.data, type = 'response'),
                      labels = test.data$severity)

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

# Escollim 0.1 com a threshold, ja que volem tenir una sensibilitat elevada.



# Taula de confusio -------------------------------------------------------

pred <- predict(mod, newdata = test.data, type = 'response')
table(test.data$severity, pred > 0.15, dnn = c('actual', 'predicted'))



# Sensitivity = TP/(FN+TP) ------------------------------------------------

sum(test.data$severity == 1 & pred > 0.15)/sum(test.data$severity == 1)



# Specificity = TN/(TN+FP) ------------------------------------------------

sum(test.data$severity == 0 & pred < 0.15)/sum(test.data$severity == 0)




# Questions a respondre ---------------------------------------------------

coefficients(mod)

# Com afecta el fet que sigui un dia del cap de setmana en les probabilitats que un accident sigui molt sever?
1/exp(coefficients(mod)[17]) # El fet que sigui un dia del cap de setmana multiplica per 1.190828  les probabilitats de ser un accident molt sever.

# Com afecta el fet que sigui de nit en les probabilitats que un accident sigui molt sever?
exp(coefficients(mod)[18]) # El fet que sigui de nit multiplica per 1.614406 les probabilitats de ser un accident molt sever.



# Prediccio de dues situacions hipotetiques -------------------------------

# Prediccio per a un accident que a priori sembla molt sever
predict(mod, 
        newdata = data.frame(nomDem = factor("Lleida", levels = levels(accidents$nomDem)), 
                             F_VIANANTS = factor("[>2]", levels = levels(accidents$F_VIANANTS)),
                             F_BICICLETES = factor("[0-2]", levels = levels(accidents$F_BICICLETES)),
                             F_CICLOMOTORS = factor("[0]", levels = levels(accidents$F_CICLOMOTORS)),
                             F_MOTOCICLETES = factor("[0-2]", levels = levels(accidents$F_MOTOCICLETES)),
                             F_VEH_LLEUGERS = factor("[0-1]", levels = levels(accidents$F_VEH_LLEUGERS)),
                             F_VEH_PESANTS = factor("[>0]", levels = levels(accidents$F_VEH_PESANTS)),
                             D_INTER_SECCIO = factor("En secció", levels = levels(accidents$D_INTER_SECCIO)),
                             D_LLUMINOSITAT = factor("Il·luminació nul·la", levels = levels(accidents$D_LLUMINOSITAT)),
                             D_SUBZONA = factor("Carretera", levels = levels(accidents$D_SUBZONA)),
                             grupDiaLab = factor("CapDeSetmana", levels = levels(accidents$grupDiaLab)),
                             grupHor = factor("Nit", levels = levels(accidents$grupHor)),
                             tipAcc = factor("Atropellament", levels = levels(accidents$tipAcc))),
        type = 'response')

# Prediccio per a un accident que a prior sembla sever
predict(mod, 
        newdata = data.frame(nomDem = factor("Barcelona", levels = levels(accidents$nomDem)), 
                             F_VIANANTS = factor("[0-2]", levels = levels(accidents$F_VIANANTS)),
                             F_BICICLETES = factor("[0-2]", levels = levels(accidents$F_BICICLETES)),
                             F_CICLOMOTORS = factor("[0]", levels = levels(accidents$F_CICLOMOTORS)),
                             F_MOTOCICLETES = factor("[0-2]", levels = levels(accidents$F_MOTOCICLETES)),
                             F_VEH_LLEUGERS = factor("[>1]", levels = levels(accidents$F_VEH_LLEUGERS)),
                             F_VEH_PESANTS = factor("[0]", levels = levels(accidents$F_VEH_PESANTS)),
                             D_INTER_SECCIO = factor("Dintre intersecció", levels = levels(accidents$D_INTER_SECCIO)),
                             D_LLUMINOSITAT = factor("Il·luminació alta", levels = levels(accidents$D_LLUMINOSITAT)),
                             D_SUBZONA = factor("Zona urbana", levels = levels(accidents$D_SUBZONA)),
                             grupDiaLab = factor("Feiners", levels = levels(accidents$grupDiaLab)),
                             grupHor = factor("Dia", levels = levels(accidents$grupHor)),
                             tipAcc = factor("Col.lisió", levels = levels(accidents$tipAcc))),
        type = 'response')