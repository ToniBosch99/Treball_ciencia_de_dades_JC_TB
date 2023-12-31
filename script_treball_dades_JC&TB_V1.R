################################################################################
# Authors: Jose Calderon & Toni Bosch                                          #
# Date: December 2023                                                          #
################################################################################

# ------------------ LIBRARIES -------------------------------------------------
library(tidyverse)
library(dplyr)
library(ggplot2)


# ------------------ LOAD DATA -------------------------------------------------
accidents <- read.csv(file = 'data/Accidents_de_tr_nsit_amb_morts_o_ferits_greus_a_Catalunya.csv',
                      stringsAsFactors = T)

# Variable mes --> month of the year
accidents$mes <- format(as.Date(accidents$dat), "%B")
accidents$mes <- as.factor(accidents$mes)

# We will create some auxiliar variables
# Variable puntuacioAccident --> How bad is the accident
###################### REVISAR! FERITS LLEUS #####################
accidents <- mutate(accidents,
                    puntuacioAccident = 9 * F_MORTS + 3 * F_FERITS_GREUS + 1 * F_FERITS_LLEUS)

# Variable severitat --> If score bigger than 9, considered severe
accidents <- mutate(accidents, severitat = ifelse(puntuacioAccident < 9, 0, 1))
accidents$severitat <- as.factor(accidents$severitat)


# -------------------- CLEAN DATA ----------------------------------------------
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
visual_inspection(accidents, "severitat", "D_CLIMATOLOGIA")

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

# Variable D_TRACAT_ALIMETRIC --> Out
visual_inspection(accidents, "severitat", "D_TRACAT_ALIMETRIC")

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



# -------------------- CREATE DATASETS -----------------------------------------
set.seed(1964)

trainSize = round(dim(accidents)[1]*0.7, 0)
train = sample(1:dim(accidents)[1], trainSize)
test = -train

accidents_train = accidents[train, ]
accidents_test = accidents[test, ]





























