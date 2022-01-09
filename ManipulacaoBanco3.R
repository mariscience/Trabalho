######\º/######\º/ CHAOS Estruturação do Banco de Dados  ######\º/######\º/
################## Manipulação Geral###########
#### Foi separado em dois bancos de dados #########

setwd("D:/Users/User/Documents/MEGA/Doutorado/CHAOS/ValidacaoCHAOS/ManipulacaoBanco")
getwd()

if(!require(pacman)) install.packages("pacman")
library(pacman)
pacman:: p_load(dplyr,
                readxl,
                xlsx,
                writexl)

#### Manipulação Geral  ######
valchaos <- read_xlsx("ChaosCompleto.xlsx") # Leitura do Banco de Dados
View(valchaos)                             # Visualização do Banco
str(valchaos)                          # Estrutura do Banco

colnames(valchaos)[2] <-"Age"
colnames(valchaos)[4] <-"School.Year"
colnames(valchaos)[5] <-"Schooling.Father"
colnames(valchaos)[6] <-"Schooling.Mother"
colnames(valchaos)[7] <-"TF_Q1"
colnames(valchaos)[8] <-"Q1"
colnames(valchaos)[9] <-"TF_Q2"
colnames(valchaos)[10] <-"Q2"
colnames(valchaos)[11] <-"TF_Q3"
colnames(valchaos)[12] <-"Q3"
colnames(valchaos)[13] <-"TF_Q4"
colnames(valchaos)[14] <-"Q4"
colnames(valchaos)[15] <-"TF_Q5"
colnames(valchaos)[16] <-"Q5"
colnames(valchaos)[17] <-"TF_Q6"
colnames(valchaos)[18] <-"Q6"
colnames(valchaos)[19] <-"TF_Q7"
colnames(valchaos)[20] <-"Q7"
colnames(valchaos)[21] <-"TF_Q8"
colnames(valchaos)[22] <-"Q8"
colnames(valchaos)[23] <-"TF_Q9"
colnames(valchaos)[24] <-"Q9"
colnames(valchaos)[25] <-"TF_Q10"
colnames(valchaos)[26] <-"Q10"
colnames(valchaos)[27] <-"TF_Q11"
colnames(valchaos)[28] <-"Q11"
colnames(valchaos)[29] <-"TF_Q12"
colnames(valchaos)[30] <-"Q12"
colnames(valchaos)[31] <-"TF_Q13"
colnames(valchaos)[32] <-"Q13"
colnames(valchaos)[33] <-"TF_Q14"
colnames(valchaos)[34] <-"Q14"
colnames(valchaos)[35] <-"TF_Q15"
colnames(valchaos)[36] <-"Q15"

View(valchaos)

# Limpando os seguintes valores missing por linha:
# A049 A0138 A076 A0252 A0267 A0282 A0283 A0288 A0289
# Obs. IMPORTANTE : A49 e A175 não foram excluidos por que 
# não são missing em outras variaveis no banco completo


valchaos <- valchaos[-c(138,176,252,267,282,283,288,289),]

View(valchaos)

##########################################################################
########## Manipulação Banco Dicotomico ################## ### ###########
##########################################################################

# Extrair um outro banco com as variaveis dicotomicas TRUE and FALSE
### Mudar
chaosdic <- valchaos[c(1:6,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35)]
View(chaosdic)




# Retirando dados Missing para uma primeira analise
# Verificar melhor metodo para imputação desses dados

chaosdic <- chaosdic[-c(1:37, 43:47,49,109,111), ]

### Omitido valores NA
# chaosdic[chaosdic==""] <- NA
# chaosdic <- na.omit (chaosdic)

# Mudando o Label das Variaveis da Analises 
# F = 1 e T = 2 

questoes <- chaosdic[,7:21]

questoes <- lapply(questoes,FUN = function(questoes){ifelse( questoes == 'T', 1, 0)})

chaosdic[,7:21]<- questoes

View(chaosdic)
library(writexl)
write_xlsx(chaosdic,"ChaosDicPronto.xlsx")
write_xlsx(chaosdic,"ChaosFinal.xlsx")
write.csv(chaosdic, file = "ChaosDicFinal.csv", sep = ",")

##########################################################################
########## Manipulação Banco Likert ################## ### ###########
##########################################################################

# Extrair um outro banco com as variaveis likert (outra forma mais enxuta)
### Mudar Colunas
chaosLikert <- valchaos[c(1:6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36)]
View(chaosLikert)

# Retirando alguns Missing values

chaosLikert <- chaosLikert[-49, ]

View(chaosLikert) 

### Omitido valores NA
chaosLikert[chaosLikert==""] <- NA
chaosLikert <- na.omit (chaosLikert)

# Mudando o Label das Variaveis da Analises 
# 1 a 4

questoes2 <- chaosLikert[,7:21]

questoes2 <- lapply(questoes2, FUN = function(questoes2){recode(questoes2,
                                                               'a'=1, 
                                                               'b'=2,
                                                               'c'=3,
                                                               'd'=4)})

chaosLikert[,7:21]<- questoes2

View(chaosdic)


View(chaosLikert)

write_xlsx(chaosLikert, "ChaosLikPronto.xlsx")
write.table(chaosLikert,"ChaosLikTXT.txt", sep = ",")
write.csv(chaosLikert, file = "ChaosLikertFinal.csv", sep = ",")










