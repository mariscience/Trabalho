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
val <- read_xlsx("ChaosCompleto.xlsx") # Leitura do Banco de Dados
View(val)                             # Visualização do Banco
str(val)                          # Estrutura do Banco

colnames(val)[2] <-"Age"
colnames(val)[4] <-"School.Year"
colnames(val)[5] <-"Schooling.Father"
colnames(val)[6] <-"Schooling.Mother"
colnames(val)[7] <-"TF_Q1"
colnames(val)[8] <-"Q1"
colnames(val)[9] <-"TF_Q2"
colnames(val)[10] <-"Q2"
colnames(val)[11] <-"TF_Q3"
colnames(val)[12] <-"Q3"
colnames(val)[13] <-"TF_Q4"
colnames(val)[14] <-"Q4"
colnames(val)[15] <-"TF_Q5"
colnames(val)[16] <-"Q5"
colnames(val)[17] <-"TF_Q6"
colnames(val)[18] <-"Q6"
colnames(val)[19] <-"TF_Q7"
colnames(val)[20] <-"Q7"
colnames(val)[21] <-"TF_Q8"
colnames(val)[22] <-"Q8"
colnames(val)[23] <-"TF_Q9"
colnames(val)[24] <-"Q9"
colnames(val)[25] <-"TF_Q10"
colnames(val)[26] <-"Q10"
colnames(val)[27] <-"TF_Q11"
colnames(val)[28] <-"Q11"
colnames(val)[29] <-"TF_Q12"
colnames(val)[30] <-"Q12"
colnames(val)[31] <-"TF_Q13"
colnames(val)[32] <-"Q13"
colnames(val)[33] <-"TF_Q14"
colnames(val)[34] <-"Q14"
colnames(val)[35] <-"TF_Q15"
colnames(val)[36] <-"Q15"

View(val)

# Limpando os seguintes valores missing por linha:
# A049 A0138 A076 A0252 A0267 A0282 A0283 A0288 A0289
# Obs. IMPORTANTE : A49 e A175 não foram excluidos por que 
# não são missing em outras variaveis no banco completo


val <- val[-c(138,176,252,267,282,283,288,289),]

View(val)

##########################################################################
########## Manipulação Banco Dicotomico ################## ### ###########
##########################################################################

# Extrair um outro banco com as variaveis dicotomicas TRUE and FALSE
### Mudar
bancodic <- val[c(1:6,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35)]
View(bancodic)




# Retirando dados Missing para uma primeira analise
# Verificar melhor metodo para imputação desses dados

bancodic <- bancodic[-c(1:37, 43:47,49,109,111), ]

### Omitido valores NA
# bancodic[bancodic==""] <- NA
# bancodic <- na.omit (bancodic)

# Mudando o Label das Variaveis da Analises 
# F = 1 e T = 2 

questoes <- bancodic[,7:21]

questoes <- lapply(questoes,FUN = function(questoes){ifelse( questoes == 'T', 1, 0)})

bancodic[,7:21]<- questoes

View(bancodic)
library(writexl)
write_xlsx(bancodic,"bancodicPronto.xlsx")
write_xlsx(bancodic,"ChaosFinal.xlsx")
write.csv(bancodic, file = "bancodicFinal.csv", sep = ",")

##########################################################################
########## Manipulação Banco Likert ################## ### ###########
##########################################################################

# Extrair um outro banco com as variaveis likert (outra forma mais enxuta)
### Mudar Colunas
bancolikert <- val[c(1:6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36)]
View(bancolikert)

# Retirando alguns Missing values

bancolikert <- bancolikert[-49, ]

View(bancolikert) 

### Omitido valores NA
bancolikert[bancolikert==""] <- NA
bancolikert <- na.omit (bancolikert)

# Mudando o Label das Variaveis da Analises 
# 1 a 4

questoes2 <- bancolikert[,7:21]

questoes2 <- lapply(questoes2, FUN = function(questoes2){recode(questoes2,
                                                               'a'=1, 
                                                               'b'=2,
                                                               'c'=3,
                                                               'd'=4)})

bancolikert[,7:21]<- questoes2

View(bancodic)


View(bancolikert)

write_xlsx(bancolikert, "ChaosLikPronto.xlsx")
write.table(bancolikert,"ChaosLikTXT.txt", sep = ",")
write.csv(bancolikert, file = "bancolikertFinal.csv", sep = ",")










