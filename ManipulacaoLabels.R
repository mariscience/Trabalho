######## Manipulação de Labels de Variáveis de Um banco de dados #########
setwd("D:/Users/User/Documents/MEGA/Doutorado/Bancos_de_dados/dados")
getwd()

library(pacman)
pacman::p_load(dplyr,readxl,plyr,arsenal,tibble,gtsummary,tidyverse,arsenal,tibble,writexl)


dados <- read_xlsx("Bancodados1.xlsx", sheet = 1)
View(dados)


# Limpando os seguintes valores missing por linha:
# A138 A143,A176,A252,A267,A282,A283,A288,289

dados <- dados[-c(138,143,176,252,267,282,283,288,289),]
View(dados)

# Verificando as estruturas das variáveis
str(dados)

# Excluindo School
dados[,6] <- NULL
dados[,24] <- NULL

#Renomeando as Variaveis

colnames(dados)[1] <-"ID"
colnames(dados)[2] <-"Heavy_drinker_at_home"
colnames(dados)[3] <-"Age_years"
colnames(dados)[4] <-"Schooling.Father"
colnames(dados)[5] <-"Schooling.Mother"
colnames(dados)[6] <-"Type_of_School"
colnames(dados)[7] <-"Gender"
colnames(dados)[8] <-"Guardian"
colnames(dados)[9] <-"How_Often"
colnames(dados)[10] <-"AuditC_Q1_A_firsttime"
colnames(dados)[11] <-"AuditC_Q1_B_age"
colnames(dados)[12] <-"AuditC_Q2_whooffered"
colnames(dados)[13] <-"AuditC_Q3_entireglass"
colnames(dados)[14] <-"AuditC_Q3_B_Age"
colnames(dados)[15] <-"AuditC_Q4_whowherewithyou"
colnames(dados)[16] <-"AuditC_Q5_drunk"
colnames(dados)[17] <-"AuditC_Q5_B_age"
colnames(dados)[18] <-"AuditC_Q6_whowherewithyou"
colnames(dados)[19] <-"AuditC_Q7_howoften"
colnames(dados)[20] <-"AuditC_Q8_howmany"
colnames(dados)[21] <-"AuditC_Q9_sixormore"
colnames(dados)[22] <-"AuditC_Q10_A_4bestfriends"
colnames(dados)[23] <-"AuditC_Q10_B_theirparents"

View(dados)
str(dados)
names(dados)

# Q5 ficou bebado:colocado numero 99 para fazer a filtragem de quem colocou idade


######## Análises Preliminares Questionário Uso de dados- Auditi-C Modificado #########

# Início dia 21/05/2021
# Término dia

# Mudanças no Banco de Dados ver script Manipulação Banco de Dados

#######Mudando Para fator algumas colunas#####


# Gender
dados$Gender <- ifelse(dados$Gender == 1, "Female", "Male")

# Q2
count(dados$AuditC_Q2_whooffered)
dados$AuditC_Q2_whooffered <- factor(dados$AuditC_Q2_whooffered)
dados$AuditC_Q2_whooffered <- revalue(dados$AuditC_Q2_whooffered, c("a" = "Sozinho",
                                                                      "b" = "Familiares",
                                                                      "c" = "Familiares",
                                                                      "d" = "Familiares",
                                                                      "e" = "Amigos", 
                                                                      "f" = "Outros",
                                                                      "g" = "Outros",

                                                                                                                                            "NuncaBebeuNada" = "NuncaBebeuNada"))
count(dados$AuditC_Q2_whooffered)

#Q6
count(dados$AuditC_Q6_whowherewithyou)
dados$AuditC_Q6_whowherewithyou <- revalue(dados$AuditC_Q6_whowherewithyou, 
                                                                    c("a" = "Sozinho",
                                                                      "b" = "Familiares",
                                                                      "c" = "Familiares",
                                                                      "d" = "Familiares",
                                                                      "e" = "Amigos", 
                                                                      "Abstemico" = "Abstemico"))
#Q4
count(dados$AuditC_Q4_whowherewithyou)
dados$AuditC_Q4_whowherewithyou <- revalue(dados$AuditC_Q4_whowherewithyou,
                                                                                c("a" = "Sozinho",
                                                                                   "b" = "Familiares",
                                                                                   "c" = "Familiares",
                                                                                   "d" = "Familiares",
                                                                                   "e" = "Amigos",
                                                                                   "Abstemico" = "Abstemico"))

View(dados)

#Q7

dados$AuditC_Q7_howoften <- revalue(dados$AuditC_Q7_howoften, c("a" = "Nunca",
                                                                   "b" = "Mensalmente ou Menos",
                                                                  "c" = "De 2 a 4 vezes por mês",
                                                              "d" = "De 2 a 3 vezes por semana"))


?count
View(dados)
#Q8
str(dados$AuditC_Q8_howmany)
count(dados$AuditC_Q8_howmany)
dados$AuditC_Q8_howmany <- revalue(dados$AuditC_Q8_howmany, c("0" = "EPG",
                                                                "0.5" = "EPG",
                                                                "1" = "De uma a 2 Doses",
                                                                "1.5" = "De uma a 2 Doses",
                                                                "1/2 ou 1" = "De uma a 2 Doses",
                                                                "2" = "De uma a 2 Doses",
                                                                "3" = "Mais de 3 Doses",
                                                                "4" = "Mais de 3 Doses",
                                                                "5" = "Mais de 3 Doses",
                                                                "10" = "Mais de 3 Doses"))




# dados %>% count(AuditC_Q8_howmany,sort = TRUE)
# Q9
count(dados$AuditC_Q9_sixormore)
dados$AuditC_Q9_sixormore <- revalue(dados$AuditC_Q9_sixormore, c("a" = "Nunca",
                                                                    "b" = "Menos que 1 vez ao mês",
                                                                    "c" = "Mensalmente",
                                                                    "d" = "Semanalmente"))
count(dados$AuditC_Q9_sixormore)
View(dados)

# Q10A
count(dados$AuditC_Q10_A_4bestfriends)
str(dados$AuditC_Q10_A_4bestfriends)
dados$AuditC_Q10_A_4bestfriends <- revalue(dados$AuditC_Q10_A_4bestfriends, c("0" ="Nenhum",
                                                                                "-" ="Um",
                                                                                "1" ="Um",
                                                                                "2" = "Dois",
                                                                                "3" = "Três",
                                                                                "4" = "Quatro ou Mais",
                                                                                "4+" = "Quatro ou Mais"))

# Q10B
str(dados$AuditC_Q10_B_theirparents)
View(dados)
count(dados$AuditC_Q10_B_theirparents)
dados[c(22,31),23]<- NA
dados$AuditC_Q10_B_theirparents <- revalue(dados$AuditC_Q10_B_theirparents,c("NS" = "Não Sabe"))

# Banco Gerado dia 26/07/2021
write_xlsx(dados,"Bancodados2.xlsx")








