setwd("D:/Users/User/Documents/TrabalhosEstatistica/Shirley/DataSetShirley")
getwd()

if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman:: p_load(readxl,
                xlsx,dplyr,
                ggplot2,
                car,
                rstatix,
                ggpubr,
                tidyr,
                pastecs,
                summarytools,
                writexl,
                QuantPsyc,
                psych,
                jtools,
                gtsummary,
                tidyverse,
                arsenal
                ,plyr,
                rockchalk,
                stringr,
                arsenal,stargazer,demoGraphic)


bancoShirley <- read.csv("BancoManipulado1.csv")
View(bancoShirley)
install.packages("gtsummary")
l
bancoShirley <- as_tibble(bancoShirley)
bancoShirley
View(bancoShirley)
colnames(bancoShirley)

str(bancoShirley)
bancoShirley %>%
  select(Q4.Orientacao_Sexual)
bancoShirley$Q4.Orientacao_Sexual <- as.character(bancoShirley$Q4.Orientacao_Sexual)

# Trabalhando Q4
bancoShirley <- bancoShirley %>%
  separate(Q4.Orientacao_Sexual, sep = ";",
           into = c("Q4.Orientacao_Sexual","Q4_B"))
View(bancoShirley)
bancoShirley <-select(bancoShirley,-Q4_B)
View(bancoShirley)

bancoShirley$Q4.Orientacao_Sexual <- as.factor(bancoShirley$Q4.Orientacao_Sexual)
levels(bancoShirley$Q4.Orientacao_Sexual)


bancoShirley$Q4.Orientacao_Sexual <- revalue(bancoShirley$Q4.Orientacao_Sexual,c("Cisgênero (me identifico com o sexo que eu nasci)" ="Sem_Resp",
                                                                                 "Gay" = "Homossexual",
                                                                                 "Lésbica" = "Homossexual"))

count(bancoShirley$Q4.Orientacao_Sexual)


# Trabalhando a Q10 - Colocar a graduação mais alta

bancoShirley$Q10.Escolaridade <- as.character(bancoShirley$Q10.Escolaridade)
colnames(bancoShirley)

bancoShirley %>%
  select(Q10.Escolaridade)


bancoShirley$Q10.Escolaridade %>%
  str_count(pattern = ";") %>% 
  max()
rm(teste)
bancoShirley <- bancoShirley %>%
  separate(Q10.Escolaridade, sep = ";",
           into = paste0("Q10_", 1:6))
View(bancoShirley)

bancoShirley$Q10_1 <- gsub("Superior completo em Psicologia", "SupCompleto", bancoShirley$Q10_1)
count(bancoShirley$Q10_1)
bancoShirley$Q10_1 <- gsub("Bacharel", "SupCompleto", bancoShirley$Q10_1)
bancoShirley$Q10_1 <- gsub("Formação de Psicólogo", "SupCompleto", bancoShirley$Q10_1)

bancoShirley$Q10_1 <- gsub("Membro Analista da Associação Junguiana do Brasil", "SupCompleto", bancoShirley$Q10_1)

count(bancoShirley$Q10_2)
View(bancoShirley)

bancoShirley$Q10_2 <- gsub("Bacharel", "SupCompleto", bancoShirley$Q10_2)
bancoShirley$Q10_2 <- gsub("Licenciatura", "SupCompleto", bancoShirley$Q10_2)

bancoShirley$Q10_2 <- gsub("Formação de Psicólogo", "SupCompleto", bancoShirley$Q10_2)
count(bancoShirley$Q10_2)

count(bancoShirley$Q10_3)
bancoShirley$Q10_3 <- gsub("Formação de Psicólogo", "SupCompleto", bancoShirley$Q10_3)
count(bancoShirley$Q10_3)

count(bancoShirley$Q10_4)
bancoShirley$Q10_4 <- gsub("Formação de Psicólogo", "SupCompleto", bancoShirley$Q10_4)
bancoShirley$Q10_4 <- gsub("Cursando pós em Arteterapia", "SupCompleto", bancoShirley$Q10_4)
bancoShirley$Q10_4 <- gsub("Curso doutorado, me encontro no 4 seméstre.", "Mestrado", bancoShirley$Q10_4)
count(bancoShirley$Q10_4)

count(bancoShirley$Q10_5)
count(bancoShirley$Q10_6)

View(bancoShirley)

write_xlsx(bancoShirley, "BancoManipulado2.xlsx")

##########
##########
###############
######################
setwd("D:/Users/User/Documents/TrabalhosEstatistica/Shirley/DataSetShirley")
getwd()

if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman:: p_load(readxl,
                xlsx,dplyr,
                ggplot2,
                car,
                rstatix,
                ggpubr,
                tidyr,
                pastecs,
                summarytools,
                writexl,
                QuantPsyc,
                psych,
                jtools,
                gtsummary,
                tidyverse,
                arsenal
                ,plyr,
                rockchalk,
                stringr)

bancoShirley <- read_xlsx("BancoManipulado2.xlsx")
bancoShirley <- as_tibble(bancoShirley)
View(bancoShirley)

# Manipulando Q13

bancoShirley$Q13.GrupoRisco <-  ifelse(grepl("Sim",bancoShirley$Q13.GrupoRisco),"Sim","Nao")
View(bancoShirley)

# Manipulando Q14

bancoShirley$Q14.Percepcao_Dist_Social %>%
  str_count(pattern = ";") %>% 
  max()

bancoShirley <- bancoShirley %>%
  separate(Q14.Percepcao_Dist_Social, sep = ";",
           into = paste0("Q14_", 1:3))

write_xlsx(bancoShirley, "BancoManipulado3.xlsx") 

###########Aqui Banco 3#######
## Separando por ; #####
# Q15
bancoShirley3 <- read_xlsx("BancoManipulado3.xlsx")
bancoShirley3 <- as_tibble(bancoShirley3)
View(bancoShirley3)

bancoShirley3$Q15.Faz_Dist_Social %>%
  str_count(pattern = ";") %>% 
  max()
bancoShirley3 <- bancoShirley3 %>%
  separate(Q15.Faz_Dist_Social, sep = ";",
           into = paste0("Q15_", 1:3))
View(bancoShirley3)

# Q21

bancoShirley3$Q21.Prof_Ben_Dur_Pan %>%
  str_count(pattern = ";") %>% 
  max()

bancoShirley3 <- bancoShirley3 %>%
  separate(Q21.Prof_Ben_Dur_Pan, sep = ";",
           into = paste0("Q21_", 1:4))

names(bancoShirley3)

# Q26

bancoShirley3$Q26.Publ_Durante_Pan %>%
  str_count(pattern = ";") %>% 
  max()

bancoShirley3 <- bancoShirley3 %>%
  separate(Q26.Publ_Durante_Pan, sep = ";",
           into = paste0("Q26_", 1:7))

names(bancoShirley3)

# Q27

bancoShirley3$Q27.Ocupação_Pan %>%
  str_count(pattern = ";") %>% 
  max()

bancoShirley3 <- bancoShirley3 %>%
  separate(Q27.Ocupação_Pan, sep = ";",
           into = paste0("Q27_", 1:5))
names(bancoShirley3)

# Q28
bancoShirley3$Q28.Maior_Deman_Pan %>%
  str_count(pattern = ";") %>% 
  max()

bancoShirley3 <- bancoShirley3 %>%
  separate(Q28.Maior_Deman_Pan, sep = ";",
           into = paste0("Q28_", 1:2))

# Q30
bancoShirley3$Q30.HomeOffice %>%
  str_count(pattern = ";") %>% 
  max()

bancoShirley3 <- bancoShirley3 %>%
  separate(Q30.HomeOffice, sep = ";",
           into = paste0("Q30_", 1:2))

names(bancoShirley3)

# Q37
bancoShirley3$Q37.Dif_Dur_Pan %>%
  str_count(pattern = ";") %>% 
  max()

bancoShirley3 <- bancoShirley3 %>%
  separate(Q37.Dif_Dur_Pan, sep = ";",
           into = paste0("Q37_", 1:8))

names(bancoShirley3)

# Q39
bancoShirley3$Q39.Facil_Dur_Pan %>%
  str_count(pattern = ";") %>% 
  max()

bancoShirley3 <- bancoShirley3 %>%
  separate(Q39.Facil_Dur_Pan, sep = ";",
           into = paste0("Q39_", 1:6))

names(bancoShirley3)

View(bancoShirley)

###################
# AQUI BANCO 4 ####
###################



write_xlsx(bancoShirley3, "BancoManipulado4.xlsx") 
bancoShirley4 <- read_xlsx("BancoManipulado4.xlsx",sheet = "BancoCompleto")

View(bancoShirley4)

Q12freq <- read_xlsx("BancoManipulado4.xlsx",sheet = "Q12")
Q37freq <- read_xlsx("BancoManipulado4.xlsx",sheet = "Q37")
Q39freq <- read_xlsx("BancoManipulado4.xlsx",sheet = "Q39")
?read_xlsx

#Proximos Passos
# Conferir se todas as variavesl que serão utilizadas estão limpas
# Classificar por numericas e fatores
# Verificar a necessidade de tirar as espaços e outras limpezas
# Lembrar as variaveis de Frequencia Q12,Q37 e Q39 estão no banco 4 cruas
# Nas analises pensar em grupos que exercem os modelos de intervenção

names(bancoShirley4)
str(bancoShirley4)
bancoShirley4[1]<- NULL
# Transformando em fatores

names <- c(1:10,13:17,20:32,40)
bancoShirley4[,names] <- lapply(bancoShirley4[,names] , factor)
str(bancoShirley4)

# Caracteristicas Socioeconomicas
names(bancoShirley4)
tabela1 <- cont_table("Q1_Idade","Q2.Raça","Q3.Sexo",bancoShirley4)


