setwd("D:/Users/User/Documents/TrabalhosEstatistica/DataSetS")
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


bancoS <- read.csv("BancoManipulado1.csv")
View(bancoS <- read.csv("BancoManipulado1.csv")
)
install.packages("gtsummary")
l
bancoS <- as_tibble(bancoS)
bancoS
View(bancoS)
colnames(bancoS)

str(bancoS)
bancoS %>%
  select(Q4.Orientacao_Sexual)
bancoS$Q4.Orientacao_Sexual <- as.character(bancoS$Q4.Orientacao_Sexual)

# Trabalhando Q4
bancoS <- bancoS %>%
  separate(Q4.Orientacao_Sexual, sep = ";",
           into = c("Q4.Orientacao_Sexual","Q4_B"))
View(bancoS)
bancoS <-select(bancoS,-Q4_B)
View(bancoS)

bancoS$Q4.Orientacao_Sexual <- as.factor(bancoS$Q4.Orientacao_Sexual)
levels(bancoS$Q4.Orientacao_Sexual)


bancoS$Q4.Orientacao_Sexual <- revalue(bancoS$Q4.Orientacao_Sexual,c("Cisgênero (me identifico com o sexo que eu nasci)" ="Sem_Resp",
                                                                                 "Gay" = "Homossexual",
                                                                                 "Lésbica" = "Homossexual"))

count(bancoS$Q4.Orientacao_Sexual)


# Trabalhando a Q10 - Colocar a graduação mais alta

bancoS$Q10.Escolaridade <- as.character(bancoS$Q10.Escolaridade)
colnames(bancoS)

bancoS %>%
  select(Q10.Escolaridade)


bancoS$Q10.Escolaridade %>%
  str_count(pattern = ";") %>% 
  max()
rm(teste)
bancoS <- bancoS %>%
  separate(Q10.Escolaridade, sep = ";",
           into = paste0("Q10_", 1:6))
View(bancoS)

bancoS$Q10_1 <- gsub("Superior completo em Psicologia", "SupCompleto", bancoS$Q10_1)
count(bancoS$Q10_1)
bancoS$Q10_1 <- gsub("Bacharel", "SupCompleto", bancoS$Q10_1)
bancoS$Q10_1 <- gsub("Formação de Psicólogo", "SupCompleto", bancoS$Q10_1)

bancoS$Q10_1 <- gsub("Membro Analista da Associação Junguiana do Brasil", "SupCompleto", bancoS$Q10_1)

count(bancoS$Q10_2)
View(bancoS)

bancoS$Q10_2 <- gsub("Bacharel", "SupCompleto", bancoS$Q10_2)
bancoS$Q10_2 <- gsub("Licenciatura", "SupCompleto", bancoS$Q10_2)

bancoS$Q10_2 <- gsub("Formação de Psicólogo", "SupCompleto", bancoS$Q10_2)
count(bancoS$Q10_2)

count(bancoS$Q10_3)
bancoS$Q10_3 <- gsub("Formação de Psicólogo", "SupCompleto", bancoS$Q10_3)
count(bancoS$Q10_3)

count(bancoS$Q10_4)
bancoS$Q10_4 <- gsub("Formação de Psicólogo", "SupCompleto", bancoS$Q10_4)
bancoS$Q10_4 <- gsub("Cursando pós em Arteterapia", "SupCompleto", bancoS$Q10_4)
bancoS$Q10_4 <- gsub("Curso doutorado, me encontro no 4 seméstre.", "Mestrado", bancoS$Q10_4)
count(bancoS$Q10_4)

count(bancoS$Q10_5)
count(bancoS$Q10_6)

View(bancoS)

write_xlsx(bancoS, "BancoManipulado2.xlsx")

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

bancoS <- read_xlsx("BancoManipulado2.xlsx")
bancoS <- as_tibble(bancoS)
View(bancoS)

# Manipulando Q13

bancoS$Q13.GrupoRisco <-  ifelse(grepl("Sim",bancoS$Q13.GrupoRisco),"Sim","Nao")
View(bancoS)

# Manipulando Q14

bancoS$Q14.Percepcao_Dist_Social %>%
  str_count(pattern = ";") %>% 
  max()

bancoS <- bancoS %>%
  separate(Q14.Percepcao_Dist_Social, sep = ";",
           into = paste0("Q14_", 1:3))

write_xlsx(bancoS, "BancoManipulado3.xlsx") 

###########Aqui Banco 3#######
## Separando por ; #####
# Q15
bancoS3 <- read_xlsx("BancoManipulado3.xlsx")
bancoS3 <- as_tibble(bancoS3)
View(bancoS3)

bancoS3$Q15.Faz_Dist_Social %>%
  str_count(pattern = ";") %>% 
  max()
bancoS3 <- bancoS3 %>%
  separate(Q15.Faz_Dist_Social, sep = ";",
           into = paste0("Q15_", 1:3))
View(bancoS3)

# Q21

bancoS3$Q21.Prof_Ben_Dur_Pan %>%
  str_count(pattern = ";") %>% 
  max()

bancoS3 <- bancoS3 %>%
  separate(Q21.Prof_Ben_Dur_Pan, sep = ";",
           into = paste0("Q21_", 1:4))

names(bancoS3)

# Q26

bancoS3$Q26.Publ_Durante_Pan %>%
  str_count(pattern = ";") %>% 
  max()

bancoS3 <- bancoS3 %>%
  separate(Q26.Publ_Durante_Pan, sep = ";",
           into = paste0("Q26_", 1:7))

names(bancoS3)

# Q27

bancoS3$Q27.Ocupação_Pan %>%
  str_count(pattern = ";") %>% 
  max()

bancoS3 <- bancoS3 %>%
  separate(Q27.Ocupação_Pan, sep = ";",
           into = paste0("Q27_", 1:5))
names(bancoS3)

# Q28
bancoS3$Q28.Maior_Deman_Pan %>%
  str_count(pattern = ";") %>% 
  max()

bancoS3 <- bancoS3 %>%
  separate(Q28.Maior_Deman_Pan, sep = ";",
           into = paste0("Q28_", 1:2))

# Q30
bancoS3$Q30.HomeOffice %>%
  str_count(pattern = ";") %>% 
  max()

bancoS3 <- bancoS3 %>%
  separate(Q30.HomeOffice, sep = ";",
           into = paste0("Q30_", 1:2))

names(bancoS3)

# Q37
bancoS3$Q37.Dif_Dur_Pan %>%
  str_count(pattern = ";") %>% 
  max()

bancoS3 <- bancoS3 %>%
  separate(Q37.Dif_Dur_Pan, sep = ";",
           into = paste0("Q37_", 1:8))

names(bancoS3)

# Q39
bancoS3$Q39.Facil_Dur_Pan %>%
  str_count(pattern = ";") %>% 
  max()

bancoS3 <- bancoS3 %>%
  separate(Q39.Facil_Dur_Pan, sep = ";",
           into = paste0("Q39_", 1:6))

names(bancoS3)

View(bancoS)

###################
# AQUI BANCO 4 ####
###################



write_xlsx(bancoS3, "BancoManipulado4.xlsx") 
bancoS4 <- read_xlsx("BancoManipulado4.xlsx",sheet = "BancoCompleto")

View(bancoS4)

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

names(bancoS4)
str(bancoS4)
bancoS4[1]<- NULL
# Transformando em fatores

names <- c(1:10,13:17,20:32,40)
bancoS4[,names] <- lapply(bancoS4[,names] , factor)
str(bancoS4)

# Caracteristicas Socioeconomicas
names(bancoS4)
tabela1 <- cont_table("Q1_Idade","Q2.Raça","Q3.Sexo",bancoS4)


