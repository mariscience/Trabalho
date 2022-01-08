## Analises ###

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
                arsenal,
                stargazer,
                demoGraphic,
                RColorBrewer)

bancoShirley4 <- read_xlsx("BancoManipulado4.xlsx",sheet = "BancoCompleto")

View(bancoShirley4)

Q37freq <- read_xlsx("BancoManipulado4.xlsx",sheet = "Q37")
Q39freq <- read_xlsx("BancoManipulado4.xlsx",sheet = "Q39")
?read_xlsx

bancoShirley4[44,11]<- 20
View(bancoShirley4)

names(bancoShirley4)
str(bancoShirley4)
bancoShirley4[1]<- NULL
bancoShirley4[51,10]<- "Mestrado"


# Transformando em fatores

names <- c(1:11,13:17,20:33,41)
bancoShirley4[,names] <- lapply(bancoShirley4[,names] , factor)
str(bancoShirley4)

bancoShirley4 <-tibble(bancoShirley4)

View(bancoShirley4)

# Caracteristicas Socioeconomic
names(bancoShirley4)
tab1 <- tableby(~ Q1.Idade + 
                  Q2.Raça + 
                  Q3.Sexo + 
                  Q4.Orientacao.Sexual +
                  Q5.Estado +
                  Q7.Estado.Civil +
                  Q8.Renda +
                  Q10.Escolaridade,
                data=bancoShirley4)
summary(tab1, text = TRUE)

write2word(tab1,"D:/Users/User/Documents/TrabalhosEstatistica/Shirley/DataSetShirley", title="SocioDemograficas.doc")


# Descritivas

# Tempo de Formado
desc(count(bancoShirley4$Q11.Tempo.Formado))

sort
tabela <- data.frame(describe(bancoShirley4$Q11.TempoFormado))
View(tabela)
hist(bancoShirley4$Q11.TempoFormado)
count(bancoShirley4$Q11.TempoFormado)
View(bancoShirley4)
colors()
summary(bancoShirley4$Q11.Tempo.Formado)
hist(bancoShirley4$Q11.Tempo.Formado,
     breaks = 40,
     main = "Tempo de Formado", 
     xlab = "Tempo de Formado", ylab = "Freq. Absoluta", 
     col = c("blue"), 
     border = FALSE, 
     xlim = c(0,50), ylim = c(0,10),
     labels = TRUE,
     xaxt='n')
axis(side=1, at=seq(0,50, 1), labels=seq(0,50,1))

# Q12
Q12freq <- read_xlsx("BancoManipulado4.xlsx",sheet = "Q12")

count(bancoShirley4$Q11.Tempo.Formado)

display.brewer.pal(n=6,name = "Reds")
display.brewer.pal(n=6,name = "Dark2")
#Q13
table(bancoShirley4$Q14.Percepcao_Dist_Social) 

risco2 <- (bancoShirley4$Q15.Faz_Dist_Social)
barplot(risco2, beside = T)
barplot(bancoShirley4$Q13.GrupoRisco)
barplot(bancoShirley4$Q13.GrupoRisco,col = c("steelblue1", "tan3"))

count(bancoShirley4$Q22.Teve.Reinventar)


count(bancoShirley4$Q26.Publ.Durante.Pan)

count(bancoShirley4$Q28.Maior.Deman.Pan)

count(bancoShirley4$Q30.Home.Office)

count(bancoShirley4$Q31.Atend.Virtuais)

count(bancoShirley4$Q32.Atend.Virturais.Dep.Pan)

count(bancoShirley4$Q33.Modalidades.Aten)

count(Q39freq)

count(bancoShirley4$Q11.Tempo.Formado)
install.packages('tinytex')
tinytex::install_tinytex()

if(!require(installr)) { install.packages("installr"); require(installr)} 

install.pandoc()
1


