## Analises ###

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
                arsenal,
                stargazer,
                demoGraphic,
                RColorBrewer)

bancoS4 <- read_xlsx("BancoManipulado4.xlsx",sheet = "BancoCompleto")

View(bancoS4)

Q37freq <- read_xlsx("BancoManipulado4.xlsx",sheet = "Q37")
Q39freq <- read_xlsx("BancoManipulado4.xlsx",sheet = "Q39")
?read_xlsx

bancoS4[44,11]<- 20
View(bancoS4)

names(bancoS4)
str(bancoS4)
bancoS4[1]<- NULL
bancoS4[51,10]<- "Mestrado"


# Transformando em fatores

names <- c(1:11,13:17,20:33,41)
bancoS4[,names] <- lapply(bancoS4[,names] , factor)
str(bancoS4)

bancoS4 <-tibble(bancoS4)

View(bancoS4)

# Caracteristicas Socioeconomic
names(bancoS4)
tab1 <- tableby(~ Q1.Idade + 
                  Q2.Raça + 
                  Q3.Sexo + 
                  Q4.Orientacao.Sexual +
                  Q5.Estado +
                  Q7.Estado.Civil +
                  Q8.Renda +
                  Q10.Escolaridade,
                data=bancoS4)
summary(tab1, text = TRUE)

write2word(tab1,"D:/Users/User/Documents/TrabalhosEstatistica/DataSetS, title="SocioDemograficas.doc")


# Descritivas

# Tempo de Formado

desc(count(bancoS4$Q11.Tempo.Formado))
sort
tabela <- data.frame(describe(bancoS4$Q11.TempoFormado))
View(tabela)
hist(bancoS4$Q11.TempoFormado)
count(bancoS4$Q11.TempoFormado)
View(bancoS4)
colors()
summary(bancoS4$Q11.Tempo.Formado)
hist(bancoS4$Q11.Tempo.Formado,
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

count(bancoS4$Q11.Tempo.Formado)

display.brewer.pal(n=6,name = "Reds")
display.brewer.pal(n=6,name = "Dark2")
#Q13
table(bancoS4$Q14.Percepcao_Dist_Social) 

risco2 <- (bancoS4$Q15.Faz_Dist_Social)
barplot(risco2, beside = T)
barplot(bancoS4$Q13.GrupoRisco)
barplot(bancoS4$Q13.GrupoRisco,col = c("steelblue1", "tan3"))

count(bancoS4$Q22.Teve.Reinventar)


count(bancoS4$Q26.Publ.Durante.Pan)

count(bancoS4$Q28.Maior.Deman.Pan)

count(bancoS4$Q30.Home.Office)

count(bancoS4$Q31.Atend.Virtuais)

count(bancoS4$Q32.Atend.Virturais.Dep.Pan)

count(bancoS4$Q33.Modalidades.Aten)

count(Q39freq)

count(bancoS4$Q11.Tempo.Formado)
install.packages('tinytex')
tinytex::install_tinytex()



