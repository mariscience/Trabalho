######\º/######\º/ CHAOS Estruturação do Banco de Dados  ######\º/######\º/
################## Imputação de Dados Missing###########
#### Seguido o tutorial: https://operdata.com.br/blog/pacote-mice-no-r-como-funciona/ #########

setwd("D:/Users/User/Documents/MEGA/Doutorado/CHAOS/ValidacaoCHAOS/ManipulacaoBanco")
getwd()

if(!require(pacman)) install.packages("pacman")
library(pacman)
pacman:: p_load(dplyr,
                readxl,
                xlsx,
                writexl,
                missForest,
                finalfit,
                mice,
                naniar)

#### Manipulação Geral  ######
chaosdic <- read_xlsx("ChaosFinal.xlsx") # Leitura do Banco de Dados
View(chaosdic)
### Deleting Collum ID

chaosdic$`Participant ID`<- NULL 

# Missing Value Percent
sapply(chaosdic, function(x) sum(is.na(x)))
tabelamissing <- data.frame(sapply(chaosdic, function(x) sum(is.na(x))))
write_xlsx(tabelamissing,"TabelaMissing.xlsx")
str(chaosdic)

# MCAR Test

mcar_test(chaosdic)

# Missing treatment
imp= chaosdic
init = mice(data=imp, maxit = 0)
meth = init$method
predM = init$predictorMatrix
names(imp)

# Change varibel class

names <- c(6:20)
imp[,names] <- lapply(imp[,names] , factor)
str(imp)

# Imputação para Variaveis continuas
meth[c(4:5)]="cart" #Algoritimo Árvores de classificação e regressão

imputed = mice(imp, method=meth %>% as.vector(), predictorMatrix=predM, m=5)

imp1cont <- complete(imputed,1)
imp2cont <- complete(imputed,2)
imp3cont <- complete(imputed,3)
imp4cont <- complete(imputed,4)
imp5cont <- complete(imputed,5)

missing_glimpse(imp1cont)%>% 
  DT::datatable()


# Imputação para Variaveis Categóricas
meth[c(6:20)]="logreg" #Algoritimo de regressão logistica

imputed = mice(imp, method=meth %>% as.vector(), predictorMatrix=predM, m=5)

imp1cat <- complete(imputed,1)
imp2cat <- complete(imputed,2)
imp3cat <- complete(imputed,3)
imp4cat <- complete(imputed,4)
imp5cat <- complete(imputed,5)

missing_glimpse(imp1cat)%>% 
  DT::datatable()

# Colocando no Banco de Dados
# Função de moda para variavel (categorica)
mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


dados_completo = data.frame(
  Age = chaosdic$Age,
  Gender = chaosdic$Gender,
  School.Year = chaosdic$School.Year,
  Schooling.Father = apply(cbind(imp1cont$Schooling.Father,imp2cont$Schooling.Father,imp3cont$Schooling.Father,imp4cont$Schooling.Father,imp5cont$Schooling.Father),1,mean),
  Schooling.Mother = apply(cbind(imp1cont$Schooling.Mother,imp2cont$Schooling.Mother,imp3cont$Schooling.Mother,imp4cont$Schooling.Mother,imp5cont$Schooling.Mother),1,mean),
  Q1 = apply(cbind(imp1cat$TF_Q1,imp2cat$TF_Q1,imp3cat$TF_Q1,imp4cat$TF_Q1,imp5cat$TF_Q1),1,mode),
  Q2 = apply(cbind(imp1cat$TF_Q2,imp2cat$TF_Q2,imp3cat$TF_Q2,imp4cat$TF_Q2,imp5cat$TF_Q2),1,mode),
  Q3 = apply(cbind(imp1cat$TF_Q3,imp2cat$TF_Q3,imp3cat$TF_Q3,imp4cat$TF_Q3,imp5cat$TF_Q3),1,mode),
  Q4 = apply(cbind(imp1cat$TF_Q4,imp2cat$TF_Q4,imp3cat$TF_Q4,imp4cat$TF_Q4,imp5cat$TF_Q4),1,mode),
  Q5 = apply(cbind(imp1cat$TF_Q5,imp2cat$TF_Q5,imp3cat$TF_Q5,imp4cat$TF_Q5,imp5cat$TF_Q5),1,mode),
  Q6 = apply(cbind(imp1cat$TF_Q6,imp2cat$TF_Q6,imp3cat$TF_Q6,imp4cat$TF_Q6,imp5cat$TF_Q6),1,mode),
  Q7 = apply(cbind(imp1cat$TF_Q7,imp2cat$TF_Q7,imp3cat$TF_Q7,imp4cat$TF_Q7,imp5cat$TF_Q7),1,mode),
  Q8 = apply(cbind(imp1cat$TF_Q8,imp2cat$TF_Q8,imp3cat$TF_Q8,imp4cat$TF_Q8,imp5cat$TF_Q8),1,mode),
  Q9 = apply(cbind(imp1cat$TF_Q9,imp2cat$TF_Q9,imp3cat$TF_Q9,imp4cat$TF_Q9,imp5cat$TF_Q9),1,mode),
  Q10 = apply(cbind(imp1cat$TF_Q10,imp2cat$TF_Q10,imp3cat$TF_Q10,imp4cat$TF_Q10,imp5cat$TF_Q10),1,mode),
  Q11 = apply(cbind(imp1cat$TF_Q11,imp2cat$TF_Q11,imp3cat$TF_Q11,imp4cat$TF_Q11,imp5cat$TF_Q11),1,mode),
  Q12 = apply(cbind(imp1cat$TF_Q12,imp2cat$TF_Q12,imp3cat$TF_Q12,imp4cat$TF_Q12,imp5cat$TF_Q12),1,mode),
  Q13 = apply(cbind(imp1cat$TF_Q13,imp2cat$TF_Q13,imp3cat$TF_Q13,imp4cat$TF_Q13,imp5cat$TF_Q13),1,mode),
  Q14 = apply(cbind(imp1cat$TF_Q14,imp2cat$TF_Q14,imp3cat$TF_Q14,imp4cat$TF_Q14,imp5cat$TF_Q14),1,mode),
  Q15 = apply(cbind(imp1cat$TF_Q15,imp2cat$TF_Q15,imp3cat$TF_Q15,imp4cat$TF_Q15,imp5cat$TF_Q15),1,mode))

# Verificando a Densidade
grafico_imput = function(real,imputado,x)
{
  data.frame("Antes de imputar" = real,
             "Depois de imputar" = imputado) %>% 
    pivot_longer(everything()) %>% 
    na.omit() %>% 
    ggplot(mapping = aes(value,col= name))+
    geom_density()+
    theme_minimal()+
    xlab(x)+
    ylab("Densidade")+
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.2))+
    scale_color_discrete(labels = c("Antes de imputar","Depois de imputar"))+
    guides(color=guide_legend(title=""))
  
} 


grafico_imput(chaosdic$Schooling.Father,dados_completo$Schooling.Father,"Schooling.Father")
grafico_imput(chaosdic$Schooling.Mother,dados_completo$Schooling.Mother,"Schooling.Maother")
grafico_imput(chaosdic$TF_Q1,dados_completo$Q1,"Q1")
grafico_imput(chaosdic$TF_Q2,dados_completo$Q2,"Q2")
grafico_imput(chaosdic$TF_Q3,dados_completo$Q3,"Q3")
grafico_imput(chaosdic$TF_Q4,dados_completo$Q4,"Q4")

write_xlsx(dados_completo,"ChaosDicProntoImput.xlsx")







