#### Mestrado Shirley#######


setwd("D:/Users/User/Documents/TrabalhosEstatistica/Shirley/DataSetShirley")
getwd()

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
                rockchalk)


bancoShirley <- read.csv("DataSetShirley.csv", sep = ",",encoding = "UTF-8")
View(bancoShirley)
write.xlsx(bancoShirley, file = "BancoExcel.xlsx")

##### Retirando Colunas #####

bancoShirley[1:6] <- NULL
View(bancoShirley)
colnames(bancoShirley)

library(arsenal)

nomesoriginais <- colnames(bancoShirley)
nomesoriginais

sink("NomesOriginais.txt")
sink()

write.table(nomesoriginais, file = "NomesOriginais.docx")

##### Renomeando Colunas #####
colnames(bancoShirley)[1] <-"Q1_Idade"
colnames(bancoShirley)[2] <-"Q2-Raça"
colnames(bancoShirley)[3] <-"Q3-Sexo"
colnames(bancoShirley)[4] <-"Q4-Orientacao_Sexual"
colnames(bancoShirley)[5] <-"Q5-Estado"
colnames(bancoShirley)[6] <-"Q6-Cidade"
colnames(bancoShirley)[7] <-"Q7-EstadoCivil"
colnames(bancoShirley)[8] <-"Q8-Renda"
colnames(bancoShirley)[9] <-"Q9-RendaFamiliar"
colnames(bancoShirley)[10] <-"Q10-Escolaridade"
colnames(bancoShirley)[11] <-"Q11-TempoFormado"
colnames(bancoShirley)[12] <-"Q12-AbordagemTeorica"
colnames(bancoShirley)[13] <-"Q13-GrupoRisco"
colnames(bancoShirley)[14] <-"Q14-Percepcao_Dist_Social"
colnames(bancoShirley)[15] <-"Q15-Faz_Dist_Social"
colnames(bancoShirley)[16] <-"Q16-Morou_Fam_Durante_DistSoc"
colnames(bancoShirley)[17] <-"Q17-Num_Pes_Moravam_Durante_DistSoc"
colnames(bancoShirley)[18] <-"Q18-Ativ_Dur_NaoSentiuPrazer"
colnames(bancoShirley)[19] <-"Q19-Ativ_Dur_SentiuPrazer"
colnames(bancoShirley)[20] <-"Q20-Duracao_DistSoc"
colnames(bancoShirley)[21] <-"Q21-Prof_Ben_Dur_Pan"
colnames(bancoShirley)[22] <-"Q22-Teve_Reinventar"
colnames(bancoShirley)[23] <-"Q23-Teve_Buscar_Infor_Tec"
colnames(bancoShirley)[24] <-"Q24-Uso_Rec_Tecno_Dif"
colnames(bancoShirley)[25] <-"Q25-Teve_Buscar_Infor_Teoricas"
colnames(bancoShirley)[26] <-"Q26-Publ_Durante_Pan"
colnames(bancoShirley)[27] <-"Q27-Ocupação_Pan"
colnames(bancoShirley)[28] <-"Q28-Maior_Deman_Pan"
colnames(bancoShirley)[29] <-"Q29-Aum_Sofr_Clientes_Pan"
colnames(bancoShirley)[30] <-"Q30-HomeOffice"
colnames(bancoShirley)[31] <-"Q31-Atend_Virturais"
colnames(bancoShirley)[32] <-"Q32-Atend_Virturais_Dep_Pan"
colnames(bancoShirley)[33] <-"Q33-Modalidades_Aten"
colnames(bancoShirley)[34] <-"Q34-Queixas_Clientes"
colnames(bancoShirley)[35] <-"Q35-Dist_Rel_Prof_Clientes"
colnames(bancoShirley)[36] <-"Q36-Dist_Rel_Fam_Clientes"
colnames(bancoShirley)[37] <-"Q37-Dif_Dur_Pan"
colnames(bancoShirley)[38] <-"Q38-Quais"
colnames(bancoShirley)[39] <-"Q39-Facil_Dur_Pan"
colnames(bancoShirley)[40] <-"Q40-Quais"
colnames(bancoShirley)[41] <-"Q41-Modelos_Atuacao"
colnames(bancoShirley)[42] <-"Q42-Modelos_Atuacao_Det"
colnames(bancoShirley)[43] <-"Q43-Mais_Inf_Pan"

bancoShirley[44] <- NULL

write.xlsx(bancoShirley, file = "BancoExcelRenomeado.xlsx")

#### Banco Renomeado######
bancoShirley <- read_excel ("BancoExcelRenomeado.xlsx",sheet= 1)

View(bancoShirley)
colnames(bancoShirley)

# Variaveis do tipo da 12 separar pelo ; em outras colunas novas 
# X12a b e assim por diante
str(bancoShirley)
bancoShirley$`Q2-Raça`<- factor(bancoShirley$`Q2-Raça`)
bancoShirley$`Q3-Sexo`<- factor(bancoShirley$`Q3-Sexo`)
bancoShirley$`Q11-TempoFormado`<- numeric(bancoShirley$`Q11-TempoFormado`)
bancoShirley$`Q8-Renda`<- factor(bancoShirley$`Q8-Renda`)
levels(bancoShirley$`Q8-Renda`)
View(bancoShirley)
library(dplyr)
bancoShirley$`Q8-Renda` <-revalue(bancoShirley$`Q8-Renda`, c("Não tenho renda." = "Sem_Renda",
                                   "Menos de um salário mínimo."="Menos_1_SalMin",
                                   "Em média 1 salário mínimo (R$1.045)" ="Salário_Mínimo",
                                   "Em média de 2 - 3 salários mínimos (R$2.090,00à R$3.135,00)"="De_2_a_3",
                                   "Em média de 4 - 5 salários mínimos (R$4.180,00 à R$5.225,00)" = "De_4_a_5",
                                   "Em média 6 a 7 salários mínimos (R$6.270,00 à R$7.315,00)" = "De_6_a_7",
                                   "Em média 8 a 10 salários mínimos (R$8.360,00 à R$10.450,00)" = "De_8_a_10",
                                   "Em média 11 a 15 salários mínimos (R$11.495,00 à R$15.675,00)" = "De_11_a_15",
                                   "Mais de 16 salários mínimos (Mais de R$16.720,00)" = "Mais_de_16"))

bancoShirley$`Q8-Renda` <- factor(bancoShirley$`Q8-Renda`, ordered = TRUE,levels =  c("Sem_Renda","Salário_Mínimo","De_2_a_3","De_4_a_5","De_6_a_7","De_8_a_10","De_11_a_15","Mais_de_16"))


bancoShirley$`Q9-RendaFamiliar` <- factor(bancoShirley$`Q9-RendaFamiliar`)
levels(bancoShirley$`Q9-RendaFamiliar`)

bancoShirley$`Q9-RendaFamiliar` <- revalue(bancoShirley$`Q9-RendaFamiliar`,c("Em média 11 a 15 salários mínimos (R$11.495,00 à R$15.675,00)" = "De_11_a_15",
                                                                             "Em média 16 a 20 salários mínimos (R$16.720,00à R$20.900,00)" = "De_16_a_20",
                                                                             "Em média 6 a 7 salários mínimos (R$6.270,00 à R$7.315,00)" = "De_6_a_7",
                                                                             "Em média 8 a 10 salários mínimos (R$8.360,00 à R$10.450,00)" = "De_8_a_10",
                                                                             "Em média de 2 - 3 salários mínimos (R$2.090,00à R$3.135,00)" = "De_2_a_3",
                                                                             "Em média de 4 - 5 salários mínimos (R$4.180,00 à R$5.225,00)" = "De_4_a_5",
                                                                             "Mais de 21 salários mínimos (Mais de R$20.900,00)" = "Mais de 21",
                                                                             "Menos de um salário mínimo (Menos de R$1.045)" = "Menos_1_SalMin"
                                                                            ))
levels(bancoShirley$`Q9-RendaFamiliar`)
bancoShirley$`Q9-RendaFamiliar` <- factor(bancoShirley$`Q9-RendaFamiliar`, ordered = TRUE, levels = c("Menos_1_SalMin","De_2_a_3","De_4_a_5","De_6_a_7","De_8_a_10","De_11_a_15","De_16_a_20","Mais de 21"))
levels(bancoShirley$`Q9-RendaFamiliar`)

# Transformando as Variaveis em Fatores

View(bancoShirley)
names <- c(14:18)
bancoShirley[,names] <- lapply(bancoShirley[,names] , factor)
str(bancoShirley)

bancoShirley$`Q16-Morou_Fam_Durante_DistSoc` <-revalue(bancoShirley$`Q16-Morou_Fam_Durante_DistSoc`, c("Não." = "Nao", "sim." = "Sim"))

levels(bancoShirley$`Q17-Num_Pes_Moravam_Durante_DistSoc`)
bancoShirley$`Q17-Num_Pes_Moravam_Durante_DistSoc`<- revalue(bancoShirley$`Q17-Num_Pes_Moravam_Durante_DistSoc`, c("Mais 1 pessoa"= "Mais_1_Pes",
                                                                                                                   "Mais 2 pessoas" = "Mais_2_Pes",
                                                                                                                   "Mais 3 pessoas" = "Mais_3_Pes",
                                                                                                                   "Mais 5 pessoas" = "Mais_5_Pes",
                                                                                                                   "Mais 6 pessoas" = "Mais_6_Pes",
                                                                                                                   "Moro /Morava sozinho (a)" = "Sozinho(a)"))

levels(bancoShirley$`Q17-Num_Pes_Moravam_Durante_DistSoc`)
# Combinando Fatores

bancoShirley$`Q17-Num_Pes_Moravam_Durante_DistSoc`<- combineLevels(bancoShirley$`Q17-Num_Pes_Moravam_Durante_DistSoc`,levs = c("Mais_3_Pes", "Mais_5_Pes","Mais_6_Pes"), newLabel = c("Mais_3_Pes"))

View(bancoShirley)
levels(bancoShirley$`Q17-Num_Pes_Moravam_Durante_DistSoc`)

# Trabalhando as Questoes Q20 a 25
rm(names)
View(bancoShirley)

names <- c(20:25)
bancoShirley[,names] <- lapply(bancoShirley[,names] , factor)
str(bancoShirley)

levels(bancoShirley$`Q20-Duracao_DistSoc`)

bancoShirley$`Q20-Duracao_DistSoc` <- revalue(bancoShirley$`Q20-Duracao_DistSoc`,c("Até junho de 2021"="Ate_Met_2021",
                                                 "Até o meio de 2021" = "Ate_Met_2021",
                                                 "Creio que até Jun/2021" = "Ate_Met_2021"))

bancoShirley$`Q20-Duracao_DistSoc` <- revalue(bancoShirley$`Q20-Duracao_DistSoc`,c("Até que tenhamos uma  vacina" = "Ate_Ter_Vacina",
                                                                                   "Até termos uma vacina" = "Ate_Ter_Vacina",
                                                                                   "Até vacina segura, acredito que ainda 2021" = "Ate_Ter_Vacina",
                                                                                   "Vai durar até sair a vacina"="Ate_Ter_Vacina",
                                                                                   "Deve durar até termos a vacina e estarmos todos vacinados"="Ate_Ter_Vacina"))

bancoShirley$`Q20-Duracao_DistSoc` <- revalue(bancoShirley$`Q20-Duracao_DistSoc`,c("Vai durar entre, 60 e 90 dias."= "Ate_Final_2020",
                                                                                   "Vai durar por, pelo menos, mais de 60 dias."= "Ate_Final_2020",
                                                                                   "Vai durar até o fim de 2020." = "Ate_Final_2020"))

bancoShirley$`Q20-Duracao_DistSoc` <- revalue(bancoShirley$`Q20-Duracao_DistSoc`,c("Vai durar até final de 2021." = "Até_Final_2021",
                                                                                   "Vai durar por mais de um ano." = "Até_Final_2021"))

bancoShirley$`Q20-Duracao_DistSoc` <- revalue(bancoShirley$`Q20-Duracao_DistSoc`,c("Já acabou e não retornará mais nenhuma forma de distanciamento." = "Já_acabou_Nao_Volta_Mais"))

bancoShirley$`Q20-Duracao_DistSoc` <- revalue(bancoShirley$`Q20-Duracao_DistSoc`,c("Não sei dizer. Está muito incerto." = "Nao_Sei",
                                                                                   "Nao sei." = "Nao_Sei"))
bancoShirley$`Q23-Teve_Buscar_Infor_Tec`<- revalue(bancoShirley$`Q23-Teve_Buscar_Infor_Tec`, c("Já atendia remotamente desde que o CRP autorizou. Não precisei de adaptação nesse sentido" = "Outra_Resposta",
                                                                                               "Sim, muitos. No início foi muito difícil, pois as novas aprendizagens invadiam meu tempo de descanso e convívio familiar. Atualmente me sinto bem com isso." = "Outra_Resposta",
                                                                                               "Não e me sinto bem por isso." = "Não_Sinto_Bem_Por_Isso",
                                                                                               "Não e me sinto mal por isso."= "Não_Sinto_Mal_Por_Isso",
                                                                                               "Sim. Muitos. E me sinto bem por isso." = "Sim_Sinto_Bem_Por_Isso",
                                                                                               "Sim. Muitos. E me sinto mal por isso." = "Sim_Sinto_Mal_Por_Isso",
                                                                                               "Sim. Pouco. E me sinto bem por isso." = "Sim_Sinto_Bem_Por_Isso",
                                                                                               "Sim. Razoavelmente. E me sinto bem por isso." = "Sim_Sinto_Bem_Por_Isso",
                                                                                               "Sim. Razoavelmente. E me sinto mal por isso." = "Sim_Sinto_Mal_Por_Isso"))
bancoShirley$`Q24-Uso_Rec_Tecno_Dif`<- revalue(bancoShirley$`Q24-Uso_Rec_Tecno_Dif`, c("Já usava tecnologias da informação e comunicação para trabalhar, mas o uso intensificou e diversificou." = "Sim",
                                                                                       "Já usava, apenas ampliei os usos" =  "Sim",
                                                                                       "Não, continuo usando os mesmos recursos." = "Nao",
                                                                                       "Não, não uso recursos tecnológicos." = "Nao",
                                                                                       "Sim. Mais chamadas com vídeo no celular com parentes, amigos e trabalho." = "Sim",
                                                                                       "Sim. Programas de Videoconferências no computador ou no celular" = "Sim"))
bancoShirley$`Q25-Teve_Buscar_Infor_Teoricas` <- factor(bancoShirley$`Q25-Teve_Buscar_Infor_Teoricas`)
levels(bancoShirley$`Q25-Teve_Buscar_Infor_Teoricas`)

bancoShirley$`Q25-Teve_Buscar_Infor_Teoricas`<- revalue(bancoShirley$`Q25-Teve_Buscar_Infor_Teoricas`, c("Não e me sinto bem por isso."= "Não_Sinto_Bem_Por_Isso",
                                                                                                         "Não e me sinto mal por isso." = "Não_Sinto_Mal_Por_Isso",
                                                                                                       "Sim, muitas. Com um custo inicial muito alto (excesso de tempo de trabalho e novas aprendizagens). Atualmente me sinto bem por isso" = "Sim_Sinto_Bem_Por_Isso",
                                                                                                         "Sim. Muitas. E me sinto bem por isso."  = "Sim_Sinto_Bem_Por_Isso",
                                                                                                         "Sim. Muitas. E me sinto mal por isso." = "Sim_Sinto_Mal_Por_Isso",
                                                                                                         "Sim. Pouco. E me sinto bem por isso."  = "Sim_Sinto_Bem_Por_Isso",
                                                                                                         "Sim. Razoavelmente. E me sinto bem por isso"  =  "Sim_Sinto_Bem_Por_Isso",
                                                                                                         "Sim. Razoavelmente. E me sinto mal por isso." = "Sim_Sinto_Mal_Por_Isso"))



bancoShirley$`Q29-Aum_Sofr_Clientes_Pan`<- factor(bancoShirley$`Q29-Aum_Sofr_Clientes_Pan`)  
levels(bancoShirley$`Q29-Aum_Sofr_Clientes_Pan`)


bancoShirley$`Q29-Aum_Sofr_Clientes_Pan` <- revalue(bancoShirley$`Q29-Aum_Sofr_Clientes_Pan`, c("ainda nao estou atendendo" = "Nao_Atende",
                                                                                                "Alguns sim." = "Sim",
                                                                                                "Aumentou por parte dos alunos (assistência)" = "Sim",
                                                                                              "Não aumentou." = "Nao",
                                                                                                "Não estou atendendo em Clínica no momento, mas percebi aumento de casos ou que potencializou os já existentes na esfera organizacional." = "Nao_Atende",
                                                                                                "Relacionado a pandemia não." = "Nao",
                                                                                                "Sim, aumentou moderadamente."  = "Sim",
                                                                                                "Sim, aumentou muito mais." = "Sim",
                                                                                                "Sim, aumentou um pouco." = "Sim"))



bancoShirley$`Q31-Atend_Virturais` <- factor(bancoShirley$`Q31-Atend_Virturais`)
levels(bancoShirley$`Q31-Atend_Virturais`)

bancoShirley$`Q31-Atend_Virturais` <- revalue(bancoShirley$`Q31-Atend_Virturais`, c("Continuo ministrando aulas remotas e orientando pós graduação"  = "Sim",
                                                                                    "estou sem atender no momento."  = "Nao_Atende",
                                                                                    "Não atendo ainda" = "Nao_Atende",
                                                                                    "Não. Atendo apenas presencialmente."  = "Nao",
                                                                                    "Nunca me identifiquei com esta modalidade, mas aprendi a respeitar. Hoje trabalho confortavelmente, pois vejo que o resultado é o mesmo, o que vale é o cuidado com o outro." = "Sim",
                                                                                    "Sim, comecei a atender nessa modalidade com a Pandemia."  = "Sim",
                                                                                    "Sim, já atendia nessa modalidade." = "Sim",
                                                                                    "Sim, mas não parei de atender presencialmente durante a Pandemia." = "Sim",
                                                                                    "Todas as supervisões têm sido online." = "Sim"))



bancoShirley$`Q41-Modelos_Atuacao` <- factor(bancoShirley$`Q41-Modelos_Atuacao`)
levels(bancoShirley$`Q41-Modelos_Atuacao`)
table(bancoShirley$`Q41-Modelos_Atuacao`)


bancoShirley$`Q41-Modelos_Atuacao` <- revalue(bancoShirley$`Q41-Modelos_Atuacao`, c("Não sei responder." = "Não_Sei",
                                                                                    "Não." = "Não",
                                                                                    "Sim." = "Sim"))

bancoShirley$`Q5-Estado` <- factor(bancoShirley$`Q5-Estado`)
levels(bancoShirley$`Q5-Estado`)

bancoShirley$`Q5-Estado`<- revalue(bancoShirley$`Q5-Estado`, c("Bahia"="BH",
                                                               "Mato grosso" ="MT",
                                                               "MG." = "MG",
                                                               "Minas Gerais" = "MG",
                                                               "Pará"= "PA",
                                                               "PARANA" = "PR",
                                                               "Paraná" = "PR",
                                                               "Pernambuco"= "PE",
                                                               "Rio De Janeiro"  = "RJ",
                                                               "Rio Grande do Norte" =  "RN",
                                                               "Rio grande do Sul" =  "RS",
                                                               "Rio Grande do Sul" =  "RS",
                                                               "Rs"= "RS",
                                                               "Santa Catarina"="SC",
                                                               "São  Paulo" =  "SP",
                                                               "Sao Paulo" =  "SP",
                                                               "São paulo" =  "SP",
                                                               "São Paulo" =  "SP",
                                                               "SO"="SP",
                                                               "Sp" =  "SP"))

bancoShirley$`Q7-EstadoCivil` <- factor(bancoShirley$`Q7-EstadoCivil`)
levels(bancoShirley$`Q7-EstadoCivil`)
bancoShirley$`Q7-EstadoCivil`<- revalue(bancoShirley$`Q7-EstadoCivil`, c("Casado (a)"  = "Casado(a)",         
                                                                         "Mora junto/Amasiado" =  "Outros",
                                                                         "Separada" = "Separado(a)",
                                                                         "Solteiro (a)" = "Solteiro(a)",          
                                                                         "União estável" =  "Outros"))


count(bancoShirley$`Q8-Renda`)
View(bancoShirley)

write.csv(bancoShirley,file = "BancoManipulado1.csv")
bancoShirley2 <- read.csv2("BancoManipulado1.csv", sep = ",")
View(bancoShirley2)

colnames(bancoShirley2)
select("Q1_Idade")
str(bancoShirley2$Q1_Idade)







