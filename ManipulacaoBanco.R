#### Análises S#######


setwd("D:/Users/User/Documents/TrabalhosEstatistica/DataSetS")
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


bancoS <- read.csv("DataSetShirley.csv", sep = ",",encoding = "UTF-8")
View(bancoS)
write.xlsx(bancoS, file = "BancoExcel.xlsx")

##### Retirando Colunas #####

bancoS[1:6] <- NULL
View(bancoS)
colnames(bancoS)

library(arsenal)

nomesoriginais <- colnames(bancoS)
nomesoriginais

sink("NomesOriginais.txt")
sink()

write.table(nomesoriginais, file = "NomesOriginais.docx")

##### Renomeando Colunas #####
colnames(bancoS)[1] <-"Q1_Idade"
colnames(bancoS)[2] <-"Q2-Raça"
colnames(bancoS)[3] <-"Q3-Sexo"
colnames(bancoS)[4] <-"Q4-Orientacao_Sexual"
colnames(bancoS)[5] <-"Q5-Estado"
colnames(bancoS)[6] <-"Q6-Cidade"
colnames(bancoS)[7] <-"Q7-EstadoCivil"
colnames(bancoS)[8] <-"Q8-Renda"
colnames(bancoS)[9] <-"Q9-RendaFamiliar"
colnames(bancoS)[10] <-"Q10-Escolaridade"
colnames(bancoS)[11] <-"Q11-TempoFormado"
colnames(bancoS)[12] <-"Q12-AbordagemTeorica"
colnames(bancoS)[13] <-"Q13-GrupoRisco"
colnames(bancoS)[14] <-"Q14-Percepcao_Dist_Social"
colnames(bancoS)[15] <-"Q15-Faz_Dist_Social"
colnames(bancoS)[16] <-"Q16-Morou_Fam_Durante_DistSoc"
colnames(bancoS)[17] <-"Q17-Num_Pes_Moravam_Durante_DistSoc"
colnames(bancoS)[18] <-"Q18-Ativ_Dur_NaoSentiuPrazer"
colnames(bancoS)[19] <-"Q19-Ativ_Dur_SentiuPrazer"
colnames(bancoS)[20] <-"Q20-Duracao_DistSoc"
colnames(bancoS)[21] <-"Q21-Prof_Ben_Dur_Pan"
colnames(bancoS)[22] <-"Q22-Teve_Reinventar"
colnames(bancoS)[23] <-"Q23-Teve_Buscar_Infor_Tec"
colnames(bancoS)[24] <-"Q24-Uso_Rec_Tecno_Dif"
colnames(bancoS)[25] <-"Q25-Teve_Buscar_Infor_Teoricas"
colnames(bancoS)[26] <-"Q26-Publ_Durante_Pan"
colnames(bancoS)[27] <-"Q27-Ocupação_Pan"
colnames(bancoS)[28] <-"Q28-Maior_Deman_Pan"
colnames(bancoS)[29] <-"Q29-Aum_Sofr_Clientes_Pan"
colnames(bancoS)[30] <-"Q30-HomeOffice"
colnames(bancoS)[31] <-"Q31-Atend_Virturais"
colnames(bancoS)[32] <-"Q32-Atend_Virturais_Dep_Pan"
colnames(bancoS)[33] <-"Q33-Modalidades_Aten"
colnames(bancoS)[34] <-"Q34-Queixas_Clientes"
colnames(bancoS)[35] <-"Q35-Dist_Rel_Prof_Clientes"
colnames(bancoS)[36] <-"Q36-Dist_Rel_Fam_Clientes"
colnames(bancoS)[37] <-"Q37-Dif_Dur_Pan"
colnames(bancoS)[38] <-"Q38-Quais"
colnames(bancoS)[39] <-"Q39-Facil_Dur_Pan"
colnames(bancoS)[40] <-"Q40-Quais"
colnames(bancoS)[41] <-"Q41-Modelos_Atuacao"
colnames(bancoS)[42] <-"Q42-Modelos_Atuacao_Det"
colnames(bancoS)[43] <-"Q43-Mais_Inf_Pan"

bancoS[44] <- NULL

write.xlsx(bancoS, file = "BancoExcelRenomeado.xlsx")

#### Banco Renomeado######
bancoS <- read_excel ("BancoExcelRenomeado.xlsx",sheet= 1)

View(bancoS)
colnames(bancoS)

# Variaveis do tipo da 12 separar pelo ; em outras colunas novas 
# X12a b e assim por diante
str(bancoS)
bancoS$`Q2-Raça`<- factor(bancoS$`Q2-Raça`)
bancoS$`Q3-Sexo`<- factor(bancoS$`Q3-Sexo`)
bancoS$`Q11-TempoFormado`<- numeric(bancoS$`Q11-TempoFormado`)
bancoS$`Q8-Renda`<- factor(bancoS$`Q8-Renda`)
levels(bancoS$`Q8-Renda`)
View(bancoS)
library(dplyr)
bancoS$`Q8-Renda` <-revalue(bancoS$`Q8-Renda`, c("Não tenho renda." = "Sem_Renda",
                                   "Menos de um salário mínimo."="Menos_1_SalMin",
                                   "Em média 1 salário mínimo (R$1.045)" ="Salário_Mínimo",
                                   "Em média de 2 - 3 salários mínimos (R$2.090,00à R$3.135,00)"="De_2_a_3",
                                   "Em média de 4 - 5 salários mínimos (R$4.180,00 à R$5.225,00)" = "De_4_a_5",
                                   "Em média 6 a 7 salários mínimos (R$6.270,00 à R$7.315,00)" = "De_6_a_7",
                                   "Em média 8 a 10 salários mínimos (R$8.360,00 à R$10.450,00)" = "De_8_a_10",
                                   "Em média 11 a 15 salários mínimos (R$11.495,00 à R$15.675,00)" = "De_11_a_15",
                                   "Mais de 16 salários mínimos (Mais de R$16.720,00)" = "Mais_de_16"))

bancoS$`Q8-Renda` <- factor(bancoS$`Q8-Renda`, ordered = TRUE,levels =  c("Sem_Renda","Salário_Mínimo","De_2_a_3","De_4_a_5","De_6_a_7","De_8_a_10","De_11_a_15","Mais_de_16"))


bancoS$`Q9-RendaFamiliar` <- factor(bancoS$`Q9-RendaFamiliar`)
levels(bancoS$`Q9-RendaFamiliar`)

bancoS$`Q9-RendaFamiliar` <- revalue(bancoS$`Q9-RendaFamiliar`,c("Em média 11 a 15 salários mínimos (R$11.495,00 à R$15.675,00)" = "De_11_a_15",
                                                                             "Em média 16 a 20 salários mínimos (R$16.720,00à R$20.900,00)" = "De_16_a_20",
                                                                             "Em média 6 a 7 salários mínimos (R$6.270,00 à R$7.315,00)" = "De_6_a_7",
                                                                             "Em média 8 a 10 salários mínimos (R$8.360,00 à R$10.450,00)" = "De_8_a_10",
                                                                             "Em média de 2 - 3 salários mínimos (R$2.090,00à R$3.135,00)" = "De_2_a_3",
                                                                             "Em média de 4 - 5 salários mínimos (R$4.180,00 à R$5.225,00)" = "De_4_a_5",
                                                                             "Mais de 21 salários mínimos (Mais de R$20.900,00)" = "Mais de 21",
                                                                             "Menos de um salário mínimo (Menos de R$1.045)" = "Menos_1_SalMin"
                                                                            ))
levels(bancoS$`Q9-RendaFamiliar`)
bancoS$`Q9-RendaFamiliar` <- factor(bancoS$`Q9-RendaFamiliar`, ordered = TRUE, levels = c("Menos_1_SalMin","De_2_a_3","De_4_a_5","De_6_a_7","De_8_a_10","De_11_a_15","De_16_a_20","Mais de 21"))
levels(bancoS$`Q9-RendaFamiliar`)

# Transformando as Variaveis em Fatores

View(bancoS)
names <- c(14:18)
bancoS[,names] <- lapply(bancoS[,names] , factor)
str(bancoS)

bancoS$`Q16-Morou_Fam_Durante_DistSoc` <-revalue(bancoS$`Q16-Morou_Fam_Durante_DistSoc`, c("Não." = "Nao", "sim." = "Sim"))

levels(bancoS$`Q17-Num_Pes_Moravam_Durante_DistSoc`)
bancoS$`Q17-Num_Pes_Moravam_Durante_DistSoc`<- revalue(bancoS$`Q17-Num_Pes_Moravam_Durante_DistSoc`, c("Mais 1 pessoa"= "Mais_1_Pes",
                                                                                                                   "Mais 2 pessoas" = "Mais_2_Pes",
                                                                                                                   "Mais 3 pessoas" = "Mais_3_Pes",
                                                                                                                   "Mais 5 pessoas" = "Mais_5_Pes",
                                                                                                                   "Mais 6 pessoas" = "Mais_6_Pes",
                                                                                                                   "Moro /Morava sozinho (a)" = "Sozinho(a)"))

levels(bancoS$`Q17-Num_Pes_Moravam_Durante_DistSoc`)
# Combinando Fatores

bancoS$`Q17-Num_Pes_Moravam_Durante_DistSoc`<- combineLevels(bancoS$`Q17-Num_Pes_Moravam_Durante_DistSoc`,levs = c("Mais_3_Pes", "Mais_5_Pes","Mais_6_Pes"), newLabel = c("Mais_3_Pes"))

View(bancoS)
levels(bancoS$`Q17-Num_Pes_Moravam_Durante_DistSoc`)

# Trabalhando as Questoes Q20 a 25
rm(names)
View(bancoS)

names <- c(20:25)
bancoS[,names] <- lapply(bancoS[,names] , factor)
str(bancoS)

levels(bancoS$`Q20-Duracao_DistSoc`)

bancoS$`Q20-Duracao_DistSoc` <- revalue(bancoS$`Q20-Duracao_DistSoc`,c("Até junho de 2021"="Ate_Met_2021",
                                                 "Até o meio de 2021" = "Ate_Met_2021",
                                                 "Creio que até Jun/2021" = "Ate_Met_2021"))

bancoS$`Q20-Duracao_DistSoc` <- revalue(bancoS$`Q20-Duracao_DistSoc`,c("Até que tenhamos uma  vacina" = "Ate_Ter_Vacina",
                                                                                   "Até termos uma vacina" = "Ate_Ter_Vacina",
                                                                                   "Até vacina segura, acredito que ainda 2021" = "Ate_Ter_Vacina",
                                                                                   "Vai durar até sair a vacina"="Ate_Ter_Vacina",
                                                                                   "Deve durar até termos a vacina e estarmos todos vacinados"="Ate_Ter_Vacina"))

bancoS$`Q20-Duracao_DistSoc` <- revalue(bancoS$`Q20-Duracao_DistSoc`,c("Vai durar entre, 60 e 90 dias."= "Ate_Final_2020",
                                                                                   "Vai durar por, pelo menos, mais de 60 dias."= "Ate_Final_2020",
                                                                                   "Vai durar até o fim de 2020." = "Ate_Final_2020"))

bancoS$`Q20-Duracao_DistSoc` <- revalue(bancoS$`Q20-Duracao_DistSoc`,c("Vai durar até final de 2021." = "Até_Final_2021",
                                                                                   "Vai durar por mais de um ano." = "Até_Final_2021"))

bancoS$`Q20-Duracao_DistSoc` <- revalue(bancoS$`Q20-Duracao_DistSoc`,c("Já acabou e não retornará mais nenhuma forma de distanciamento." = "Já_acabou_Nao_Volta_Mais"))

bancoS$`Q20-Duracao_DistSoc` <- revalue(bancoS$`Q20-Duracao_DistSoc`,c("Não sei dizer. Está muito incerto." = "Nao_Sei",
                                                                                   "Nao sei." = "Nao_Sei"))
bancoS$`Q23-Teve_Buscar_Infor_Tec`<- revalue(bancoS$`Q23-Teve_Buscar_Infor_Tec`, c("Já atendia remotamente desde que o CRP autorizou. Não precisei de adaptação nesse sentido" = "Outra_Resposta",
                                                                                               "Sim, muitos. No início foi muito difícil, pois as novas aprendizagens invadiam meu tempo de descanso e convívio familiar. Atualmente me sinto bem com isso." = "Outra_Resposta",
                                                                                               "Não e me sinto bem por isso." = "Não_Sinto_Bem_Por_Isso",
                                                                                               "Não e me sinto mal por isso."= "Não_Sinto_Mal_Por_Isso",
                                                                                               "Sim. Muitos. E me sinto bem por isso." = "Sim_Sinto_Bem_Por_Isso",
                                                                                               "Sim. Muitos. E me sinto mal por isso." = "Sim_Sinto_Mal_Por_Isso",
                                                                                               "Sim. Pouco. E me sinto bem por isso." = "Sim_Sinto_Bem_Por_Isso",
                                                                                               "Sim. Razoavelmente. E me sinto bem por isso." = "Sim_Sinto_Bem_Por_Isso",
                                                                                               "Sim. Razoavelmente. E me sinto mal por isso." = "Sim_Sinto_Mal_Por_Isso"))
bancoS$`Q24-Uso_Rec_Tecno_Dif`<- revalue(bancoS$`Q24-Uso_Rec_Tecno_Dif`, c("Já usava tecnologias da informação e comunicação para trabalhar, mas o uso intensificou e diversificou." = "Sim",
                                                                                       "Já usava, apenas ampliei os usos" =  "Sim",
                                                                                       "Não, continuo usando os mesmos recursos." = "Nao",
                                                                                       "Não, não uso recursos tecnológicos." = "Nao",
                                                                                       "Sim. Mais chamadas com vídeo no celular com parentes, amigos e trabalho." = "Sim",
                                                                                       "Sim. Programas de Videoconferências no computador ou no celular" = "Sim"))
bancoS$`Q25-Teve_Buscar_Infor_Teoricas` <- factor(bancoS$`Q25-Teve_Buscar_Infor_Teoricas`)
levels(bancoS$`Q25-Teve_Buscar_Infor_Teoricas`)

bancoS$`Q25-Teve_Buscar_Infor_Teoricas`<- revalue(bancoS$`Q25-Teve_Buscar_Infor_Teoricas`, c("Não e me sinto bem por isso."= "Não_Sinto_Bem_Por_Isso",
                                                                                                         "Não e me sinto mal por isso." = "Não_Sinto_Mal_Por_Isso",
                                                                                                       "Sim, muitas. Com um custo inicial muito alto (excesso de tempo de trabalho e novas aprendizagens). Atualmente me sinto bem por isso" = "Sim_Sinto_Bem_Por_Isso",
                                                                                                         "Sim. Muitas. E me sinto bem por isso."  = "Sim_Sinto_Bem_Por_Isso",
                                                                                                         "Sim. Muitas. E me sinto mal por isso." = "Sim_Sinto_Mal_Por_Isso",
                                                                                                         "Sim. Pouco. E me sinto bem por isso."  = "Sim_Sinto_Bem_Por_Isso",
                                                                                                         "Sim. Razoavelmente. E me sinto bem por isso"  =  "Sim_Sinto_Bem_Por_Isso",
                                                                                                         "Sim. Razoavelmente. E me sinto mal por isso." = "Sim_Sinto_Mal_Por_Isso"))



bancoS$`Q29-Aum_Sofr_Clientes_Pan`<- factor(bancoS$`Q29-Aum_Sofr_Clientes_Pan`)  
levels(bancoS$`Q29-Aum_Sofr_Clientes_Pan`)


bancoS$`Q29-Aum_Sofr_Clientes_Pan` <- revalue(bancoS$`Q29-Aum_Sofr_Clientes_Pan`, c("ainda nao estou atendendo" = "Nao_Atende",
                                                                                                "Alguns sim." = "Sim",
                                                                                                "Aumentou por parte dos alunos (assistência)" = "Sim",
                                                                                              "Não aumentou." = "Nao",
                                                                                                "Não estou atendendo em Clínica no momento, mas percebi aumento de casos ou que potencializou os já existentes na esfera organizacional." = "Nao_Atende",
                                                                                                "Relacionado a pandemia não." = "Nao",
                                                                                                "Sim, aumentou moderadamente."  = "Sim",
                                                                                                "Sim, aumentou muito mais." = "Sim",
                                                                                                "Sim, aumentou um pouco." = "Sim"))



bancoS$`Q31-Atend_Virturais` <- factor(bancoS$`Q31-Atend_Virturais`)
levels(bancoS$`Q31-Atend_Virturais`)

bancoS$`Q31-Atend_Virturais` <- revalue(bancoS$`Q31-Atend_Virturais`, c("Continuo ministrando aulas remotas e orientando pós graduação"  = "Sim",
                                                                                    "estou sem atender no momento."  = "Nao_Atende",
                                                                                    "Não atendo ainda" = "Nao_Atende",
                                                                                    "Não. Atendo apenas presencialmente."  = "Nao",
                                                                                    "Nunca me identifiquei com esta modalidade, mas aprendi a respeitar. Hoje trabalho confortavelmente, pois vejo que o resultado é o mesmo, o que vale é o cuidado com o outro." = "Sim",
                                                                                    "Sim, comecei a atender nessa modalidade com a Pandemia."  = "Sim",
                                                                                    "Sim, já atendia nessa modalidade." = "Sim",
                                                                                    "Sim, mas não parei de atender presencialmente durante a Pandemia." = "Sim",
                                                                                    "Todas as supervisões têm sido online." = "Sim"))



bancoS$`Q41-Modelos_Atuacao` <- factor(bancoS$`Q41-Modelos_Atuacao`)
levels(bancoS$`Q41-Modelos_Atuacao`)
table(bancoS$`Q41-Modelos_Atuacao`)


bancoS$`Q41-Modelos_Atuacao` <- revalue(bancoS$`Q41-Modelos_Atuacao`, c("Não sei responder." = "Não_Sei",
                                                                                    "Não." = "Não",
                                                                                    "Sim." = "Sim"))

bancoS$`Q5-Estado` <- factor(bancoS$`Q5-Estado`)
levels(bancoS$`Q5-Estado`)

bancoS$`Q5-Estado`<- revalue(bancoS$`Q5-Estado`, c("Bahia"="BH",
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

bancoS$`Q7-EstadoCivil` <- factor(bancoS$`Q7-EstadoCivil`)
levels(bancoS$`Q7-EstadoCivil`)
bancoS$`Q7-EstadoCivil`<- revalue(bancoS$`Q7-EstadoCivil`, c("Casado (a)"  = "Casado(a)",         
                                                                         "Mora junto/Amasiado" =  "Outros",
                                                                         "Separada" = "Separado(a)",
                                                                         "Solteiro (a)" = "Solteiro(a)",          
                                                                         "União estável" =  "Outros"))


count(bancoS$`Q8-Renda`)
View(bancoS)

write.csv(bancoS,file = "BancoManipulado1.csv")
bancoS2 <- read.csv2("BancoManipulado1.csv", sep = ",")
View(bancoS2)

colnames(bancoS2)
select("Q1_Idade")
str(bancoS2$Q1_Idade)







