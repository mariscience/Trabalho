###### Missing CHAOS######
setwd("D:/Users/User/Documents/MEGA/Doutorado/CHAOS/ValidacaoCHAOS/ManipulacaoBanco")
getwd()


# Contando Valores missing 
sum(is.na(valchaos$Q1))
sum(is.na(valchaos$TF_Q1))
sum(is.na(valchaos$TF_Q13))


na_count <-sapply(valchaos, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
View(na_count)

valchaos %>% summarize_all(funs(sum(is.na(.)) / length(.)))*100

percentna <- valchaos %>% summarize_all(funs(sum(is.na(.)) / length(.)))*100
percentna <- round(percentna, digits = 2)
View(percentna)
write.xlsx(percentna, "PorcentagemMissing.xlsx")